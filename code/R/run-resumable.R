# S17 — the resumable per-chunk runner (#21): the JVM-free ORCHESTRATOR.
#
# Two process types, strict separation (ticket #21):
#   * run_resumable() — this file. Never touches r5r/rJava. Owns the durable
#     run manifest (data/matrice/<run_label>/manifest.json), spawns fresh
#     children strictly sequentially (one live JVM at a time, forever), and
#     alone decides which entries are complete: a child's receipt is advisory,
#     the sha256 cross-check plus an independent re-validation are the trust
#     boundary.
#   * run_chunk_worker() — run-chunk-worker.R. One Rscript invocation per
#     chunk; routes exactly one chunk across ALL its modes and reports one
#     receipt per (mode x chunk). Children NEVER touch the manifest.
#
# Vocabulary (CONTEXT.md Core, 2026-08-24): the resume unit is the CHUNK;
# "batch" is retired and no grouping level above a chunk exists.
#
# Status lifecycle: pending -> running -> complete | failed. `running` is a
# CLAIM, not a fact — a crash leaves stale claims behind and resume treats any
# non-complete entry as work to do. There is no recovery protocol; absence of
# completion IS the instruction.
#
# Concurrency: NO lock subsystem (maintainer decision 2026-08-24) — the runner
# is sequential by construction and the operator rule is ONE RUN AT A TIME:
# never launch two orchestrations against the same run label.
#
# Atomicity: every durable write here goes through atomic_write_json() —
# PID-tagged temp name + same-volume rename (the acquire.R promote pattern).
#
# Reading discipline: this file's data.tables are extracted with `[[` or `j`,
# NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' The manifest sentinel conventions: kind + version, like every durable JSON
#' this project writes (acquisition manifest, durable-root sentinel,
#' network-identity marker).
run_manifest_kind <- function() "matrice-run-manifest"

#' UTC ISO-8601 timestamp (house convention for every recorded instant).
utc_now_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' The entry id of one (mode x chunk): "<mode>_<chunk_id>" — byte-identical to
#' the artifact stem (write_matrix_chunk) and the receipt stem.
chunk_entry_id <- function(mode, chunk_id) {
  sprintf("%s_%d", mode, as.integer(chunk_id))
}

#' The sanitized per-run directory under the matrice root (same rule as
#' run_tracer's run_out_dir).
matrice_run_dir <- function(out_dir = file.path("data", "matrice"),
                            run_label = "run") {
  stopifnot(is.character(out_dir), length(out_dir) == 1L, !is.na(out_dir),
            nzchar(out_dir))
  stopifnot(is.character(run_label), length(run_label) == 1L,
            !is.na(run_label), nzchar(run_label))
  file.path(out_dir, gsub("[^A-Za-z0-9_.-]+", "-", run_label))
}

#' The durable manifest path of a run.
run_manifest_path <- function(out_dir = file.path("data", "matrice"),
                              run_label = "run") {
  file.path(matrice_run_dir(out_dir, run_label), "manifest.json")
}

#' Atomic JSON write: PID-tagged temp sibling + rename onto `path`.
#'
#' The single-writer discipline's enforcement point (#21): readers of the
#' manifest can only ever observe a fully-written file, never a torn one. The
#' temp file lives in the target directory (same volume, rename stays atomic)
#' and carries the writer's PID so two stray writers cannot collide silently.
atomic_write_json <- function(x, path) {
  stopifnot(is.character(path), length(path) == 1L, !is.na(path),
            nzchar(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  # Promote (acquire.R pattern): rename; if the platform refuses an existing
  # target, replace deliberately, then fall back to copy.
  if (!isTRUE(file.rename(tmp, path))) {
    unlink(path)
    if (!isTRUE(file.rename(tmp, path))) {
      if (!isTRUE(file.copy(tmp, path, overwrite = TRUE))) {
        stop("could not promote ", tmp, " onto ", path, call. = FALSE)
      }
    }
  }
  invisible(path)
}

#' The frozen plan census (#21): chunk geometry over unique routing
#' coordinates, frozen into the manifest at first orchestration; any drift
#' refuses resume (first_resume_mismatch).
plan_census <- function(chunk_size, n_origins, n_origin_coords,
                        n_destinations, n_dest_coords) {
  stopifnot(is.numeric(chunk_size), length(chunk_size) == 1L,
            !is.na(chunk_size), chunk_size >= 1L)
  for (v in list(n_origins, n_origin_coords, n_destinations, n_dest_coords)) {
    stopifnot(is.numeric(v), length(v) == 1L, !is.na(v), v >= 0)
  }
  if (n_origins < n_origin_coords) {
    stop("census impossible: more unique origin coordinates than origin identities",
         call. = FALSE)
  }
  list(
    chunk_size = as.integer(chunk_size),
    n_chunks = as.integer(ceiling(n_origin_coords / chunk_size)),
    n_origins = as.integer(n_origins),
    n_origin_coords = as.integer(n_origin_coords),
    n_destinations = as.integer(n_destinations),
    n_dest_coords = as.integer(n_dest_coords),
    expected_full_run = full_run_coordinate_counts()
  )
}

#' A new run manifest: identity block + frozen census + every entry pending.
new_run_manifest <- function(run_label, identity, plan_census_block,
                             modes, n_chunks) {
  stopifnot(is.list(identity))
  stopifnot(is.list(plan_census_block))
  modes <- as.character(modes)
  stopifnot(length(modes) >= 1L, all(modes %in% atomic_modes()))
  entries <- list()
  for (mode in modes) {
    for (i in seq_len(n_chunks)) {
      entries[[chunk_entry_id(mode, i)]] <- list(
        mode = mode,
        chunk_id = as.integer(i),
        status = "pending",
        attempts = 0L,
        path = NULL,
        n_rows = NULL,
        sha256 = NULL,
        route_seconds = NULL,
        validated_at = NULL,
        error = NULL
      )
    }
  }
  list(
    kind = run_manifest_kind(),
    manifest_version = 1L,
    run_label = as.character(run_label),
    created_at = utc_now_iso(),
    updated_at = utc_now_iso(),
    identity = identity,
    plan_census = plan_census_block,
    entries = entries
  )
}

#' Save the manifest atomically (the orchestrator is its ONLY writer).
save_run_manifest <- function(manifest, path) {
  stopifnot(identical(manifest[["kind"]], run_manifest_kind()),
            identical(as.integer(manifest[["manifest_version"]]), 1L))
  manifest$updated_at <- utc_now_iso()
  atomic_write_json(manifest, path)
}

#' Load the manifest; a missing or alien file is a hard error — resume without
#' the frozen identity would be guessing.
load_run_manifest <- function(path) {
  if (!file.exists(path)) {
    stop("run manifest not found: ", path,
         " (first orchestration must create it)", call. = FALSE)
  }
  m <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(m) || !identical(m$kind, run_manifest_kind())) {
    stop("not a ", run_manifest_kind(), ": ", path, call. = FALSE)
  }
  m
}

#' Claim an entry for a child spawn: running is a CLAIM, not a fact. Each
#' claim bumps attempts so the manifest records how often a chunk was tried.
claim_chunk_entry <- function(manifest, entry_id) {
  e <- manifest$entries[[entry_id]]
  if (is.null(e)) stop("no such manifest entry: ", entry_id, call. = FALSE)
  e$status <- "running"
  e$attempts <- as.integer(e$attempts) + 1L
  e$error <- NULL
  manifest$entries[[entry_id]] <- e
  manifest
}

complete_chunk_entry <- function(manifest, entry_id, path, n_rows, sha256,
                                 route_seconds, validated_at = utc_now_iso()) {
  e <- manifest$entries[[entry_id]]
  if (is.null(e)) stop("no such manifest entry: ", entry_id, call. = FALSE)
  e$status <- "complete"
  e$path <- as.character(path)
  e$n_rows <- as.integer(n_rows)
  e$sha256 <- as.character(sha256)
  e$route_seconds <- as.numeric(route_seconds)
  e$validated_at <- validated_at
  e$error <- NULL
  manifest$entries[[entry_id]] <- e
  manifest
}

fail_chunk_entry <- function(manifest, entry_id, reason) {
  e <- manifest$entries[[entry_id]]
  if (is.null(e)) stop("no such manifest entry: ", entry_id, call. = FALSE)
  e$status <- "failed"
  e$error <- paste(as.character(reason), collapse = "; ")
  manifest$entries[[entry_id]] <- e
  manifest
}

#' Is the run complete? DERIVED from the entries — there is no aggregate flag.
manifest_all_complete <- function(manifest) {
  statuses <- vapply(manifest$entries, `[[`, character(1L), "status")
  length(statuses) > 0L && all(statuses == "complete")
}

# --- Resume-compatibility identity (#21) ------------------------------------
#
# COMPOSES existing seams rather than inventing a second system: the
# network_cache_identity fingerprint rides VERBATIM, plus refusal-grade
# routing parameters (D5: a speed change is a re-run), the universe content
# keys, the frozen plan census and the checkout git SHA. Comparison mirrors
# probe_network_cache's miss-reason pattern: the FIRST mismatched component
# is named with both sides spelled out.

#' Canonical scalar/vector form for identity comparison. JSON round-trips
#' blur integer/double boundaries, so numerics compare through fixed-precision
#' text; NULL is its own distinct value (a NULL -> datetime change is drift).
canonical_identity_value <- function(v) {
  if (is.null(v)) return("__null__")
  if (is.numeric(v)) return(format(as.numeric(v), digits = 15,
                                   scientific = FALSE, trim = TRUE))
  if (is.logical(v)) return(as.character(v))
  as.character(v)
}

identity_mismatch <- function(component, expected, found) {
  list(
    component = component,
    reason = sprintf(
      "%s mismatch — expected %s but the manifest records %s",
      component, expected, found
    )
  )
}

#' The FIRST resume-incompatible component, or NULL when compatible.
#'
#' Fixed comparison order (first mismatch wins the report): version ->
#' network fingerprint -> routing parameters (walk_speed, bike_speed,
#' max_trip_duration, elevation setting, canonical departure_datetime string,
#' time_window, percentiles) -> universe keys -> plan census -> git SHA.
#' \code{allow_code_drift = TRUE} overrides ONLY the git-SHA mismatch and is
#' recorded by the orchestrator as deliberate continuation past a code change.
#'
#' @return Invisible-style: NULL when compatible; otherwise a list(component,
#'   reason) whose reason spells out both values (probe_network_cache pattern).
first_resume_mismatch <- function(expected_identity, found_identity,
                                  allow_code_drift = FALSE) {
  stopifnot(is.list(expected_identity), is.list(found_identity))
  stopifnot(is.logical(allow_code_drift), length(allow_code_drift) == 1L,
            !is.na(allow_code_drift))

  if (!identical(canonical_identity_value(expected_identity$version),
                 canonical_identity_value(found_identity$version))) {
    return(identity_mismatch("version",
                             canonical_identity_value(expected_identity$version),
                             canonical_identity_value(found_identity$version)))
  }

  exp_fp <- expected_identity$network_fingerprint
  found_fp <- found_identity$network_fingerprint
  if (!identical(as.character(exp_fp), as.character(found_fp))) {
    hit <- identity_mismatch("network_fingerprint", exp_fp, found_fp)
    hit$reason <- sprintf(
      paste0("network cache fingerprint mismatch — manifest %s vs requested ",
             "%s (an input changed)"),
      found_fp, exp_fp)
    return(hit)
  }

  rp_fields <- c("walk_speed", "bike_speed", "max_trip_duration", "elevation",
                 "departure_datetime", "time_window", "percentiles")
  for (f in rp_fields) {
    ev <- canonical_identity_value(expected_identity$routing_parameters[[f]])
    fv <- canonical_identity_value(found_identity$routing_parameters[[f]])
    if (!identical(ev, fv)) {
      return(identity_mismatch(paste0("routing_parameters.", f), ev, fv))
    }
  }

  u_fields <- c("bdnb_residential", "bpe_destinations")
  for (f in u_fields) {
    ev <- canonical_identity_value(expected_identity$universes[[f]])
    fv <- canonical_identity_value(found_identity$universes[[f]])
    if (!identical(ev, fv)) {
      return(identity_mismatch(paste0("universes.", f), ev, fv))
    }
  }

  pc_fields <- c("chunk_size", "n_chunks", "n_origins", "n_origin_coords",
                 "n_destinations", "n_dest_coords")
  for (f in pc_fields) {
    ev <- canonical_identity_value(expected_identity$plan_census[[f]])
    fv <- canonical_identity_value(found_identity$plan_census[[f]])
    if (!identical(ev, fv)) {
      return(identity_mismatch(paste0("plan_census.", f), ev, fv))
    }
  }

  exp_sha <- canonical_identity_value(expected_identity$git_sha)
  found_sha <- canonical_identity_value(found_identity$git_sha)
  if (!identical(exp_sha, found_sha)) {
    if (!isTRUE(allow_code_drift)) {
      hit <- identity_mismatch("git_sha", exp_sha, found_sha)
      hit$reason <- sprintf(paste0(
        "code drift: checkout moved from %s to %s since first orchestration ",
        "(resume would mix outputs across code versions) — pass ",
        "allow_code_drift = TRUE to document deliberate continuation"),
        found_sha, exp_sha)
      return(hit)
    }
  }

  NULL
}

#' Content key of the residential universe: sha256 over canonical
#' "id|lon|lat" lines, radix-sorted so staging order never exists as a
#' concept (the cache-identity idiom applied to origin identities).
#'
#' The coordinates are the WGS84 routing identities — exactly what the plan
#' is built over — so any change that could alter the plan changes the key.
bdnb_universe_key <- function(origins) {
  stopifnot(is.data.frame(origins),
            all(c("id", "lon", "lat") %in% names(origins)))
  o <- data.table::as.data.table(origins)
  lines <- sprintf("%s|%.10f|%.10f", o[["id"]], o[["lon"]], o[["lat"]])
  digest::digest(paste(sort(lines, method = "radix"), collapse = "\n"),
                 algo = "sha256", serialize = FALSE)
}
