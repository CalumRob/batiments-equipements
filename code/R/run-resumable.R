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

#' Promote a fully-written temp file onto its final path (same volume).
#'
#' The rename half of every atomic write (#21): rename; if the platform
#' refuses an existing target, replace deliberately; copy as last resort.
#' The acquire.R promote pattern, factored out for the manifest, receipts and
#' matrix chunks alike.
promote_temp_file <- function(tmp, path) {
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
  promote_temp_file(tmp, path)
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
                                  route_seconds, validated_at = utc_now_iso(),
                                  n_routed_pairs = NULL,
                                  n_identity_pairs = NULL) {
  e <- manifest$entries[[entry_id]]
  if (is.null(e)) stop("no such manifest entry: ", entry_id, call. = FALSE)
  e$status <- "complete"
  e$path <- as.character(path)
  e$n_rows <- as.integer(n_rows)
  # Pair-count profile (#22 gate deliverable); receipts written before the
  # field existed carry neither — recorded as absent, never fabricated.
  e$n_routed_pairs <- if (is.null(n_routed_pairs)) NULL else as.integer(n_routed_pairs)
  e$n_identity_pairs <- if (is.null(n_identity_pairs)) NULL else
    as.integer(n_identity_pairs)
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

#' The checkout's HEAD commit — the code-provenance component of the resume
#' identity. Injectable in run_resumable for deterministic tests.
current_git_sha <- function() {
  out <- suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                                  stdout = TRUE, stderr = FALSE))
  sha <- trimws(out[[1L]])
  if (!grepl("^[0-9a-f]{40}$", sha)) {
    stop("could not determine the checkout git SHA (git rev-parse HEAD) — ",
         "code provenance is part of the run identity", call. = FALSE)
  }
  sha
}

#' Atomic parquet write: PID-tagged temp + promote (same discipline as
#' atomic_write_json — children read these files in a LATER process).
write_parquet_atomic <- function(x, path) {
  stopifnot(is.data.frame(x))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  arrow::write_parquet(x, tmp)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  promote_temp_file(tmp, path)
}

#' The residential routing identities in WGS84, exactly as run_tracer derives
#' them (scope read, optional explicit ids, EPSG:2154 -> 4326, NA guard).
#' The default origins_provider behind run_resumable.
bdnb_origin_coordinates <- function(scope = c("epci", "bretagne"),
                                    epci = "200072452",
                                    origin_ids = NULL,
                                    data_dir = "data",
                                    manifest_path = file.path(data_dir,
                                                              "manifest.json"),
                                    use_cache = TRUE) {
  scope <- match.arg(scope)
  if (identical(scope, "bretagne")) {
    bdnb <- read_bdnb_residential_universe(
      departements = c("22", "29", "35", "56"), data_dir = data_dir,
      manifest_path = manifest_path, use_cache = use_cache)
  } else {
    communes <- read_epci_communes(epci, data_dir = data_dir,
                                   manifest_path = manifest_path,
                                   use_cache = use_cache)
    bdnb <- read_bdnb_residential_universe(
      communes = communes[["code_insee"]], data_dir = data_dir,
      manifest_path = manifest_path, use_cache = use_cache)
  }
  selection <- select_origin_ids(bdnb, origin_ids)
  bdnb <- selection$origins
  sf_pts <- sf::st_as_sf(as.data.frame(bdnb),
                         coords = c("x_2154", "y_2154"), crs = 2154L,
                         remove = FALSE)
  sf_pts <- sf::st_transform(sf_pts, 4326L)
  xy <- as.data.frame(sf::st_coordinates(sf_pts))
  origins <- data.table::data.table(id = bdnb[["origin_id"]],
                                    lon = xy[["X"]], lat = xy[["Y"]])
  ok <- !is.na(origins[["lon"]]) & !is.na(origins[["lat"]])
  n_excluded <- sum(!ok)
  if (n_excluded > 0L) {
    warning(sprintf(
      "%d origin(s) with NA lon/lat excluded from routing", n_excluded),
      call. = FALSE)
    origins <- origins[ok]
  }
  list(origins = origins,
       n_requested = selection$n_requested,
       n_selected = selection$n_selected)
}

# --- The generated child bootstrap ------------------------------------------

#' The Rscript bootstrap of every chunk child (#21).
#'
#' Line-order IS the D6 contract: the heap is set from the request BEFORE any
#' source is loaded that could start rJava/r5r, and the process logs its heap
#' plus whether Java was already loaded at that instant — the smoke test
#' parses this line to prove heap-before-rJava ordering through the real
#' process boundary. Exit codes: 0 success, 2 usage error, 3 worker failure.
chunk_worker_bootstrap_script <- function() {
  c(
    "## Auto-generated by run_resumable (#21): one fresh-JVM child per chunk.",
    "## DO NOT EDIT — the orchestrator regenerates this file at each start.",
    "args <- commandArgs(trailingOnly = TRUE)",
    "if (length(args) != 1L) {",
    "  cat('usage: worker_bootstrap.R <request.json>\\n')",
    "  quit(save = 'no', status = 2L)",
    "}",
    "req <- jsonlite::fromJSON(args[[1L]], simplifyVector = TRUE)",
    "heap <- if (is.null(req$heap)) '-Xmx24G' else as.character(req$heap)",
    "## D6: heap BEFORE anything that touches rJava/r5r.",
    "options(java.parameters = heap)",
    "cat(sprintf('{\"bootstrap\": \"matrice-chunk-worker\", \"heap\": \"%s\", \"rjava_loaded\": %s}\\n',",
    "            heap, tolower(as.character('rJava' %in% loadedNamespaces()))))",
    "code_dir <- if (is.null(req$code_dir)) 'code' else as.character(req$code_dir)",
    "for (f in sort(list.files(file.path(code_dir, 'R'),",
    "                          pattern = '[.]R$', full.names = TRUE))) source(f)",
    "status <- tryCatch({",
    "  chunk_worker_main(args[[1L]])",
    "  0L",
    "}, error = function(e) {",
    "  cat(sprintf('chunk worker failed: %s\\n', conditionMessage(e)))",
    "  3L",
    "})",
    "quit(save = 'no', status = status)"
  )
}

write_worker_bootstrap <- function(run_dir) {
  path <- file.path(run_dir, "worker_bootstrap.R")
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(chunk_worker_bootstrap_script(), path)
  path
}

#' Resolve the directory whose R/ sources chunk children must load.
#'
#' The orchestrator may be invoked from a repo/worktree ROOT (sources under
#' \code{<cwd>/code}) or from the package directory itself (\code{<cwd>} IS
#' the package). The result is always ABSOLUTE with forward slashes so the
#' child process's own working directory can never matter — the #22 probe's
#' first launch failed exactly here: children silently sourced nothing when
#' the orchestrator ran above \code{code/}.
resolve_code_dir <- function(cwd = getwd()) {
  stopifnot(is.character(cwd), length(cwd) == 1L, !is.na(cwd), nzchar(cwd))
  cand <- if (dir.exists(file.path(cwd, "code"))) file.path(cwd, "code") else cwd
  out <- normalizePath(cand, winslash = "/", mustWork = TRUE)
  if (!dir.exists(file.path(out, "R"))) {
    stop("cannot find the package sources: ", out,
         " carries no R/ directory (run the orchestrator from the repo/worktree root or from the package directory)",
         call. = FALSE)
  }
  out
}

#' The CLI entry a child process runs (after ITS bootstrap set the heap):
#' read the request, dispatch to the real r5r router with a real network
#' loader. The migrated D6 guard lives here: no heap, no run.
chunk_worker_main <- function(request_path) {
  jp <- getOption("java.parameters")
  if (is.null(jp) || !any(grepl("-Xmx", jp, fixed = TRUE))) {
    stop(
      "chunk_worker_main: the -Xmx java heap must be set BEFORE loading ",
      "anything that touches r5r/rJava (D6 hard budget moved inside the ",
      "child bootstrap)", call. = FALSE
    )
  }
  req <- chunk_request_load(request_path)
  run_chunk_worker(req, router = NULL,
                   network_loader = default_network_loader(req))
  invisible(TRUE)
}

#' Spawn one real child process: Rscript running the generated bootstrap on
#' one request. This is the DEFAULT spawn_child — tests inject fakes instead.
spawn_chunk_child <- function(bootstrap_path, request_path,
                              r_bin = "Rscript") {
  stopifnot(file.exists(bootstrap_path), file.exists(request_path))
  out <- suppressWarnings(system2(
    r_bin, c(shQuote(normalizePath(bootstrap_path)),
             shQuote(normalizePath(request_path))),
    stdout = TRUE, stderr = TRUE))
  status <- if (is.null(attr(out, "status"))) 0L else as.integer(attr(out, "status"))
  text <- if (is.character(out)) paste(out, collapse = "\n") else ""
  list(status = status, stdout = text, stderr = text)
}

# --- The orchestrator --------------------------------------------------------

#' Build one child request from the frozen manifest facts.
build_chunk_request <- function(manifest, chunk_id, modes, run_dir,
                                network_dir, code_dir, heap) {
  rt <- manifest$identity$routing_parameters
  plan_dir <- file.path(run_dir, "plan")
  list(
    kind = "matrice-chunk-request",
    request_version = 1L,
    run_label = manifest$run_label,
    code_dir = code_dir,
    heap = heap,
    network_dir = network_dir,
    chunk_id = as.integer(chunk_id),
    chunk_size = as.integer(manifest$plan_census$chunk_size),
    n_origin_coords = as.integer(manifest$plan_census$n_origin_coords),
    modes = modes,
    paths = list(
      origin_points = file.path(plan_dir, "origin_points.parquet"),
      origin_link = file.path(plan_dir, "origin_link.parquet"),
      destination_points = file.path(plan_dir, "destination_points.parquet"),
      destination_link = file.path(plan_dir, "destination_link.parquet"),
      destination_map = file.path(plan_dir, "destination_map.parquet"),
      artifacts_dir = file.path(run_dir, "chunks"),
      receipts_dir = file.path(run_dir, "receipts")
    ),
    routing = list(
      walk_speed = rt$walk_speed,
      bike_speed = rt$bike_speed,
      max_trip_duration = rt$max_trip_duration,
      elevation = rt$elevation,
      dem_path = NULL,
      departure_datetime = rt$departure_datetime,
      # The epoch rides BESIDE the string: jsonlite::fromJSON auto-coerces
      # ISO strings to Date (time silently dropped -> midnight UTC), which
      # routed the #22 probe's transit at 02:00 Paris and collapsed every
      # chunk to pure walk. Numbers survive the JSON boundary verbatim; the
      # reader prefers this field and keeps the string for humans.
      departure_epoch = if (is.null(rt$departure_datetime)) NULL
        else as.numeric(as.POSIXct(rt$departure_datetime,
                                   format = "%Y-%m-%dT%H:%M:%S%z",
                                   tz = "UTC")),
      time_window = rt$time_window,
      percentiles = rt$percentiles,
      n_threads = NULL
    )
  )
}

#' Startup integrity sweep (#21 box: rerunning resumes missing, invalid or
#' incomplete entries without rewriting valid ones).
#'
#' Two passes over the entries, both trusting NOTHING:
#'   * COMPLETE entries are trusted only while their recorded sha256 still
#'     matches the bytes on disk — an orphan-child overwrite or any corruption
#'     cause demotes the entry back to pending;
#'   * NON-COMPLETE entries whose artifact AND receipt survived a crashed
#'     child are independently re-validated (schema + row count + checksum
#'     against the receipt) and promoted — a child dying mid-chunk keeps its
#'     finished modes committed instead of re-routing them.
sweep_run_entries <- function(manifest, run_dir, durable_root) {
  receipts_dir <- file.path(run_dir, "receipts")
  for (id in names(manifest$entries)) {
    e <- manifest$entries[[id]]
    abs <- if (is.null(e$path)) NULL else file.path(durable_root, e$path)

    if (identical(e$status, "complete")) {
      drifted <- is.null(abs) || !file.exists(abs) ||
        !identical(suppressWarnings(sha256_file(abs)),
                   as.character(e$sha256))
      if (drifted) {
        e$status <- "pending"
        e$error <- paste(
          "demoted at startup: recorded sha256 no longer matches the",
          "artifact on disk (orphan-child overwrite or corruption)")
        message(sprintf("manifest sweep: %s demoted (%s)", id, e$error))
        manifest$entries[[id]] <- e
      }
      next
    }

    # Non-complete: try to salvage surviving work from the crashed child.
    if (is.null(abs) || !file.exists(abs)) next
    rp <- chunk_receipt_path(receipts_dir, e$mode, e$chunk_id)
    if (!file.exists(rp)) next
    salvaged <- tryCatch({
      rec <- read_chunk_receipt(rp)
      validate_chunk_artifact(abs, rec)
      TRUE
    }, error = function(err) FALSE)
    if (salvaged) {
      rec <- read_chunk_receipt(rp)
      manifest <- complete_chunk_entry(
        manifest, id, path = e$path,
        n_rows = rec$n_rows, sha256 = rec$sha256,
        route_seconds = rec$route_seconds,
        validated_at = utc_now_iso(),
        n_routed_pairs = rec$n_routed_pairs,
        n_identity_pairs = rec$n_identity_pairs)
      message(sprintf("manifest sweep: %s salvaged from the previous child",
                      id))
    }
  }
  manifest
}

#' The owed (mode x chunk) entries of one chunk: everything not complete.
#' `running` is a CLAIM, not a fact — stale claims are owed like the rest.
owed_modes_of_chunk <- function(manifest, chunk_id) {
  owed <- character(0)
  for (mode in unique(vapply(manifest$entries, `[[`, character(1L), "mode"))) {
    e <- manifest$entries[[chunk_entry_id(mode, chunk_id)]]
    if (!is.null(e) && !identical(e$status, "complete")) {
      owed <- c(owed, mode)
    }
  }
  owed
}

as_canonical_datetime_string <- function(departure_datetime) {
  if (is.null(departure_datetime)) return(NULL)
  format(departure_datetime, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}

#' The JVM-free ORCHESTRATOR of the resumable once-run (#21).
#'
#' Owns data/matrice/<run_label>/manifest.json (single writer), spawns ONE
#' child per chunk STRICTLY sequentially (one live JVM at a time, forever —
#' never two concurrent 24 GiB heaps on the 32 GiB machine; operator rule:
#' ONE RUN AT A TIME, there is deliberately NO lock subsystem), and marks a
#' (mode x chunk) entry complete ONLY after the child validated its artifact
#' AND the orchestrator independently re-validates it and cross-checks its
#' sha256 against the child's receipt.
#'
#' @param run_label Identifies the run directory (data/matrice/<label>/).
#' @param modes Atomic modes to route; every mode x chunk gets an entry.
#' @param chunk_size Max unique origin coordinates per chunk — uniform across
#'   modes by decision (2026-08-24); frozen into the census at first
#'   orchestration.
#' @param network_identity A network_cache_identity() result (fingerprint +
#'   components); rides VERBATIM into the resume identity.
#' @param network_dir Passed to children (where network.dat lives).
#' @param origins_provider,destinations_provider Injection seams (permanent
#'   design, like spawn_child/router): NULL uses the real readers
#'   (bdnb_origin_coordinates / prepare_bpe_destinations). Providers make the
#'   whole orchestrator fixture-scale testable and decouple it from data/
#'   presence.
#' @param git_sha Code provenance (current_git_sha()); refusal-grade unless
#'   \code{allow_code_drift}.
#' @param allow_code_drift Continue past a git-SHA change, recording the
#'   deliberate continuation in the manifest. Overrides NOTHING else.
#' @param spawn_child function(bootstrap_path, request_path) -> list(status,
#'   stdout, stderr). Default spawn_chunk_child really runs Rscript.
#'
#' @return Invisibly, a summary: manifest_path, complete (derived),
#'   counts per status, spawned chunk ids, code_drift_allowed.
#' @export
run_resumable <- function(run_label,
                          modes = atomic_modes(),
                          chunk_size = 50000L,
                          W = border_width_m(),
                          walk_speed = 4,
                          bike_speed = 12,
                          max_trip_duration = cap_minutes(),
                          elevation = "NONE",
                          departure_datetime = NULL,
                          time_window = 60L,
                          percentiles = c(1L, 50L),
                          network_identity,
                          network_dir,
                          heap = "-Xmx24G",
                          data_dir = "data",
                          manifest_path = file.path(data_dir, "manifest.json"),
                          use_cache = TRUE,
                          scope = c("epci", "bretagne"),
                          epci = "200072452",
                          origin_ids = NULL,
                          out_dir = file.path("data", "matrice"),
                          origins_provider = NULL,
                          destinations_provider = NULL,
                          git_sha = current_git_sha(),
                          allow_code_drift = FALSE,
                          spawn_child = spawn_chunk_child,
                          code_dir = NULL,
                          verbose = TRUE) {
  # --- load-bearing arguments ----------------------------------------------
  probe_run_modes(modes, departure_datetime)
  assert_within_cap(max_trip_duration)
  code_dir <- resolve_code_dir(if (is.null(code_dir)) getwd() else code_dir)
  stopifnot(is.character(run_label), length(run_label) == 1L,
            !is.na(run_label), nzchar(run_label))
  stopifnot(is.list(network_identity),
            !is.null(network_identity$fingerprint))
  stopifnot(is.character(network_dir), length(network_dir) == 1L,
            !is.na(network_dir), nzchar(network_dir))
  stopifnot(is.numeric(chunk_size), length(chunk_size) == 1L,
            !is.na(chunk_size), chunk_size >= 1L)
  stopifnot(is.function(spawn_child))
  say <- function(...) if (isTRUE(verbose)) message(...)

  run_dir <- matrice_run_dir(out_dir, run_label)
  for (d in c("plan", "chunks", "receipts", "requests")) {
    dir.create(file.path(run_dir, d), recursive = TRUE, showWarnings = FALSE)
  }
  durable_root <- durable_root_of_data_dir(data_dir)

  # --- universes, plans, census (rebuilt every invocation) ------------------
  dest_prep <- if (is.null(destinations_provider)) {
    prepare_bpe_destinations(W = W, data_dir = data_dir,
                             manifest_path = manifest_path,
                             use_cache = use_cache)
  } else destinations_provider()
  registry_path <- write_destination_registry(
    dest_prep$registry, file.path(run_dir, "destination_registry.parquet"))
  registry_sha <- sha256_file(registry_path)

  origins <- if (is.null(origins_provider)) {
    bdnb_origin_coordinates(scope = scope, epci = epci, origin_ids = origin_ids,
                            data_dir = data_dir, manifest_path = manifest_path,
                            use_cache = use_cache)$origins
  } else origins_provider()

  op <- coordinate_routing_plan(origins, prefix = "coord_o")
  dp <- coordinate_routing_plan(dest_prep$destinations, prefix = "coord_d")
  census <- plan_census(chunk_size = chunk_size,
                        n_origins = nrow(origins),
                        n_origin_coords = nrow(op$points),
                        n_destinations = nrow(dest_prep$destinations),
                        n_dest_coords = nrow(dp$points))

  identity <- list(
    version = 1L,
    network_fingerprint = network_identity$fingerprint,
    routing_parameters = list(
      walk_speed = as.numeric(walk_speed),
      bike_speed = as.numeric(bike_speed),
      max_trip_duration = as.numeric(max_trip_duration),
      elevation = as.character(elevation),
      departure_datetime = as_canonical_datetime_string(departure_datetime),
      time_window = as.integer(time_window),
      percentiles = as.integer(percentiles)
    ),
    universes = list(
      bdnb_residential = bdnb_universe_key(origins),
      bpe_destinations = registry_sha
    ),
    plan_census = census[c("chunk_size", "n_chunks", "n_origins",
                           "n_origin_coords", "n_destinations", "n_dest_coords")],
    git_sha = as.character(git_sha)
  )

  # --- create or resume the manifest ----------------------------------------
  mpath <- file.path(run_dir, "manifest.json")
  code_drift_allowed <- FALSE
  if (file.exists(mpath)) {
    manifest <- load_run_manifest(mpath)
    hit <- first_resume_mismatch(manifest$identity, identity,
                                 allow_code_drift = FALSE)
    if (!is.null(hit) && identical(hit$component, "git_sha") &&
        isTRUE(allow_code_drift)) {
      code_drift_allowed <- TRUE
      manifest$code_drift <- list(
        allowed = TRUE,
        note = sprintf(
          "deliberate continuation past a code change: %s -> %s at %s",
          manifest$identity$git_sha, identity$git_sha, utc_now_iso()))
      manifest$identity$git_sha <- identity$git_sha
    } else if (!is.null(hit)) {
      stop(sprintf("resume refused (%s): %s", hit$component, hit$reason),
           call. = FALSE)
    }
    # Mode set is structural: the entries were frozen at creation.
    frozen_modes <- sort(unique(vapply(manifest$entries, `[[`,
                                       character(1L), "mode")))
    if (!identical(frozen_modes, sort(unique(modes)))) {
      stop(sprintf(
        "manifest carries modes [%s] but [%s] were requested — cut the new mode set behind a fresh run label",
        paste(frozen_modes, collapse = ", "),
        paste(sort(unique(modes)), collapse = ", ")), call. = FALSE)
    }
    say(sprintf("run_resumable: resuming '%s' (%d/%d entries complete)",
                run_label,
                sum(vapply(manifest$entries, function(e)
                  identical(e$status, "complete"), logical(1L))),
                length(manifest$entries)))
  } else {
    manifest <- new_run_manifest(run_label, identity, census, modes,
                                 census$n_chunks)
    say(sprintf("run_resumable: new run '%s': %d chunks x %d modes = %d entries",
                run_label, census$n_chunks, length(modes),
                length(manifest$entries)))
  }

  # Persist the plan (children consume these files; identical cuts everywhere).
  write_parquet_atomic(op$points, file.path(run_dir, "plan",
                                            "origin_points.parquet"))
  write_parquet_atomic(op$link, file.path(run_dir, "plan",
                                          "origin_link.parquet"))
  write_parquet_atomic(dp$points, file.path(run_dir, "plan",
                                            "destination_points.parquet"))
  write_parquet_atomic(dp$link, file.path(run_dir, "plan",
                                          "destination_link.parquet"))
  write_parquet_atomic(dest_prep$dest_map, file.path(run_dir, "plan",
                                                     "destination_map.parquet"))

  bootstrap_path <- write_worker_bootstrap(run_dir)

  # --- startup sweep: trust nothing, salvage what validates ------------------
  manifest <- sweep_run_entries(manifest, run_dir, durable_root)
  save_run_manifest(manifest, mpath)

  # --- sequential work loop ---------------------------------------------------
  spawned <- character(0)
  for (i in seq_len(census$n_chunks)) {
    owed <- owed_modes_of_chunk(manifest, i)
    if (length(owed) == 0L) next

    req <- build_chunk_request(manifest, i, owed, run_dir,
                               network_dir = network_dir,
                               code_dir = code_dir, heap = heap)
    req_path <- file.path(run_dir, "requests", sprintf("chunk_%d.json", i))
    chunk_request_save(req, req_path)

    for (mode in owed) {
      manifest <- claim_chunk_entry(manifest, chunk_entry_id(mode, i))
    }
    save_run_manifest(manifest, mpath)   # the CLAIM is durable before spawning

    say(sprintf("run_resumable: chunk %d/%d -> child (%s)",
                i, census$n_chunks, paste(owed, collapse = " + ")))
    res <- spawn_child(bootstrap_path, req_path)
    spawned <- c(spawned, as.character(i))

    if (!identical(as.integer(res$status), 0L)) {
      stderr_text <- if (is.null(res$stderr)) "" else as.character(res$stderr)
      tail_lines <- utils::tail(strsplit(stderr_text, "\n",
                                         fixed = TRUE)[[1L]], 3L)
      reason <- sprintf("child exited %d: %s", as.integer(res$status),
                        paste(tail_lines, collapse = " | "))
      for (mode in owed) {
        manifest <- fail_chunk_entry(manifest, chunk_entry_id(mode, i), reason)
      }
      say(sprintf("run_resumable: chunk %d FAILED (%s)", i, reason))
    } else {
      for (mode in owed) {
        id <- chunk_entry_id(mode, i)
        outcome <- tryCatch({
          rp <- chunk_receipt_path(file.path(run_dir, "receipts"), mode, i)
          rec <- read_chunk_receipt(rp)
          rel <- file.path(req$paths$artifacts_dir,
                           sprintf("%s_%d.parquet", mode, i))
          abs <- resolve_under_durable_root(rel, durable_root)
          validate_chunk_artifact(abs, rec)
          manifest <- complete_chunk_entry(
            manifest, id,
            path = portable_path(abs, durable_root),
            n_rows = rec$n_rows, sha256 = rec$sha256,
            route_seconds = rec$route_seconds,
            n_routed_pairs = rec$n_routed_pairs,
            n_identity_pairs = rec$n_identity_pairs)
          NULL
        }, error = function(err) conditionMessage(err))
        if (!is.null(outcome)) {
          manifest <- fail_chunk_entry(manifest, id, outcome)
          say(sprintf("run_resumable: %s failed validation: %s", id, outcome))
        }
      }
    }
    save_run_manifest(manifest, mpath)   # single writer, after every chunk
  }

  statuses <- vapply(manifest$entries, `[[`, character(1L), "status")
  complete <- manifest_all_complete(manifest)
  if (complete) {
    assemble_run_metadata(manifest, run_dir, durable_root)
    say(sprintf("run_resumable: '%s' complete — run_metadata.json assembled",
                run_label))
  }

  invisible(list(
    manifest_path = mpath,
    run_dir = run_dir,
    complete = complete,
    n_complete = sum(statuses == "complete"),
    n_failed = sum(statuses == "failed"),
    n_pending = sum(statuses == "pending" | statuses == "running"),
    spawned_chunks = spawned,
    code_drift_allowed = code_drift_allowed
  ))
}

#' Resolve an artifact path that may be recorded relative to the durable
#' root or already absolute/cwd-relative (the resolve_cached_path idiom).
resolve_under_durable_root <- function(path, durable_root) {
  if (file.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  cand <- file.path(durable_root, path)
  if (file.exists(cand)) {
    return(normalizePath(cand, winslash = "/", mustWork = FALSE))
  }
  stop("artifact missing (tried cwd-relative and under the durable root): ",
       path, call. = FALSE)
}

#' Final run_metadata.json — assembled BY THE ORCHESTRATOR when every entry
#' is complete (derived completeness, never a stored flag). Portable paths
#' only (#19), written atomically.
assemble_run_metadata <- function(manifest, run_dir, durable_root) {
  entries <- manifest$entries
  modes_frozen <- unique(vapply(entries, `[[`, character(1L), "mode"))
  per_mode <- stats::setNames(lapply(modes_frozen, function(md) {
    es <- Filter(function(e) identical(e$mode, md), entries)
    list(
      n_chunks = length(es),
      n_rows = sum(unlist(lapply(es, function(e)
        if (is.null(e$n_rows)) 0L else as.integer(e$n_rows)))),
      # Pair-count profile (#22 gate deliverable): routed coordinate pairs and
      # what expansion restored them to, summed over the mode's chunks.
      n_routed_pairs = sum(unlist(lapply(es, function(e)
        if (is.null(e$n_routed_pairs)) 0L else as.integer(e$n_routed_pairs)))),
      n_identity_pairs = sum(unlist(lapply(es, function(e)
        if (is.null(e$n_identity_pairs)) 0L else as.integer(e$n_identity_pairs)))),
      route_seconds = sum(unlist(lapply(es, function(e)
        if (is.null(e$route_seconds)) 0 else as.numeric(e$route_seconds))))
    )
  }), modes_frozen)
  meta <- list(
    kind = "matrice-run-metadata",
    version = 1L,
    run_label = manifest$run_label,
    created_at = manifest$created_at,
    completed_at = utc_now_iso(),
    identity = manifest$identity,
    code_drift = manifest$code_drift,
    cap_minutes = cap_minutes(),
    ladder_rungs = unname(ladder_rungs()),
    ladder_cols = unname(ladder_cols()),
    per_mode = per_mode,
    manifest_path = portable_path(file.path(run_dir, "manifest.json"),
                                  durable_root)
  )
  meta <- make_metadata_portable(meta, durable_root)
  assert_no_absolute_paths(meta)
  atomic_write_json(meta, file.path(run_dir, "run_metadata.json"))
}
