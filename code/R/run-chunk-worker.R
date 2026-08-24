# S18 — the chunk CHILD of the resumable once-run (#21): run_chunk_worker().
#
# One worker invocation = ONE chunk routed across ALL its modes, reporting one
# receipt per (mode x chunk). In production it runs as a fresh Rscript process
# whose bootstrap (run-resumable.R's chunk_worker_bootstrap_script) sets the
# D6 heap BEFORE any rJava/r5r load — the caller contract of run_tracer moved
# inside the child. In tests the SAME function runs headless: inject a router
# (the stub_route_pairs precedent from #20) and no network is ever touched.
#
# The pipeline per mode — plan slice -> route -> expand -> derive ->
# temp-write -> rename -> validate -> sha256 -> receipt:
#   * the origin slice comes from the persisted plan parquets by the same
#     arithmetic the orchestrator froze into the census (chunk_point_slice),
#     so every process cuts identical chunks;
#   * pairs expand back to BDNB/BPE identities BEFORE derivation (#20);
#   * artifacts land at the deterministic final path via a PID-tagged temp +
#     same-volume rename — a killed child can never leave a half-written
#     parquet at a path a later resume would trust;
#   * the child validates its own artifact (read_matrix + validate_matrix)
#     BEFORE writing the receipt; the ORCHESTRATOR still re-validates
#     independently and cross-checks sha256 — receipts are advisory, the
#     checksum is the trust boundary;
#   * children NEVER touch the manifest (single writer = orchestrator).
#
# Reading discipline: this file's data.tables are extracted with `[[` or `j`,
# NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' The receipt path of one (mode x chunk): "<receipts_dir>/<mode>_<chunk>.json".
chunk_receipt_path <- function(receipts_dir, mode, chunk_id) {
  file.path(receipts_dir, paste0(chunk_entry_id(mode, chunk_id), ".json"))
}

#' Read one child receipt (advisory provenance; never a trust decision).
read_chunk_receipt <- function(path) {
  if (!file.exists(path)) {
    stop("chunk receipt not found: ", path, call. = FALSE)
  }
  r <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(r)) stop("chunk receipt unreadable: ", path, call. = FALSE)
  r
}

#' Validate an artifact against a receipt: checksum cross-check FIRST (the
#' trust boundary — a corrupt file never even reaches the parser), then the
#' schema contract and row count. Used TWICE by design (#21): the child
#' validates before writing its receipt, and the orchestrator re-validates
#' independently before trusting anything.
validate_chunk_artifact <- function(path, receipt) {
  if (!file.exists(path)) {
    stop(sprintf("artifact missing for %s chunk %d: %s",
                 receipt$mode, as.integer(receipt$chunk_id), path),
         call. = FALSE)
  }
  observed <- sha256_file(path)
  if (!identical(observed, as.character(receipt$sha256))) {
    stop(sprintf(
      "sha256 mismatch for %s — artifact hashes %s but the receipt claims %s (orphan overwrite or corrupted bytes)",
      path, observed, as.character(receipt$sha256)
    ), call. = FALSE)
  }
  m <- read_matrix(path)
  validate_matrix(m)
  if (!identical(nrow(m), as.integer(receipt$n_rows))) {
    stop(sprintf("row-count mismatch for %s: artifact has %d rows, receipt claims %d",
                 path, nrow(m), as.integer(receipt$n_rows)), call. = FALSE)
  }
  invisible(TRUE)
}

#' The chunk slice arithmetic shared by orchestrator and child: chunk i takes
#' plan points ((i-1)*chunk_size, i*chunk_size] — identical in every process
#' because the plan order itself is deterministic (setorder'd universes,
#' coordinate_routing_plan).
chunk_point_slice <- function(points, chunk_id, chunk_size) {
  stopifnot(is.data.frame(points))
  stopifnot(length(chunk_id) == 1L, !is.na(chunk_id), chunk_id >= 1L)
  stopifnot(length(chunk_size) == 1L, !is.na(chunk_size), chunk_size >= 1L)
  n <- nrow(points)
  start <- (as.integer(chunk_id) - 1L) * as.integer(chunk_size) + 1L
  if (start > n) {
    stop(sprintf(
      "chunk %d out of range: %d unique coordinates cut into chunks of %d (%d chunks)",
      as.integer(chunk_id), n, as.integer(chunk_size),
      ceiling(n / chunk_size)
    ), call. = FALSE)
  }
  idx <- seq.int(start, min(as.integer(chunk_id) * as.integer(chunk_size), n))
  points[idx, , drop = FALSE]
}

#' Normalize a request read back from JSON: defaults made explicit,
#' NULL n_threads meaning r5r's default (Inf), departure string -> POSIXct.
as_chunk_request <- function(req) {
  stopifnot(is.list(req))
  if (!identical(req$kind, "matrice-chunk-request")) {
    stop("not a matrice-chunk-request", call. = FALSE)
  }
  missing_fields <- setdiff(
    c("chunk_id", "chunk_size", "n_origin_coords", "modes", "paths"),
    names(req))
  if (length(missing_fields)) {
    stop("chunk request missing fields: ",
         paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  bad_modes <- setdiff(req$modes, atomic_modes())
  if (length(bad_modes)) {
    stop("unknown mode(s) in chunk request: ",
         paste(bad_modes, collapse = ", "), call. = FALSE)
  }
  req$routing <- req$routing %||% list()
  rt <- req$routing
  rt$walk_speed <- if (is.null(rt$walk_speed)) 4 else as.numeric(rt$walk_speed)
  rt$bike_speed <- if (is.null(rt$bike_speed)) 12 else as.numeric(rt$bike_speed)
  rt$max_trip_duration <- if (is.null(rt$max_trip_duration)) cap_minutes() else
    as.numeric(rt$max_trip_duration)
  assert_within_cap(rt$max_trip_duration)
  rt$elevation <- if (is.null(rt$elevation)) "NONE" else as.character(rt$elevation)
  rt$time_window <- if (is.null(rt$time_window)) 60L else as.integer(rt$time_window)
  rt$percentiles <- if (is.null(rt$percentiles)) c(1L, 50L) else
    as.integer(rt$percentiles)
  rt$n_threads <- if (is.null(rt$n_threads)) Inf else as.numeric(rt$n_threads)
  rt$departure_datetime <- if (is.null(rt$departure_datetime)) NULL else
    as.POSIXct(as.character(rt$departure_datetime), tz = "UTC")
  rt$dem_path <- if (is.null(rt$dem_path)) NULL else as.character(rt$dem_path)
  req$routing <- rt
  req
}

#' Persist / load a chunk request (versioned JSON, atomic write).
chunk_request_save <- function(request, path) {
  stopifnot(identical(request$kind, "matrice-chunk-request"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  atomic_write_json(request, path)
}

chunk_request_load <- function(path) {
  if (!file.exists(path)) {
    stop("chunk request not found: ", path, call. = FALSE)
  }
  j <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(j)) stop("chunk request unreadable: ", path, call. = FALSE)
  as_chunk_request(j)
}

#' The default router: link.R's wrappers dispatched per atomic mode. This is
#' the ONLY place in the runner that knows about r5r's per-mode argument
#' shapes; injected routers replace it wholesale (permanent seam).
default_mode_dispatch <- function(routing) {
  force(routing)
  walk_speed <- routing$walk_speed
  bike_speed <- routing$bike_speed
  max_trip_duration <- routing$max_trip_duration
  n_threads <- routing$n_threads
  time_window <- routing$time_window
  percentiles <- routing$percentiles
  departure_datetime <- routing$departure_datetime
  function(network, origins, destinations, mode) {
    if (identical(mode, "transit")) {
      route_transit_pairs(network, origins, destinations,
                          departure_datetime = departure_datetime,
                          max_trip_duration = max_trip_duration,
                          walk_speed = walk_speed, n_threads = n_threads,
                          time_window = time_window, percentiles = percentiles)
    } else if (identical(mode, "bike")) {
      route_bike_pairs(network, origins, destinations,
                       max_trip_duration = max_trip_duration,
                       bike_speed = bike_speed, n_threads = n_threads)
    } else {
      route_pairs(network, origins, destinations, mode = toupper(mode),
                  max_trip_duration = max_trip_duration,
                  walk_speed = walk_speed, bike_speed = bike_speed,
                  n_threads = n_threads)
    }
  }
}

#' Atomic matrix-chunk write (link.R's write_matrix_chunk + the #21 rename
#' discipline): PID-tagged temp in the target directory, then promote onto the
#' deterministic final path <mode>_<chunk>.parquet.
write_matrix_chunk_atomic <- function(rows, mode, chunk_id, out_dir) {
  stopifnot(is.data.frame(rows))
  stopifnot(is.character(mode), length(mode) == 1L, !is.na(mode),
            nzchar(mode))
  stopifnot(length(chunk_id) == 1L, !is.na(chunk_id))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, sprintf("%s_%d.parquet", mode, as.integer(chunk_id)))
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  arrow::write_parquet(rows, tmp)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  promote_temp_file(tmp, path)
  invisible(path)
}

#' Route ONE chunk across ALL its modes; report one receipt per (mode x chunk).
#'
#' @param request A matrice-chunk-request list (see chunk_request_load): which
#'   chunk, which modes, where the plan parquets live, where artifacts and
#'   receipts land, and the routing parameters.
#' @param router Function(network, origins, destinations, mode) -> pairs.
#'   NULL builds default_mode_dispatch(request$routing) — the real r5r
#'   dispatch. Injection is permanent design, not a test trick: with a stub
#'   router and no network_loader the entire pipeline runs JVM-free.
#' @param network A prebuilt network handle passed straight to the router.
#' @param network_loader Zero-argument function producing that handle when
#'   `network` is NULL (the production child passes
#'   default_network_loader(request); tests pass neither and inject a router).
#'
#' @return Invisibly, list(artifacts = named paths, receipts = named lists).
#' @export
run_chunk_worker <- function(request, router = NULL, network = NULL,
                             network_loader = NULL, verbose = FALSE) {
  req <- if (inherits(request, "matrice-chunk-request")) request
         else as_chunk_request(request)
  cid <- as.integer(req$chunk_id)
  chunk_size <- as.integer(req$chunk_size)

  # --- inputs: the persisted plan (identical bytes in every process) --------
  pts <- data.table::as.data.table(arrow::read_parquet(req$paths$origin_points))
  dest_pts <- data.table::as.data.table(arrow::read_parquet(req$paths$destination_points))
  origin_link <- data.table::as.data.table(arrow::read_parquet(req$paths$origin_link))
  dest_link <- data.table::as.data.table(arrow::read_parquet(req$paths$destination_link))
  dest_map <- data.table::as.data.table(arrow::read_parquet(req$paths$destination_map))

  origins_chunk <- chunk_point_slice(pts, cid, chunk_size)

  # --- routing handle -------------------------------------------------------
  rt <- req$routing
  r <- if (is.null(router)) default_mode_dispatch(rt) else router
  net <- if (!is.null(network)) network
         else if (!is.null(network_loader)) network_loader()
         else NULL
  if (is.null(net) && is.null(router)) {
    stop(paste0(
      "run_chunk_worker: the default r5r dispatch needs a built network — ",
      "pass network or network_loader (production children get ",
      "default_network_loader), or inject a router for JVM-free operation"),
      call. = FALSE)
  }

  say <- function(...) if (isTRUE(verbose)) message(...)
  say(sprintf("run_chunk_worker: chunk %d/%d (%d routing coordinates) x %d destination coordinates; modes %s",
              cid, ceiling(req$n_origin_coords / chunk_size), nrow(origins_chunk),
              nrow(dest_pts), paste(req$modes, collapse = " + ")))

  artifacts <- list()
  receipts <- list()
  derive_rows <- function(pairs, mode) {
    if (identical(mode, "transit")) {
      return(derive_transit_matrix_rows(pairs, dest_map, mode))
    }
    # r5r labels the single-value reading travel_time_p50 even for
    # window-less modes (its misnomer); normalize like run_tracer does.
    if ("travel_time_p50" %in% names(pairs)) {
      data.table::setnames(pairs, "travel_time_p50", "travel_time")
    }
    if (!"travel_time" %in% names(pairs)) {
      stop(sprintf(
        "router returned %s for %s; expected a travel_time (or travel_time_p50) column",
        paste(names(pairs), collapse = ", "), mode), call. = FALSE)
    }
    views <- route_pair_views(pairs)
    derive_matrix_rows(views$collapsed, dest_map, mode)
  }

  for (mode in req$modes) {
    t0 <- proc.time()[["elapsed"]]
    pairs <- r(net, origins_chunk, dest_pts, mode)
    stopifnot(is.data.frame(pairs))
    route_seconds <- proc.time()[["elapsed"]] - t0

    # Expansion BEFORE derivation (#20): identity-keyed pairs, sparse kept.
    pairs <- data.table::as.data.table(pairs)
    if (nrow(pairs) > 0L) {
      pairs <- expand_pairs_to_identities(pairs, origin_link, dest_link)
    }

    rows <- if (nrow(pairs) == 0L) {
      # A fully unreachable pass still derives a valid 0-row matrix;
      # data.table's aggregation emits its benign min-over-nothing warning
      # on the way — scoped here where emptiness is proven.
      suppressWarnings(derive_rows(pairs, mode))
    } else {
      derive_rows(pairs, mode)
    }

    # Temp-write -> rename, then validate BEFORE the receipt exists.
    path <- write_matrix_chunk_atomic(rows, mode, cid, req$paths$artifacts_dir)
    m <- read_matrix(path)
    validate_matrix(m)
    n_rows <- nrow(rows)
    rm(pairs, rows, m)

    versions <- tryCatch(r5r_runtime_versions(),
                         error = function(e) list(r5r = NA_character_,
                                                  r5 = NA_character_))
    receipt <- list(
      kind = "matrice-chunk-receipt",
      receipt_version = 1L,
      mode = mode,
      chunk_id = cid,
      status = "complete",
      path = basename(path),
      n_rows = as.integer(n_rows),
      sha256 = sha256_file(path),
      route_seconds = as.numeric(route_seconds),
      validated_at = utc_now_iso(),
      heap = if (is.null(req$heap)) NULL else as.character(req$heap),
      r5r_version = versions$r5r,
      r5_version = versions$r5
    )
    atomic_write_json(receipt, chunk_receipt_path(req$paths$receipts_dir, mode, cid))

    key <- chunk_entry_id(mode, cid)
    artifacts[[key]] <- path
    receipts[[key]] <- receipt
    say(sprintf("run_chunk_worker: %s chunk %d complete: %d rows in %.1f s -> %s",
                mode, cid, n_rows, route_seconds, basename(path)))
  }

  invisible(list(artifacts = artifacts, receipts = receipts))
}

#' The production network loader: build/reuse the r5r network in the request's
#' network directory (network.dat cache reloads in seconds — the decisive
#' fact behind per-chunk processes). Called only after the bootstrap set the
#' D6 heap.
default_network_loader <- function(request) {
  force(request)
  rt <- request$routing
  elevation <- if (is.null(rt$dem_path)) "NONE" else rt$dem_path
  function() {
    link_network(data_path = request$network_dir, elevation = elevation,
                 verbose = FALSE)
  }
}
