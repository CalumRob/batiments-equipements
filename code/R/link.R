# S10 — the linking module: the once-run's routing pass (r5r wrappers, the
# R-shape derivation, the matrix writer).
#
# Implements run-strategy §2 steps 2–3 for the single-value modes (walk, car —
# the tracer's subset; bike is the same shape; transit's percentile axis is an
# extension, see derive_matrix_rows). link_network and route_pairs are thin
# wrappers over r5r's setup_r5 / travel_time_matrix; derive_matrix_rows turns
# the transient pair table into the matrix contract shape (D3 — the readings
# come from the pair pass, never from accessibility()); write_matrix_chunk
# writes one (mode × chunk) parquet to data/matrice/ (D10). The driver — a
# later worker — owns the loop: origin chunking, the gc() + rJava::.jgc()
# discipline (D7), and final validation; this file never calls r5r's engine
# itself.
#
# Reading discipline: this file's data.tables are extracted with `[[` or `j`,
# NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' Build (or reuse) the r5r street network for a data directory.
#'
#' Thin wrapper over r5r::setup_r5. The tracer runs elevation OFF (ADR-0001),
#' so the default here is "NONE" — a deliberate override of r5r's own default
#' ("TOBLER"): forgetting it would silently make the walk/bike travel times
#' hill-weighted. The built network is cached as network.dat *in* `data_path`
#' (r5r's own cache): a second setup_r5 on the same directory reuses it,
#' re-running nothing (run-strategy §2 step 2). Returns the r5r_network
#' object, ready for route_pairs.
link_network <- function(data_path, elevation = "NONE", verbose = FALSE,
                         overwrite = FALSE) {
  stopifnot(is.character(data_path), length(data_path) == 1L,
            !is.na(data_path), nzchar(data_path))
  stopifnot(is.character(elevation), length(elevation) == 1L,
            !is.na(elevation), nzchar(elevation))
  if (!identical(toupper(elevation), "NONE")) {
    # r5r accepts a DEM path here, but does not provide a useful error for a
    # missing/unreadable input. Validate before setup so elevation can never be
    # accidentally requested with an unvalidated raster.
    validate_dem_raster(elevation)
  } else {
    elevation <- "NONE"
  }
  r5r::setup_r5(
    data_path = data_path,
    verbose = verbose,
    elevation = elevation,
    overwrite = overwrite
  )
}

#' Route one chunk of origins against the destinations; returns the pairs.
#'
#' Thin wrapper over r5r::travel_time_matrix for the single-value modes. The
#' once-run routes per origin chunk (run-strategy D1/D7); `origins` and
#' `destinations` are data.frame/data.table of points with columns id, lon,
#' lat (WGS84) — the caller prepares those (e.g. from the BDNB residential
#' universe and the BPE universe). `mode` is the r5r mode string ("WALK" |
#' "CAR" | "BICYCLE" | ...).
#'
#' time_window/percentiles are deliberately OMITTED: walk/car are single-value
#' modes with no departure window (run-strategy D3), so the output carries one
#' travel_time column. Returns the pair table r5r returns, as a data.table:
#' from_id, to_id, travel_time — one row per (origin, destination) pair
#' reachable within max_trip_duration (r5r omits the unreachable pairs, which
#' is exactly the matrix's sparsity, D3). `max_trip_duration` defaults to the
#' cap (cap_minutes = 30, D4); values above it would produce rows whose
#' tt_nearest exceeds the cap and fail the contract validator downstream.
route_pairs <- function(network, origins, destinations, mode,
                        max_trip_duration = cap_minutes(),
                        walk_speed = 4, bike_speed = 12, n_threads = Inf) {
  stopifnot(is.numeric(max_trip_duration), length(max_trip_duration) == 1L,
            !is.na(max_trip_duration), max_trip_duration > 0)
  stopifnot(is.character(mode), length(mode) == 1L, !is.na(mode), nzchar(mode))
  stopifnot(is.numeric(walk_speed), length(walk_speed) == 1L,
            !is.na(walk_speed), walk_speed > 0)
  stopifnot(is.numeric(bike_speed), length(bike_speed) == 1L,
            !is.na(bike_speed), bike_speed > 0)
  r5r_mode <- c(walk = "WALK", car = "CAR", bike = "BICYCLE",
                transit = "TRANSIT")[tolower(mode)]
  if (is.na(r5r_mode)) r5r_mode <- toupper(mode)
  data.table::as.data.table(
    r5r::travel_time_matrix(
      r5r_network = network,
      origins = origins,
      destinations = destinations,
      mode = r5r_mode,
      max_trip_duration = max_trip_duration,
      walk_speed = walk_speed,
      bike_speed = bike_speed,
      n_threads = n_threads,
      verbose = FALSE,
      progress = FALSE
    )
  )
}

#' Route one origin chunk by the atomic walk+transit composite.
#'
#' This is deliberately separate from `route_pairs()`: r5r's transit result is
#' a departure-window distribution, not the single reading returned for walk,
#' car, and bicycle.  The requested window is 60 minutes and the two retained
#' readings are the best case (p1) and median (p50).
route_transit_pairs <- function(network, origins, destinations,
                                departure_datetime,
                                max_trip_duration = cap_minutes(),
                                walk_speed = 4, n_threads = Inf,
                                time_window = 60, percentiles = c(1, 50)) {
  if (!inherits(departure_datetime, "POSIXct") ||
      length(departure_datetime) != 1L || is.na(departure_datetime)) {
    stop("departure_datetime must be one non-NA POSIXct date-time; it is required for reproducible transit routing",
         call. = FALSE)
  }
  stopifnot(is.numeric(max_trip_duration), length(max_trip_duration) == 1L,
            !is.na(max_trip_duration), max_trip_duration > 0)
  stopifnot(is.numeric(walk_speed), length(walk_speed) == 1L,
            !is.na(walk_speed), walk_speed > 0)
  stopifnot(is.numeric(time_window), length(time_window) == 1L,
            !is.na(time_window), time_window > 0)
  stopifnot(is.numeric(percentiles), length(percentiles) == 2L,
            !anyNA(percentiles), identical(as.numeric(percentiles), c(1, 50)))

  data.table::as.data.table(
    r5r::travel_time_matrix(
      r5r_network = network,
      origins = origins,
      destinations = destinations,
      mode = "TRANSIT",
      departure_datetime = departure_datetime,
      max_trip_duration = max_trip_duration,
      walk_speed = walk_speed,
      time_window = time_window,
      percentiles = percentiles,
      n_threads = n_threads,
      verbose = FALSE,
      progress = FALSE
    )
  )
}

# r5r has changed the spelling of percentile columns between releases. Keep
# the compatibility rule in one place, and return the canonical contract name.
transit_percentile_column <- function(names_in, percentile) {
  candidates <- if (percentile == 1L) {
    c("travel_time_p1", "travel_time_p01", "travel_time_1",
      "travel_time_percentile_1", "travel_time_percentile_01")
  } else {
    c("travel_time_p50", "travel_time_50", "travel_time_percentile_50")
  }
  found <- candidates[candidates %in% names_in]
  if (length(found) == 0L) {
    stop(sprintf(
      "transit pairs missing travel-time percentile p%d; expected one of: %s (received: %s)",
      percentile, paste(candidates, collapse = ", "), paste(names_in, collapse = ", ")
    ), call. = FALSE)
  }
  found[[1L]]
}

#' Derive the transit matrix axis without discarding the percentile readings.
#'
#' The chosen matrix contract is:
#' `tt_nearest` and `count_*` are the p1 (primary/compatibility) reading;
#' `travel_time_p1` and `travel_time_p50` retain nearest p1/p50 times; and
#' `count_*_p50` retains the p50 count ladder.  Thus downstream derivations can
#' select the median without routing again.  The destination map must have one
#' TYPEQU per id; duplicate routing points remain rows in `pairs` and are
#' collapsed by minimum time/count aggregation at (from_id, TYPEQU).
derive_transit_matrix_rows <- function(pairs, destinations, mode = "transit") {
  stopifnot(is.data.frame(pairs), is.data.frame(destinations))
  stopifnot(is.character(mode), length(mode) == 1L, !is.na(mode),
            identical(mode, "transit"))

  p <- data.table::as.data.table(pairs)
  dst <- data.table::as.data.table(destinations)
  missing_pairs <- setdiff(c("from_id", "to_id"), names(p))
  if (length(missing_pairs) > 0L) {
    stop(sprintf("transit pairs missing required columns: %s",
                 paste(missing_pairs, collapse = ", ")), call. = FALSE)
  }
  missing_dst <- setdiff(c("id", "TYPEQU"), names(dst))
  if (length(missing_dst) > 0L) {
    stop(sprintf("destinations missing required columns: %s",
                 paste(missing_dst, collapse = ", ")), call. = FALSE)
  }
  if (anyDuplicated(dst[["id"]])) {
    stop("destinations must map each to_id to exactly one TYPEQU (duplicate id)",
         call. = FALSE)
  }

  p1_col <- transit_percentile_column(names(p), 1L)
  p50_col <- transit_percentile_column(names(p), 50L)
  if (!is.numeric(p[[p1_col]]) || !is.numeric(p[[p50_col]])) {
    stop("transit percentile travel-time columns must be numeric", call. = FALSE)
  }
  if (anyNA(p[[p1_col]]) || anyNA(p[[p50_col]])) {
    stop("transit percentile travel-time columns must not be NA", call. = FALSE)
  }
  if (any(p[[p1_col]] > p[[p50_col]])) {
    stop("transit travel_time_p1 must be <= travel_time_p50", call. = FALSE)
  }

  # nomatch = 0 preserves sparse reachability and excludes unmapped ids.
  merged <- p[dst, on = c(to_id = "id"), nomatch = 0L]
  primary_counts <- stats::setNames(
    lapply(ladder_rungs(), function(r) sum(.SD[[1L]] <= r)), ladder_cols()
  )
  median_counts <- stats::setNames(
    lapply(ladder_rungs(), function(r) sum(.SD[[2L]] <= r)),
    paste0(ladder_cols(), "_p50")
  )
  out <- merged[, c(
    list(
      tt_nearest = min(.SD[[1L]]),
      travel_time_p1 = min(.SD[[1L]]),
      travel_time_p50 = min(.SD[[2L]])
    ), primary_counts, median_counts),
    by = .(from_id, TYPEQU), .SDcols = c(p1_col, p50_col)]

  data.table::setnames(out, "from_id", "batiment_id")
  data.table::set(out, j = "mode", value = mode)
  data.table::setcolorder(out, c(
    "batiment_id", "TYPEQU", "mode", "tt_nearest", ladder_cols(),
    "travel_time_p1", "travel_time_p50", paste0(ladder_cols(), "_p50")
  ))
  data.table::setorder(out, batiment_id, TYPEQU)
  out[]
}

#' Route the bike atomic mode with its explicit r5r mapping and speed.
route_bike_pairs <- function(network, origins, destinations,
                             max_trip_duration = cap_minutes(),
                             bike_speed = 12, n_threads = Inf) {
  route_pairs(network, origins, destinations, mode = "BICYCLE",
              max_trip_duration = max_trip_duration,
              bike_speed = bike_speed, n_threads = n_threads)
}

#' Derive matrix rows from a transient pair table (run-strategy D3).
#'
#' Per (from_id, TYPEQU): tt_nearest = min(travel_time) and one count per
#' ladder rung r = sum(travel_time <= r). The count reading comes from the
#' same travel_time_matrix pass as the nearest reading, never from
#' r5r::accessibility() — its dense output is not the contract (D3).
#'
#' `pairs` is a data.table from route_pairs — columns from_id, to_id,
#' travel_time (single-value modes). `destinations` is the to_id → TYPEQU map:
#' a data.table with columns id and TYPEQU, one row per id (the caller prepares
#' it — e.g. the BPE universe's id/TYPEQU columns). `mode` is the MATRIX mode
#' label, lowercase ("walk" | "car"), the value stored in the contract's mode
#' column (an atomic mode, constants.R).
#'
#' Returns a data.table in the matrix contract shape (validate_matrix-
#' compatible): batiment_id (= from_id), TYPEQU, mode, tt_nearest, plus one
#' count column per ladder rung (ladder_cols()). ONE row per (from_id, TYPEQU)
#' that appears in pairs — sparse: r5r only returns pairs reachable within
#' max_trip_duration, so rows exist only where reachable within the cap, by
#' construction.
#'
#' Future-proofing seam (run-strategy "Matrix contract" — transit/bike/
#' elevation are axis EXTENSIONS, not schema breaks): if pairs ever carries
#' the transit percentile columns (travel_time_p1/travel_time_p50) instead of
#' travel_time, this function still works unchanged provided the caller slices
#' first — it picks the single-value travel_time column when present, and
#' errors pointing at that rule when it is absent. No transit machinery is
#' built here.
derive_matrix_rows <- function(pairs, destinations, mode) {
  stopifnot(is.data.frame(pairs), is.data.frame(destinations))
  stopifnot(is.character(mode), length(mode) == 1L, !is.na(mode), nzchar(mode),
            mode %in% atomic_modes())

  p <- data.table::as.data.table(pairs)
  dst <- data.table::as.data.table(destinations)

  missing_pairs <- setdiff(c("from_id", "to_id"), names(p))
  if (length(missing_pairs) > 0) {
    stop(sprintf(
      "pairs missing required columns: %s",
      paste(missing_pairs, collapse = ", ")
    ), call. = FALSE)
  }
  missing_dst <- setdiff(c("id", "TYPEQU"), names(dst))
  if (length(missing_dst) > 0) {
    stop(sprintf(
      "destinations missing required columns: %s",
      paste(missing_dst, collapse = ", ")
    ), call. = FALSE)
  }
  if (anyDuplicated(dst[["id"]])) {
    stop("destinations must map each to_id to exactly one TYPEQU (duplicate id)",
         call. = FALSE)
  }

  # The single-value travel_time column. Transit's percentiles are an axis
  # extension: the caller slices the chosen percentile to `travel_time` before
  # calling — no transit machinery here (future-proofing seam).
  if (!"travel_time" %in% names(p)) {
    stop(
      "pairs carries no single-value travel_time column; the transit percentile columns (travel_time_p1/travel_time_p50) are axis extensions the caller slices first (rename the chosen percentile to 'travel_time' before calling)",
      call. = FALSE
    )
  }

  # to_id → TYPEQU via the caller's map. nomatch = 0L keeps only pair rows
  # whose destination is mapped — the sparse contract, by construction.
  merged <- p[dst, on = c(to_id = "id"), nomatch = 0L]

  # tt_nearest + the ladder counts, straight from the same pair pass (D3).
  out <- merged[, c(
    list(tt_nearest = min(.SD[[1L]])),
    stats::setNames(
      lapply(ladder_rungs(), function(r) sum(.SD[[1L]] <= r)),
      ladder_cols()
    )
  ), by = .(from_id, TYPEQU), .SDcols = "travel_time"]

  data.table::setnames(out, "from_id", "batiment_id")
  data.table::set(out, j = "mode", value = mode)
  data.table::setcolorder(out, c("batiment_id", "TYPEQU", "mode", "tt_nearest",
                                 ladder_cols()))
  data.table::setorder(out, batiment_id, TYPEQU)
  out[]
}

#' Write one (mode × chunk) slice of the matrix to parquet (run-strategy D10).
#'
#' Writes `rows` (a matrix-shape data.table, as returned by
#' derive_matrix_rows) with arrow::write_parquet to
#' out_dir/<mode>_<chunk_id>.parquet — the long format, one parquet per
#' (mode × chunk), under data/matrice/ by default. Creates out_dir
#' recursively. Returns the file path invisibly. A pure write: the driver
#' validates the assembled matrix (validate_matrix) after the loop.
write_matrix_chunk <- function(rows, mode, chunk_id,
                               out_dir = file.path("data", "matrice")) {
  stopifnot(is.data.frame(rows))
  stopifnot(is.character(mode), length(mode) == 1L, !is.na(mode), nzchar(mode))
  stopifnot(length(chunk_id) == 1L, !is.na(chunk_id))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, sprintf("%s_%s.parquet", mode, chunk_id))
  arrow::write_parquet(rows, path)
  invisible(path)
}

#' Write the unaggregated route pairs for an opt-in instrumentation run.
#'
#' Unlike write_matrix_chunk(), this writer deliberately does not group or
#' deduplicate rows.  In particular, multiple routing points belonging to one
#' destination id remain visible: the destination registry is the separate
#' join seam back to BPE/SIRET/TYPEQU and coordinates.
write_route_pairs_chunk <- function(pairs, mode, chunk_id, run_label,
                                    out_dir) {
  stopifnot(is.data.frame(pairs))
  required <- c("from_id", "to_id", "travel_time")
  missing <- setdiff(required, names(pairs))
  if (length(missing) > 0L) {
    stop(sprintf("pairs missing required columns: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  if (!is.character(pairs[["from_id"]]) || !is.character(pairs[["to_id"]])) {
    stop("pairs from_id and to_id must be character vectors", call. = FALSE)
  }
  if (!is.numeric(pairs[["travel_time"]])) {
    stop("pairs travel_time must be numeric", call. = FALSE)
  }
  stopifnot(is.character(mode), length(mode) == 1L, !is.na(mode), nzchar(mode))
  stopifnot(length(chunk_id) == 1L, !is.na(chunk_id))
  stopifnot(is.character(run_label), length(run_label) == 1L,
            !is.na(run_label), nzchar(run_label))
  stopifnot(is.character(out_dir), length(out_dir) == 1L,
            !is.na(out_dir), nzchar(out_dir))

  out <- data.table::as.data.table(data.table::copy(pairs))
  data.table::set(out, j = "mode", value = mode)
  data.table::set(out, j = "chunk_id", value = as.integer(chunk_id))
  data.table::set(out, j = "run_label", value = run_label)
  data.table::setcolorder(out, c(required, "mode", "chunk_id", "run_label"))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  safe_label <- gsub("[^A-Za-z0-9_.-]+", "-", run_label)
  path <- file.path(out_dir, sprintf("%s_%s_%s.parquet",
                                     safe_label, mode, chunk_id))
  arrow::write_parquet(out, path)
  invisible(path)
}
