# S11 — the once-run driver: origin chunking, per-mode routing against the
# full BPE universe, heap discipline (D6/D7/D8), and the per-(chunk x mode)
# parquet writes (D10). The driver the tracer (ticket 06) executes on the toy
# region — real walk + car routing of the Fougeres residential buildings
# against the full BPE universe — and the full run is a re-run with different
# arguments (run-strategy §2 end-to-end: acquire -> build once -> route by
# chunks -> derive in R -> publish).
#
# run_tracer owns the loop and the memory discipline; link_network,
# route_pairs, derive_matrix_rows, write_matrix_chunk (link.R, S10) are the
# module it drives, and it never calls r5r's engine itself. Two driver-side
# preparations the module contract requires (both discovered against real BPE
# 2025 data, see the tracer run note):
#   * destination ids are per (establishment, TYPEQU): BPE rows are per
#     equipement, so one SIRET hosts many TYPEQU — derive_matrix_rows requires
#     a unique-id map (one TYPEQU per id), and the matrix counts are
#     establishment-level (derive.R: "establishments within T"). Routable rows
#     with an empty SIRET (33 % of the routable universe in BPE 2025) get a
#     synthetic per-point id — sharing id "" would cross-contaminate every
#     TYPEQU the anonymous rows host;
#   * the pair pass is pre-aggregated to the per-(from_id, to_id) minimum so
#     an establishment listed at several points counts once, at its nearest
#     point (the derive step counts rows, so one pair row per establishment is
#     the only way to get establishment counts through it).
#
# Reading discipline: this file's data.tables are extracted with `[[` or `j`,
# NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' The once-run driver: route residential buildings against the full BPE
#' universe for a set of atomic modes and write the matrix parquet.
#'
#' The tracer's end-to-end run (run-strategy §2): read the EPCI's residential
#' universe (origins), read the border-aware BPE universe (destinations, the
#' full type axis per ADR-0002), build the street network once from
#' \code{network_pbf} (cached as network.dat in a dedicated directory, D1/D2),
#' route origins in chunks of \code{chunk_size} against every destination for
#' each \code{mode}, derive the matrix rows in R (D3), write one parquet per
#' (chunk x mode) under \code{out_dir} (D10), and validate each written chunk
#' against the matrix contract (validate_matrix). The whole run is
#' parameterized so the full run is a re-run with different arguments.
#'
#' @param network_pbf Path to the OSM extract the network is built from (the
#'   region crop, Bretagne + the ADR-0002 strip for the full run). It is
#'   copied into a dedicated \code{data/acquired/osm/network_<basename>/}
#'   directory before setup_r5 — setup_r5 scans \code{data_path} for ALL
#'   \code{.osm.pbf}, so the shared osm directory (which also holds the
#'   Bretagne crops) must never be passed directly.
#' @param epci SIREN of the EPCI whose communes scope the origins (default:
#'   the toy region, CA Fougeres Agglomeration).
#' @param modes Atomic modes to route, lowercase labels (constants.R). The
#'   tracer subset is walk + car.
#' @param chunk_size Max origins per routing chunk (D7 — the primary memory
#'   lever; transit contingency 50k is a full-run decision).
#' @param W Strip width in metres for read_bpe_universe (ADR-0002; 25 km
#'   today).
#' @param walk_speed Walking speed in km/h (D5; 4 km/h).
#' @param max_trip_duration The cap in minutes (D4; cap_minutes() = 30).
#' @param n_threads r5r thread count (D8: Inf = Java default, no pre-cap).
#' @param out_dir Where the per-(chunk x mode) parquet land (data/matrice/).
#' @param data_dir The project data root.
#' @param manifest_path The acquisition manifest.
#' @param use_cache Hit the readers' caches (the acquired universes).
#' @param verbose Message each step.
#'
#' @section Heap contract (D6): the CALLER must set
#'   \code{options(java.parameters = "-Xmx24G")} BEFORE loading anything that
#'   touches r5r/rJava — a package function cannot set it after the JVM has
#'   started. run_tracer asserts the option is present and warns when the heap
#'   is not the 24G budget.
#'
#' @return Invisibly, a list summary of the run: origin/destination counts,
#'   the network build time, per-mode stats (route seconds, pair and row
#'   counts, chunk count), the written parquet paths, and the java heap
#'   setting. A human-readable summary is printed.
#' @export
run_tracer <- function(network_pbf,
                       epci = "200072452",
                       modes = c("walk", "car"),
                       chunk_size = 100000L,
                       W = 25000,
                       walk_speed = 4,
                       max_trip_duration = cap_minutes(),
                       n_threads = Inf,
                       out_dir = file.path("data", "matrice"),
                       data_dir = "data",
                       manifest_path = file.path(data_dir, "manifest.json"),
                       use_cache = TRUE,
                       verbose = TRUE) {
  # --- load-bearing arguments ----------------------------------------------
  stopifnot(is.character(network_pbf), length(network_pbf) == 1L,
            !is.na(network_pbf), nzchar(network_pbf), file.exists(network_pbf))
  stopifnot(is.character(epci), length(epci) == 1L, !is.na(epci), nzchar(epci))
  stopifnot(is.character(modes), length(modes) >= 1L, all(modes %in% atomic_modes()))
  stopifnot(is.numeric(chunk_size), length(chunk_size) == 1L,
            !is.na(chunk_size), chunk_size >= 1L)
  stopifnot(is.numeric(W), length(W) == 1L, !is.na(W), W > 0)
  stopifnot(is.numeric(walk_speed), length(walk_speed) == 1L,
            !is.na(walk_speed), walk_speed > 0)
  stopifnot(is.numeric(max_trip_duration), length(max_trip_duration) == 1L,
            !is.na(max_trip_duration), max_trip_duration > 0)

  # --- 1. heap guard (D6) --------------------------------------------------
  # The caller owns the heap: options(java.parameters) must be set before any
  # r5r/rJava load, or the JVM starts at r5r's 512m default and the build +
  # chunked routing cannot fit. Assert the option, warn on a non-24G budget.
  jp <- getOption("java.parameters")
  if (is.null(jp) || !any(grepl("-Xmx", jp, fixed = TRUE))) {
    stop(
      "run_tracer: the -Xmx java heap must be set by the caller via ",
      "options(java.parameters = '-Xmx24G') BEFORE loading anything that ",
      "touches r5r/rJava (D6 hard budget) — a package function cannot set it ",
      "after the JVM has started",
      call. = FALSE
    )
  }
  heap_line <- jp[grepl("-Xmx", jp, fixed = TRUE)][[1L]]
  if (!grepl("24G", heap_line, fixed = TRUE)) {
    warning(sprintf(
      "run_tracer: java heap is '%s', not the D6 24G budget (-Xmx24G) — the tracer's heap-headroom settlement (run-strategy §3) assumes 24G",
      heap_line
    ), call. = FALSE)
  }
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: heap %s (D6); modes %s; chunk_size %d; cap %g min; walk %g km/h; W %g m; n_threads %s",
      heap_line, paste(modes, collapse = " + "), chunk_size, max_trip_duration,
      walk_speed, W, if (is.infinite(n_threads)) "Inf" else n_threads
    ))
  }

  # --- 2. origins (toy scope: the EPCI's residential buildings) -------------
  communes <- read_epci_communes(epci, data_dir = data_dir,
                                 manifest_path = manifest_path,
                                 use_cache = use_cache)
  if (isTRUE(verbose)) {
    message(sprintf("run_tracer: EPCI %s -> %d communes", epci, nrow(communes)))
  }
  bdnb <- read_bdnb_residential_universe(
    communes = communes[["code_insee"]],
    data_dir = data_dir, manifest_path = manifest_path, use_cache = use_cache
  )
  if (isTRUE(verbose)) {
    message(sprintf("run_tracer: %d residential origins (BDNB universe, EPSG:2154)", nrow(bdnb)))
  }

  # EPSG:2154 -> WGS84 for r5r (origins table: id, lon, lat only).
  sf_pts <- sf::st_as_sf(as.data.frame(bdnb),
                         coords = c("x_2154", "y_2154"), crs = 2154L,
                         remove = FALSE)
  sf_pts <- sf::st_transform(sf_pts, 4326L)
  xy <- as.data.frame(sf::st_coordinates(sf_pts))
  origins <- data.table::data.table(id = bdnb[["origin_id"]],
                                    lon = xy[["X"]], lat = xy[["Y"]])
  ok_origin <- !is.na(origins[["lon"]]) & !is.na(origins[["lat"]])
  n_origin_excluded <- sum(!ok_origin)
  if (n_origin_excluded > 0L) {
    # The universe should carry none (drop_fictive_only + the geometry
    # hierarchy resolve every origin); if any appear, keep them out of the
    # routing and report — never fail the run on them.
    warning(sprintf(
      "run_tracer: %d origin(s) with NA lon/lat excluded from routing",
      n_origin_excluded
    ), call. = FALSE)
    origins <- origins[ok_origin]
  }

  # --- 3. destinations (full BPE universe, ADR-0002) ------------------------
  bpe <- read_bpe_universe(W = W, data_dir = data_dir,
                           manifest_path = manifest_path, use_cache = use_cache)
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: BPE universe %d rows (%d bretagne, %d zone_frontaliere)",
      nrow(bpe), sum(bpe[["zone"]] == "bretagne"),
      sum(bpe[["zone"]] == "zone_frontaliere")
    ))
  }
  rout <- bpe[!is.na(bpe[["LONGITUDE"]]) & !is.na(bpe[["LATITUDE"]])]
  n_na_coord <- nrow(bpe) - nrow(rout)
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: %d BPE rows routable (non-NA coords); %d anonymised NA-coord rows stay on the type axis but cannot snap",
      nrow(rout), n_na_coord
    ))
  }

  # SIRET sanity (known: 0 NA — the empty-string case is the real quirk).
  n_siret_na <- sum(is.na(bpe[["SIRET"]]))
  n_siret_shared <- nrow(bpe) - data.table::uniqueN(bpe[["SIRET"]])
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: SIRET: %d NA (expected 0); %d rows share a SIRET (BPE rows are per-equipement: one SIRET hosts many TYPEQU, and the routable universe carries %d empty-SIRET rows)",
      n_siret_na, n_siret_shared, sum(!nzchar(rout[["SIRET"]]))
    ))
  }

  # Destination identity, per the matrix contract (see the file header):
  #   - unique routable points first (dedupes exact (SIRET, point) repeats);
  #   - a base id per point: SIRET where present, else a synthetic
  #     no-siret_%06d (per-point; the establishment identity is the point);
  #   - one routing destination per (base id, TYPEQU), id = base_id|TYPEQU —
  #     the map then has unique ids (derive_matrix_rows' requirement) and each
  #     TYPEQU an establishment hosts counts once (derive.R's establishment
  #     semantic).
  pts <- unique(rout[, .(SIRET, lon = LONGITUDE, lat = LATITUDE)])
  base_id <- pts[["SIRET"]]
  empty_pts <- which(!nzchar(base_id))
  base_id[empty_pts] <- sprintf("no-siret_%06d", seq_along(empty_pts))
  data.table::set(pts, j = "base_id", value = base_id)
  rout_pts <- unique(rout[, .(SIRET, lon = LONGITUDE, lat = LATITUDE, TYPEQU)])
  mapped <- pts[rout_pts, on = c("SIRET", "lon", "lat")]
  data.table::set(mapped, j = "id",
                  value = paste0(mapped[["base_id"]], "|", mapped[["TYPEQU"]]))
  destinations <- mapped[, .(id, lon, lat)]
  dest_map <- unique(mapped[, .(id, TYPEQU)])
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: destinations %d routing points (%d with empty SIRET -> synthetic id) -> %d (id, TYPEQU) destinations, map %d entries (unique ids)",
      nrow(pts), length(empty_pts), nrow(destinations), nrow(dest_map)
    ))
  }

  # --- 4. network (build once, D1/D2) ---------------------------------------
  # Dedicated dir: setup_r5 scans data_path for ALL .osm.pbf, and the shared
  # data/acquired/osm/ dir also holds the bretagne crop + merged extracts.
  net_dir <- file.path(data_dir, "acquired", "osm",
                       paste0("network_", basename(network_pbf)))
  dir.create(net_dir, recursive = TRUE, showWarnings = FALSE)
  target_pbf <- file.path(net_dir, basename(network_pbf))
  if (!file.exists(target_pbf)) {
    if (isTRUE(verbose)) {
      message(sprintf("run_tracer: copying network pbf into dedicated dir %s", net_dir))
    }
    file.copy(network_pbf, target_pbf)
  }
  # Teardown on any error path (registered before the network exists). NB r5r
  # 2.3.0's stop_r5 REMOVES the network object from the caller's frame
  # (rm(list = names(running_cores), envir = parent.frame())) — after the
  # explicit stop in step 6 the local `net` binding is gone, so the guard must
  # test existence, not NULL.
  net <- NULL
  on.exit(if (exists("net", inherits = FALSE) && !is.null(net)) {
    r5r::stop_r5(net)
  }, add = TRUE)
  t_net0 <- proc.time()[["elapsed"]]
  net <- link_network(data_path = net_dir, verbose = isTRUE(verbose))
  network_build_seconds <- proc.time()[["elapsed"]] - t_net0
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: network built/reused in %.1f s (network.dat at %s)",
      network_build_seconds, file.path(net_dir, "network.dat")
    ))
  }

  # --- 5. chunk loop (D7/D10) ------------------------------------------------
  n_chunks <- ceiling(nrow(origins) / chunk_size)
  run_stats <- list()
  files <- character(0)
  for (i in seq_len(n_chunks)) {
    idx <- seq.int((i - 1L) * chunk_size + 1L, min(i * chunk_size, nrow(origins)))
    origins_chunk <- origins[idx]
    if (isTRUE(verbose)) {
      message(sprintf(
        "run_tracer: chunk %d/%d: %d origins x %d destinations",
        i, n_chunks, nrow(origins_chunk), nrow(destinations)
      ))
    }
    for (mode in modes) {
      t0 <- proc.time()[["elapsed"]]
      pairs <- route_pairs(
        net, origins_chunk, destinations,
        mode = toupper(mode),
        max_trip_duration = max_trip_duration,
        walk_speed = walk_speed,
        n_threads = n_threads
      )
      route_seconds <- proc.time()[["elapsed"]] - t0
      n_pairs <- nrow(pairs)

      # r5r 2.3.0 labels the single-value reading travel_time_p50 (its default
      # percentiles = 50L) even for window-less walk/car — the label is a
      # misnomer there: direct modes have no departure window, the value IS
      # the travel time. derive_matrix_rows' documented seam: the caller
      # slices the percentile column to `travel_time` before calling.
      if ("travel_time_p50" %in% names(pairs)) {
        data.table::setnames(pairs, "travel_time_p50", "travel_time")
      }
      if (!"travel_time" %in% names(pairs)) {
        stop(sprintf(
          "run_tracer: r5r returned %s; expected a travel_time (or travel_time_p50) column",
          paste(names(pairs), collapse = ", ")
        ), call. = FALSE)
      }

      # Establishment-level pair pass: an establishment listed at several
      # points yields one r5r pair row per point; collapse to the
      # per-(from_id, to_id) minimum so derive_matrix_rows counts
      # establishments (derive.R), at their nearest point.
      if (n_pairs > 0L) {
        pairs <- pairs[, .(travel_time = min(travel_time)),
                       by = .(from_id, to_id)]
      }

      rows <- derive_matrix_rows(pairs, dest_map, mode)
      n_rows <- nrow(rows)
      path <- write_matrix_chunk(rows, mode, i, out_dir)
      files <- c(files, path)

      # The driver validates the assembled matrix (write_matrix_chunk's
      # contract: "the driver validates ... after the loop").
      m <- read_matrix(path)
      validate_matrix(m)

      run_stats[[length(run_stats) + 1L]] <- list(
        chunk_id = i, mode = mode, route_seconds = route_seconds,
        n_pairs = n_pairs, n_rows = n_rows, path = path
      )
      if (isTRUE(verbose)) {
        message(sprintf(
          "run_tracer: %s chunk %d: routed in %.1f s (%d pairs -> %d rows); wrote %s; validated",
          mode, i, route_seconds, n_pairs, n_rows, path
        ))
      }

      # D7 discipline: drop the transient tables and run both GCs before the
      # next chunk/mode.
      rm(pairs, rows, m)
      gc()
      rJava::.jgc(R.gc = TRUE)
    }
  }

  # --- 6. teardown -----------------------------------------------------------
  r5r::stop_r5(net)

  # --- 7. summary ------------------------------------------------------------
  per_mode <- lapply(split(run_stats, vapply(run_stats, `[[`, "", "mode")),
                     function(entries) {
                       list(
                         route_seconds = sum(vapply(entries, `[[`, 0, "route_seconds")),
                         n_pairs = sum(vapply(entries, `[[`, 0L, "n_pairs")),
                         n_rows = sum(vapply(entries, `[[`, 0L, "n_rows")),
                         n_chunks = length(entries)
                       )
                     })
  out <- list(
    network_pbf = network_pbf,
    epci = epci,
    n_origins = nrow(origins),
    n_destinations = nrow(destinations),
    n_routable_destinations = nrow(rout),
    n_na_coord_destinations = n_na_coord,
    n_map_entries = nrow(dest_map),
    network_dir = net_dir,
    network_dat = file.path(net_dir, "network.dat"),
    network_build_seconds = network_build_seconds,
    per_mode = per_mode,
    files = files,
    java_heap = heap_line
  )

  # Human-readable settlement (run-strategy §3 measurements).
  cat(sprintf(
    paste0(
      "\nrun_tracer summary (%s)\n",
      "  heap            : %s (D6)\n",
      "  origins         : %d (EPCI %s)\n",
      "  destinations    : %d routable BPE rows (of %d universe; %d NA-coord excluded)\n",
      "                    -> %d (id, TYPEQU) routing destinations, map %d entries\n",
      "  network build   : %.1f s (network.dat at %s)\n"
    ),
    paste(modes, collapse = " + "), heap_line, nrow(origins), epci,
    nrow(rout), nrow(bpe), n_na_coord, nrow(destinations), nrow(dest_map),
    network_build_seconds, out$network_dat
  ))
  for (mode in modes) {
    st <- per_mode[[mode]]
    cat(sprintf(
      "  %-4s             : routed in %.1f s; r5r returned %d pairs -> %d matrix rows (%d chunk%s)\n",
      mode, st$route_seconds, st$n_pairs, st$n_rows, st$n_chunks,
      if (st$n_chunks == 1L) "" else "s"
    ))
  }
  cat(sprintf("  parquet files   : %s\n", paste(files, collapse = ", ")))

  invisible(out)
}
