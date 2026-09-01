# S11 — the once-run driver: origin chunking, per-mode routing against the
# full BPE universe, heap discipline (D6/D7/D8), and the per-(chunk x mode)
# parquet writes (D10). The driver the tracer (ticket 06) executes on the toy
# region — real walk + car routing of the Fougeres residential buildings
# against the full BPE universe — and the full run is a re-run with different
# arguments (run-strategy §2 end-to-end: acquire -> build once -> route by
# chunks -> derive in R -> publish).
#
# Since #20 the loop routes COORDINATES, not identities: each exact origin
# coordinate routes once across all chunks and each exact destination
# coordinate routes once per run (route-coordinates.R, S13); every chunk's
# pairs expand back to BDNB origin ids and BPE listing ids BEFORE the matrix
# derivation, so the derived matrix stays semantically identical to reference
# (non-deduplicated) routing. Deduplication is exact-coordinate-equality ONLY.
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

#' Select an explicit origin subset without starting the routing engine.
select_origin_ids <- function(origins, origin_ids = NULL) {
  stopifnot(is.data.frame(origins), "origin_id" %in% names(origins))
  if (is.null(origin_ids)) {
    return(list(origins = origins, n_requested = nrow(origins),
                n_selected = nrow(origins)))
  }
  stopifnot(is.character(origin_ids), length(origin_ids) > 0L,
            all(!is.na(origin_ids)), all(nzchar(origin_ids)))
  requested <- unique(origin_ids)
  selected <- origins[origins[["origin_id"]] %in% requested, , drop = FALSE]
  if (nrow(selected) == 0L) {
    stop("origin_ids: none of the requested IDs match the BDNB residential universe",
         call. = FALSE)
  }
  list(origins = selected, n_requested = length(requested),
       n_selected = nrow(selected))
}

#' Keep the raw routing result alongside the matrix's establishment view.
#'
#' This deliberately returns two copies: instrumentation consumes `raw`, while
#' the matrix contract consumes `collapsed`. Keeping this seam explicit makes
#' it difficult to move sidecar capture after aggregation by accident.
route_pair_views <- function(pairs) {
  stopifnot(is.data.frame(pairs))
  raw <- data.table::as.data.table(data.table::copy(pairs))
  collapsed <- if (nrow(raw) > 0L) {
    raw[, .(travel_time = min(travel_time)), by = .(from_id, to_id)]
  } else {
    raw
  }
  list(raw = raw, collapsed = collapsed)
}

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
#' @param W Strip width in metres for read_bpe_universe (ADR-0002;
#'   border_width_m()).
#' @param walk_speed Walking speed in km/h (D5; 4 km/h).
#' @param max_trip_duration The cap in minutes (D4 — the authoritative
#'   once-run cap is 20 minutes, cap_minutes()). Windows beyond the cap are
#'   refused (assert_within_cap): no code path may route or emit beyond it.
#' @param n_threads r5r thread count (D8: Inf = Java default, no pre-cap).
#' @param out_dir Where the per-(chunk x mode) parquet land (data/matrice/).
#' @param data_dir The project data root.
#' @param manifest_path The acquisition manifest.
#' @param use_cache Hit the readers' caches (the acquired universes).
#' @param origin_ids Optional explicit BDNB origin IDs to route. NULL keeps the
#'   complete residential universe.
#' @param elevation NONE disables native elevation; any other symbolic value
#'   enables it and resolves the validated DEM, while an existing DEM filename
#'   is accepted directly.
#' @param dem_path Optional caller-provided DEM filename. It is validated before
#'   being passed to r5r; otherwise cached SRTM tiles are assembled.
#' @param gtfs_path Deprecated single-feed argument retained for compatibility;
#'   transit runs stage the promoted current manifest instead.
#' @param pairs_out_dir Optional directory for raw route-pair parquet sidecars.
#'   NULL (the default) disables instrumentation.
#' @param run_label Label identifying the network/run in pair sidecars.
#' @param transit_service_date Optional GTFS service date. When omitted for a
#'   transit run, it is derived from \code{departure_datetime} in Europe/Paris.
#' @param transit_required_ids Transit feed ids that must have service on the
#'   selected date. The default is \code{full_run_transit_required_ids()}.
#' @param transit_activity_window Half-open local-time feed-activity window;
#'   the default is \code{full_run_transit_activity_window()}.
#' @param verbose Message each step.
#'
#' @section Heap contract (D6): the CALLER must set
#'   \code{options(java.parameters = "-Xmx24G")} BEFORE loading anything that
#'   touches r5r/rJava — a package function cannot set it after the JVM has
#'   started. run_tracer asserts the option is present and warns when the heap
#'   is not the 24G budget.
#'
#' @return Invisibly, a list summary of the run: origin/destination counts
#'   (identities AND unique routing coordinates — #20's coordinate_dedup
#'   block records the exact-equality rule, the expected full-universe census
#'   and the actual routed counts), the network build time, per-mode stats
#'   (route seconds, routed coordinate pairs vs expanded identity pairs and
#'   row counts, chunk count), the written parquet paths (matrix chunks + the
#'   destination registry sidecar path), the java heap setting, and the
#'   cap-and-ladder contract entries (cap_minutes, ladder_rungs, ladder_cols)
#'   that run_metadata.json records. A human-readable summary is printed.
#' @export
run_tracer <- function(network_pbf,
                       epci = "200072452",
                       scope = c("epci", "bretagne"),
                       modes = c("walk", "car"),
                       chunk_size = 100000L,
                       W = border_width_m(),
                       walk_speed = 4,
                       max_trip_duration = cap_minutes(),
                       n_threads = Inf,
                       out_dir = file.path("data", "matrice"),
                       data_dir = "data",
                       manifest_path = file.path(data_dir, "manifest.json"),
                       use_cache = TRUE,
                       origin_ids = NULL,
                       pairs_out_dir = NULL,
                       run_label = "run",
                       verbose = TRUE,
                       departure_datetime = NULL,
                       transit_service_date = NULL,
                       transit_required_ids = full_run_transit_required_ids(),
                       transit_activity_window = full_run_transit_activity_window(),
                       bike_speed = 12,
                       elevation = "NONE",
                       gtfs_path = NULL,
                       dem_path = NULL,
                       dry_run = FALSE) {
  # --- load-bearing arguments ----------------------------------------------
  stopifnot(is.character(network_pbf), length(network_pbf) == 1L,
            !is.na(network_pbf), nzchar(network_pbf), file.exists(network_pbf))
  scope <- match.arg(scope)
  stopifnot(is.character(epci), length(epci) == 1L, !is.na(epci), nzchar(epci))
  stopifnot(is.character(modes), length(modes) >= 1L,
            all(modes %in% atomic_modes()))
  stopifnot(is.character(elevation), length(elevation) == 1L,
            !is.na(elevation), nzchar(elevation))
  probe_run_modes(modes, departure_datetime)
  transit_date <- NULL
  if ("transit" %in% modes) {
    if (!is.character(transit_required_ids) || !length(transit_required_ids) ||
        anyNA(transit_required_ids) || any(!nzchar(transit_required_ids))) {
      stop("transit_required_ids must contain at least one non-empty feed id",
           call. = FALSE)
    }
    transit_date <- resolve_transit_service_date(
      departure_datetime, service_date = transit_service_date
    )
    transit_activity_window <- .as_gtfs_activity_window(
      transit_activity_window
    )
   }
   elevation_enabled <- !identical(toupper(elevation), "NONE")
  requested_dem <- dem_path
  if (isTRUE(elevation_enabled) && is.null(requested_dem) && file.exists(elevation)) {
    requested_dem <- elevation
  }
  stopifnot(is.numeric(bike_speed), length(bike_speed) == 1L, !is.na(bike_speed), bike_speed > 0)
  stopifnot(is.numeric(chunk_size), length(chunk_size) == 1L,
            !is.na(chunk_size), chunk_size >= 1L)
  stopifnot(is.numeric(W), length(W) == 1L, !is.na(W), W > 0)
  stopifnot(is.numeric(walk_speed), length(walk_speed) == 1L,
            !is.na(walk_speed), walk_speed > 0)
  assert_within_cap(max_trip_duration)
  stopifnot(is.character(run_label), length(run_label) == 1L,
            !is.na(run_label), nzchar(run_label))
  if (!is.null(pairs_out_dir)) {
    stopifnot(is.character(pairs_out_dir), length(pairs_out_dir) == 1L,
              !is.na(pairs_out_dir), nzchar(pairs_out_dir))
  }
  if (isTRUE(dry_run)) {
    dry_routing <- full_run_routing_parameters(bike_speed = bike_speed,
                                               elevation = elevation)
    dry_routing$modes <- modes
    dry_routing$max_trip_duration <- max_trip_duration
    dry_routing$walk_speed <- walk_speed
    dry_routing$transit <- list(time_window = 60, percentiles = c(1, 50),
      departure_datetime = if (is.null(departure_datetime)) NULL else
        format(departure_datetime, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
    if ("transit" %in% modes) {
      dry_routing$transit$service_date <- transit_date
      dry_routing$transit$required_ids <- transit_required_ids
      dry_routing$transit$feed_activity_window <- transit_activity_window
    }
    dry_routing$scope <- scope
    dry_routing$W <- W
    dry_routing$chunk_size <- chunk_size
    dry_routing$n_threads <- n_threads
    dry_routing$elevation$dem_path <- NULL
    # The cap-and-ladder contract rides in the metadata (#17): every summary
    # names the authoritative cap and the exact rungs it was derived from.
    # The #20 coordinate-dedup census records the expected full-universe
    # reduction (constants.R) alongside this run's parameters.
    return(invisible(list(scope = scope, modes = modes,
      departure_datetime = departure_datetime, bike_speed = bike_speed,
      elevation = elevation, routing_parameters = dry_routing,
      probe = probe_run_modes(modes, departure_datetime),
      cap_minutes = cap_minutes(),
      ladder_rungs = unname(ladder_rungs()),
      ladder_cols = unname(ladder_cols()),
      coordinate_dedup = list(rule = "exact (lon, lat) equality",
                              expected_full_run = full_run_coordinate_counts()))))
  }

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
  if (identical(scope, "bretagne")) {
    bdnb <- read_bdnb_residential_universe(
      departements = c("22", "29", "35", "56"),
      data_dir = data_dir, manifest_path = manifest_path, use_cache = use_cache)
    scope_label <- "bretagne (departements 22/29/35/56)"
  } else {
    communes <- read_epci_communes(epci, data_dir = data_dir,
                                   manifest_path = manifest_path,
                                   use_cache = use_cache)
    if (isTRUE(verbose)) message(sprintf("run_tracer: EPCI %s -> %d communes", epci, nrow(communes)))
    bdnb <- read_bdnb_residential_universe(
      communes = communes[["code_insee"]],
      data_dir = data_dir, manifest_path = manifest_path, use_cache = use_cache)
    scope_label <- paste0("epci ", epci)
  }
  origin_selection <- select_origin_ids(bdnb, origin_ids)
  bdnb <- origin_selection$origins
  n_origins_requested <- origin_selection$n_requested
  n_origins_selected <- origin_selection$n_selected
  if (isTRUE(verbose)) {
    message(sprintf("run_tracer: %d residential origins selected (%d requested; BDNB universe, EPSG:2154)",
                    nrow(bdnb), n_origins_requested, n_origins_selected))
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

  # Coordinate-level routing (#20): each EXACT origin coordinate routes once
  # across all chunks. The plan is built over the whole universe so a
  # coordinate shared by two chunks still routes exactly one time; expansion
  # back to origin identities happens per chunk, BEFORE derive_matrix_rows,
  # so downstream derivations never see deduplicated identities.
  origin_plan <- coordinate_routing_plan(origins, prefix = "coord_o")
  n_origin_coords <- nrow(origin_plan$points)
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: %d origin identities -> %d unique routing coordinates (#20 exact-coordinate dedup)",
      nrow(origins), n_origin_coords
    ))
  }

  # --- 3. destinations (full BPE universe, ADR-0002) ------------------------
  # Destination preparation is extracted (prepare-destinations.R, S12): the
  # universe read, the routable filter, the per-point base ids, and the
  # (id, TYPEQU) destinations + the lossless registry back-link. The ids are
  # byte-identical to what the recorded run routed (the extraction is
  # verbatim); the registry is the new persistence — any destination id links
  # back to its BPE 2025 universe rows.
  dest_prep <- prepare_bpe_destinations(W, data_dir, manifest_path, use_cache)
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: BPE universe %d rows (%d bretagne, %d zone_frontaliere)",
      dest_prep$n_universe, dest_prep$n_bretagne, dest_prep$n_zone_frontaliere
    ))
    message(sprintf(
      "run_tracer: %d BPE rows routable (non-NA coords); %d anonymised NA-coord rows stay on the type axis but cannot snap",
      dest_prep$n_routable, dest_prep$n_na_coord
    ))
    message(sprintf(
      "run_tracer: SIRET: %d NA (expected 0); %d rows share a SIRET (BPE rows are per-equipement: one SIRET hosts many TYPEQU, and the routable universe carries %d empty-SIRET rows)",
      dest_prep$n_siret_na, dest_prep$n_siret_shared, dest_prep$n_empty_siret
    ))
    message(sprintf(
      "run_tracer: destinations %d routing points (%d with empty SIRET -> synthetic id) -> %d (id, TYPEQU) destinations, map %d entries (unique ids)",
      dest_prep$n_pts, dest_prep$n_empty_pts,
      nrow(dest_prep$destinations), nrow(dest_prep$dest_map)
    ))
  }
  # Coordinate-level routing (#20): each EXACT destination coordinate routes
  # once per run; co-located listings (distinct SIRET/NOMRS/TYPEQU) share one
  # routing point and are restored by expansion before derive_matrix_rows.
  # Deduplication is exact-coordinate-equality ONLY — no snapping, no
  # rounding, no SIRET/NOMRS identity grouping.
  dest_plan <- coordinate_routing_plan(dest_prep$destinations,
                                       prefix = "coord_d")
  n_dest_coords <- nrow(dest_plan$points)
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: %d listing identities -> %d unique routing coordinates (#20 exact-coordinate dedup)",
      nrow(dest_prep$destinations), n_dest_coords
    ))
  }

  # The registry sidecar: a pure function of the pinned universe (cache-hit
  # derived), written once per run before the network build / chunk loop.
  run_out_dir <- file.path(out_dir, gsub("[^A-Za-z0-9_.-]+", "-", run_label))
  reg_path <- write_destination_registry(
    dest_prep$registry, file.path(run_out_dir, "destination_registry.parquet")
  )
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: destination registry written: %s (%d rows, one per routable BPE universe row)",
      reg_path, nrow(dest_prep$registry)
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
  staged <- NULL
  transit_staged <- NULL
  if ("transit" %in% modes) {
    # The current regime is the complete namespaced routing universe. Raw
    # KORRIGOBRET is provenance/D1 input and must not be co-staged with its
    # derived feeds, or every network would be routed twice.
    transit_staged <- stage_transit_feeds(
      network_dir = net_dir,
      data_dir = data_dir,
      manifest_path = manifest_path,
      regime = "current",
      service_date = transit_date,
      required_ids = transit_required_ids,
      activity_window = transit_activity_window
    )
  }
  if ("transit" %in% modes || isTRUE(elevation_enabled)) {
    # DEM staging remains the responsibility of the legacy helper. Passing a
    # NULL GTFS path prevents it from copying the obsolete single-feed input.
    staged <- stage_full_run_inputs(net_dir, data_dir, NULL, requested_dem,
                                    require_dem = elevation_enabled)
    if (!is.null(transit_staged)) staged$transit <- transit_staged
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
  link_elevation <- if (isTRUE(elevation_enabled)) staged[["dem_path"]] else "NONE"
  net <- link_network(data_path = net_dir, elevation = link_elevation,
                      verbose = isTRUE(verbose))
  network_build_seconds <- proc.time()[["elapsed"]] - t_net0
  if (isTRUE(verbose)) {
    message(sprintf(
      "run_tracer: network built/reused in %.1f s (network.dat at %s)",
      network_build_seconds, file.path(net_dir, "network.dat")
    ))
  }

  # --- 5. chunk loop (D7/D10) ------------------------------------------------
  # Chunks iterate over UNIQUE origin coordinates (#20): each coordinate
  # routes exactly once; expansion restores every origin identity before the
  # matrix derivation.
  n_chunks <- ceiling(nrow(origin_plan$points) / chunk_size)
  run_stats <- list()
  files <- character(0)
  pair_files <- character(0)
  for (i in seq_len(n_chunks)) {
    idx <- seq.int((i - 1L) * chunk_size + 1L,
                   min(i * chunk_size, nrow(origin_plan$points)))
    origins_chunk <- origin_plan$points[idx]
    if (isTRUE(verbose)) {
      message(sprintf(
        "run_tracer: chunk %d/%d: %d routing coordinates x %d routing coordinates (destinations)",
        i, n_chunks, nrow(origins_chunk), nrow(dest_plan$points)
      ))
    }
    for (mode in modes) {
      t0 <- proc.time()[["elapsed"]]
      pairs <- if (identical(mode, "bike")) {
        route_bike_pairs(net, origins_chunk, dest_plan$points,
          max_trip_duration = max_trip_duration, bike_speed = bike_speed,
          n_threads = n_threads)
      } else if (identical(mode, "transit")) {
        route_transit_pairs(net, origins_chunk, dest_plan$points,
          departure_datetime = departure_datetime, max_trip_duration = max_trip_duration,
          walk_speed = walk_speed, n_threads = n_threads)
      } else route_pairs(net, origins_chunk, dest_plan$points,
          mode = toupper(mode), max_trip_duration = max_trip_duration,
          walk_speed = walk_speed, n_threads = n_threads)
      route_seconds <- proc.time()[["elapsed"]] - t0
      n_routed_pairs <- nrow(pairs)

      # Expansion back to identities (#20): the pairs leave this seam keyed
      # on BDNB origin ids and BPE listing ids — bit-for-bit what reference
      # (non-deduplicated) routing would have returned, sparse rows included.
      if (nrow(pairs) > 0L) {
        pairs <- expand_pairs_to_identities(pairs, origin_plan$link,
                                            dest_plan$link)
      }
      n_pairs <- nrow(pairs)

      if (identical(mode, "transit")) {
        views <- list(raw = pairs, collapsed = pairs)
        if (!is.null(pairs_out_dir)) pair_files <- c(pair_files,
          write_transit_pairs_chunk(views$raw, i, run_label, pairs_out_dir))
        rows <- derive_transit_matrix_rows(pairs, dest_prep$dest_map, mode)
        path <- write_matrix_chunk(rows, mode, i, run_out_dir)
        m <- read_matrix(path); validate_matrix(m)
        n_rows <- nrow(rows)
        run_stats[[length(run_stats) + 1L]] <- list(chunk_id=i, mode=mode,
          route_seconds=route_seconds, n_pairs=n_pairs,
          n_routed_pairs=n_routed_pairs, n_rows=n_rows, path=path)
        files <- c(files, path)
        rm(pairs, rows, m, views); gc(); rJava::.jgc(R.gc = TRUE)
        next
      }

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

      views <- route_pair_views(pairs)
      if (!is.null(pairs_out_dir)) {
        pair_files <- c(pair_files, write_route_pairs_chunk(
          views$raw, mode, i, run_label, pairs_out_dir
        ))
      }

      # Establishment-level pair pass: an establishment listed at several
      # points yields one r5r pair row per point; collapse to the
      # per-(from_id, to_id) minimum so derive_matrix_rows counts
      # establishments (derive.R), at their nearest point.
      pairs <- views$collapsed

      rows <- derive_matrix_rows(pairs, dest_prep$dest_map, mode)
      n_rows <- nrow(rows)
       path <- write_matrix_chunk(rows, mode, i, run_out_dir)
      files <- c(files, path)

      # The driver validates the assembled matrix (write_matrix_chunk's
      # contract: "the driver validates ... after the loop").
      m <- read_matrix(path)
      validate_matrix(m)

      run_stats[[length(run_stats) + 1L]] <- list(
        chunk_id = i, mode = mode, route_seconds = route_seconds,
        n_pairs = n_pairs, n_routed_pairs = n_routed_pairs,
        n_rows = n_rows, path = path
      )
      if (isTRUE(verbose)) {
        message(sprintf(
          "run_tracer: %s chunk %d: routed %d coordinate pairs in %.1f s (expanded to %d identity pairs -> %d rows); wrote %s; validated",
          mode, i, n_routed_pairs, route_seconds, n_pairs, n_rows, path
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
                         n_routed_pairs = sum(vapply(entries, `[[`, 0L, "n_routed_pairs")),
                         n_rows = sum(vapply(entries, `[[`, 0L, "n_rows")),
                         n_chunks = length(entries)
                       )
                     })
  out <- list(
    network_pbf = network_pbf,
    epci = epci,
    scope = scope_label,
    n_origins = nrow(origins),
    # Coordinate-level routing (#20): what the pair pass actually routed.
    n_origins_routed = n_origin_coords,
    n_destinations = nrow(dest_prep$destinations),
    n_destinations_routed = n_dest_coords,
    coordinate_dedup = list(
      rule = "exact (lon, lat) equality",
      expected_full_run = full_run_coordinate_counts(),
      actual_origins_input = nrow(origins),
      actual_origins_routed = n_origin_coords,
      actual_destinations_input = nrow(dest_prep$destinations),
      actual_destinations_routed = n_dest_coords
    ),
    n_routable_destinations = dest_prep$n_routable,
    n_na_coord_destinations = dest_prep$n_na_coord,
    n_map_entries = nrow(dest_prep$dest_map),
    destination_registry = reg_path,
    run_label = run_label,
    pairs_out_dir = pairs_out_dir,
    pair_files = pair_files,
    n_origins_requested = n_origins_requested,
    n_origins_selected = n_origins_selected,
    network_dir = net_dir,
    network_dat = file.path(net_dir, "network.dat"),
    network_build_seconds = network_build_seconds,
    per_mode = per_mode,
    files = files,
    java_heap = heap_line
  )
  out$departure_datetime <- departure_datetime
  out$bike_speed <- bike_speed
  out$elevation <- elevation
  out$dem_path <- if (is.null(staged)) NULL else staged[["dem_path"]]
  out$gtfs <- if (is.null(staged)) NULL else if (!is.null(staged$transit)) {
    staged$transit
  } else {
    list(path = staged[["gtfs_path"]], sha256 = staged[["gtfs_sha256"]])
  }
  out$chunk_size <- chunk_size
  out$n_threads <- n_threads
  routing_parameters <- full_run_routing_parameters(bike_speed = bike_speed,
                                                    elevation = elevation)
  routing_parameters$modes <- modes
  routing_parameters$max_trip_duration <- max_trip_duration
  routing_parameters$walk_speed <- walk_speed
  routing_parameters$transit <- list(
    time_window = 60,
    percentiles = c(1, 50),
    # Keep the POSIXct at the public API, but use an explicit string in the
    # JSON metadata object so serialization does not depend on jsonlite's
    # date options.
    departure_datetime = if (is.null(departure_datetime)) NULL else
      format(departure_datetime, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  )
  if ("transit" %in% modes) {
    routing_parameters$transit$service_date <- transit_date
    routing_parameters$transit$required_ids <- transit_required_ids
    routing_parameters$transit$feed_activity_window <- transit_activity_window
  }
  routing_parameters$scope <- scope_label
  routing_parameters$W <- W
  routing_parameters$chunk_size <- chunk_size
  routing_parameters$n_threads <- n_threads
  routing_parameters$elevation$dem_path <- out$dem_path
  out$routing_parameters <- routing_parameters
  # The cap-and-ladder contract rides in run_metadata.json (#17): the summary
  # names the authoritative cap and the exact rungs every count column came
  # from — a 30-minute artifact can never be mistaken for this run's output.
  out$cap_minutes <- cap_minutes()
  out$ladder_rungs <- unname(ladder_rungs())
  out$ladder_cols <- unname(ladder_cols())
  metadata_path <- file.path(run_out_dir, "run_metadata.json")
  dir.create(run_out_dir, recursive = TRUE, showWarnings = FALSE)
  # Metadata portability (#19): every recorded path is rewritten relative to
  # the durable checkout root before serialization — absolute worktree paths
  # in run_metadata.json were the failed attempt's dead-reference defect.
  cache_root <- durable_root_of_data_dir(data_dir)
  out <- make_metadata_portable(out, cache_root)
  jsonlite::write_json(out, metadata_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  out$metadata_path <- portable_path(metadata_path, cache_root)

  # Human-readable settlement (run-strategy §3 measurements).
  cat(sprintf(
    paste0(
      "\nrun_tracer summary (%s)\n",
      "  heap            : %s (D6)\n",
      "  origins         : %d selected / %d requested (EPCI %s)\n",
      "                    -> %d unique routing coordinates (#20 exact-coordinate dedup)\n",
      "  destinations    : %d routable BPE rows (of %d universe; %d NA-coord excluded)\n",
      "                    -> %d (id, TYPEQU) routing destinations, map %d entries\n",
      "                    -> %d unique routing coordinates (#20 exact-coordinate dedup)\n",
      "  registry        : %s (%d rows, lossless back-link to the BPE universe)\n",
      "  network build   : %.1f s (network.dat at %s)\n"
    ),
    paste(modes, collapse = " + "), heap_line, nrow(origins), n_origins_requested, epci,
    n_origin_coords,
    dest_prep$n_routable, dest_prep$n_universe, dest_prep$n_na_coord,
    nrow(dest_prep$destinations), nrow(dest_prep$dest_map),
    n_dest_coords,
    reg_path, nrow(dest_prep$registry),
    network_build_seconds, out$network_dat
  ))
  for (mode in modes) {
    st <- per_mode[[mode]]
    cat(sprintf(
      "  %-4s             : routed in %.1f s; r5r returned %d coordinate pairs -> %d identity pairs -> %d matrix rows (%d chunk%s)\n",
      mode, st$route_seconds, st$n_routed_pairs, st$n_pairs, st$n_rows,
      st$n_chunks, if (st$n_chunks == 1L) "" else "s"
    ))
  }
  cat(sprintf("  parquet files   : %s\n", paste(files, collapse = ", ")))
  if (length(pair_files) > 0L) {
    cat(sprintf("  pair sidecars   : %s\n", paste(pair_files, collapse = ", ")))
  }

  invisible(out)
}
