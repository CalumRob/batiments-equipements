# Shared fixtures for the #21 resumable-runner tests: a miniature but
# structurally-faithful run (tiny BDNB/BPE-style universes, real
# coordinate_routing_plan cuts, persisted plan parquets) plus SCRIPTED FAKE
# CHILDREN that drive the manifest state machine without any Rscript/JVM.
#
# The scripted children execute the REAL run_chunk_worker pipeline in-process
# (router-injected, stub_route_pairs precedent) and then apply one deliberate
# damage: crash before writing, receipt-without-artifact,
# artifact-without-receipt, corrupted bytes.

stub_mode_dispatch <- function(cap = 20, base_minutes = 2) {
  function(network, origins, destinations, mode) {
    if (identical(mode, "transit")) {
      sp <- stub_route_pairs(origins, destinations, cap = cap,
                             base_minutes = base_minutes)
      sp[, travel_time_p1 := travel_time]
      sp[, travel_time_p50 := travel_time + 5]
      sp[, .(from_id, to_id, travel_time_p1, travel_time_p50)]
    } else {
      stub_route_pairs(origins, destinations, cap = cap,
                       base_minutes = base_minutes)
    }
  }
}

#' A miniature run layout under a fake durable checkout root:
#'   <root>/data/matrice/<label>/{plan,chunks,receipts,requests}
#' Returns the universes, both routing plans, the frozen census and every
#' directory the orchestrator and children agree on.
fixture_run_layout <- function(label = "fixture-run",
                               root = tempfile("run-root-"),
                               chunk_size = 2L) {
  origins <- data.table::data.table(
    id = c("b1a", "bx", "b1b", "b2", "b3"),
    lon = c(-1.35, -1.38, -1.35, -1.42, -2.00),
    lat = c(48.11, 48.12, 48.11, 48.13, 48.60)
  )
  dests <- data.table::data.table(
    id = sprintf("bpe_listing_%06d", 1:3),
    lon = c(-1.35, -1.35, -1.40),
    lat = c(48.10, 48.10, 48.12),
    TYPEQU = c("B104", "D265", "B204")
  )
  op <- coordinate_routing_plan(origins, prefix = "coord_o")     # 4 unique coords
  dp <- coordinate_routing_plan(dests[, .(id, lon, lat)],
                                prefix = "coord_d")              # 2 unique coords
  dest_map <- dests[, .(id, TYPEQU)]

  run_dir <- file.path(root, "data", "matrice", label)
  plan_dir <- file.path(run_dir, "plan")
  dir.create(plan_dir, recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(op$points, file.path(plan_dir, "origin_points.parquet"))
  arrow::write_parquet(op$link, file.path(plan_dir, "origin_link.parquet"))
  arrow::write_parquet(dp$points, file.path(plan_dir, "destination_points.parquet"))
  arrow::write_parquet(dp$link, file.path(plan_dir, "destination_link.parquet"))
  arrow::write_parquet(dest_map, file.path(plan_dir, "destination_map.parquet"))

  list(
    root = root,
    data_dir = file.path(root, "data"),
    run_dir = run_dir,
    plan_dir = plan_dir,
    chunks_dir = file.path(run_dir, "chunks"),
    receipts_dir = file.path(run_dir, "receipts"),
    requests_dir = file.path(run_dir, "requests"),
    manifest_path = file.path(run_dir, "manifest.json"),
    origins = origins, dests = dests, dest_map = dest_map,
    origin_plan = op, dest_plan = dp,
    chunk_size = chunk_size,
    census = plan_census(chunk_size = chunk_size,
                         n_origins = nrow(origins),
                         n_origin_coords = nrow(op$points),
                         n_destinations = nrow(dests),
                         n_dest_coords = nrow(dp$points))
  )
}

#' Build a chunk request the way the orchestrator does (paths into the layout).
fixture_chunk_request <- function(fx, chunk_id, modes = c("walk", "car"),
                                  heap = "-Xmx24G") {
  list(
    kind = "matrice-chunk-request",
    request_version = 1L,
    run_label = basename(fx$run_dir),
    code_dir = "code",
    heap = heap,
    network_dir = file.path(fx$data_dir, "networks", "current"),
    chunk_id = as.integer(chunk_id),
    chunk_size = as.integer(fx$chunk_size),
    n_origin_coords = nrow(fx$origin_plan$points),
    modes = modes,
    paths = list(
      origin_points = file.path(fx$plan_dir, "origin_points.parquet"),
      origin_link = file.path(fx$plan_dir, "origin_link.parquet"),
      destination_points = file.path(fx$plan_dir, "destination_points.parquet"),
      destination_link = file.path(fx$plan_dir, "destination_link.parquet"),
      destination_map = file.path(fx$plan_dir, "destination_map.parquet"),
      artifacts_dir = fx$chunks_dir,
      receipts_dir = fx$receipts_dir
    ),
    routing = list(
      walk_speed = 4,
      bike_speed = 12,
      max_trip_duration = cap_minutes(),
      elevation = "NONE",
      dem_path = NULL,
      departure_datetime = NULL,
      time_window = 60L,
      percentiles = c(1L, 50L),
      n_threads = NULL   # NULL means r5r's default (Inf)
    )
  )
}

#' Scripted fake children: a spawn_child replacement whose behaviour per
#' chunk_id comes from `script` (default "ok"). Each invocation parses the
#' request, executes the REAL worker pipeline with the stub dispatch (unless
#' the script crashes first) and applies its damage afterwards.
#' Returns the spawn function; `$calls` (environment) records one entry per
#' spawn: chunk_id, modes, status.
scripted_spawn <- function(script = list(), router = NULL,
                           corrupt_mode = "walk") {
  calls <- new.env(parent = emptyenv())
  calls$chunks <- character(0)
  force(script); force(router); force(corrupt_mode)
  function(bootstrap_path, request_path, ...) {
    req <- chunk_request_load(request_path)
    cid <- as.integer(req$chunk_id)
    behaviour <- if (!is.null(script[[as.character(cid)]])) {
      script[[as.character(cid)]]
    } else "ok"
    calls$chunks <- c(calls$chunks, sprintf("%s:%s", cid, behaviour))

    if (identical(behaviour, "crash-before-write")) {
      return(list(status = 3L, stdout = "", stderr = "simulated JVM death"))
    }

    r <- if (is.null(router)) stub_mode_dispatch() else router
    out <- run_chunk_worker(req, router = r, network = NULL)

    if (identical(behaviour, "receipt-without-artifact")) {
      for (mode in req$modes) {
        p <- file.path(req$paths$artifacts_dir,
                       sprintf("%s_%d.parquet", mode, cid))
        if (file.exists(p)) unlink(p)
      }
    } else if (identical(behaviour, "artifact-without-receipt")) {
      for (mode in req$modes) {
        rp <- chunk_receipt_path(req$paths$receipts_dir, mode, cid)
        if (file.exists(rp)) unlink(rp)
      }
    } else if (identical(behaviour, "corrupted-bytes")) {
      p <- file.path(req$paths$artifacts_dir,
                     sprintf("%s_%d.parquet", corrupt_mode, cid))
      cat("JUNK-BYTES", file = p, append = TRUE)
    }
    list(status = 0L, stdout = "", stderr = "")
  }
}

#' Access the recorded spawn log of a scripted_spawn() child factory:
#' spawn_calls(spy)$chunks is c("<chunk_id>:<behaviour>", ...) in order.
spawn_calls <- function(spawn_fn) environment(spawn_fn)$calls

#' Standard orchestrator arguments against a fixture layout.
fixture_run_args <- function(fx, modes = c("walk", "car"),
                             fingerprint = strrep("a", 64),
                             git_sha = strrep("7", 40), chunk_size = NULL) {
  list(
    run_label = basename(fx$run_dir),
    modes = modes,
    chunk_size = if (is.null(chunk_size)) fx$chunk_size else chunk_size,
    network_identity = list(fingerprint = fingerprint, components = NULL),
    network_dir = file.path(fx$data_dir, "networks", "current"),
    origins_provider = function() fx$origins,
    destinations_provider = function() list(
      destinations = fx$dests[, .(id, lon, lat)],
      dest_map = fx$dest_map,
      registry = fx$dests),
    git_sha = git_sha,
    data_dir = fx$data_dir,
    out_dir = file.path(fx$root, "data", "matrice")
  )
}
