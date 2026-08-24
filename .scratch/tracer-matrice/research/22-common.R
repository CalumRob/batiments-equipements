# 22-common.R — shared prelude for every #22 gate script.
#
# Ticket #22 — cap-20 performance + partial-artifact gate. All REAL staging,
# network build, probe and classification artifacts land in the MAIN
# checkout's durable data tree (E:/batiments-equipements/data) by ABSOLUTE
# path; this worktree contributes only code + research evidence.
#
# House rules honoured here: no renv:: anywhere; data.table [[ extraction;
# UTC ISO-8601 stamps; no new packages.

WT <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
MAIN <- "E:/batiments-equipements"
DATA <- file.path(MAIN, "data")                      # durable root's data dir
DURABLE_ROOT <- MAIN                                 # durable_root_of_data_dir(DATA)
OUT_DIR <- file.path(DATA, "matrice")                # durable matrix root
NET_DIR <- file.path(DATA, "networks", "cap20-current")
RESEARCH <- file.path(WT, ".scratch", "tracer-matrice", "research")
OUTPUTS <- file.path(RESEARCH, "outputs")

for (f in sort(list.files(file.path(WT, "code", "R"),
                          pattern = "[.]R$", full.names = TRUE))) {
  source(f)
}
dir.create(OUTPUTS, recursive = TRUE, showWarnings = FALSE)

# --- probe-frozen parameters (recorded verbatim in run metadata) ------------

PROBE <- list(
  ticket = "22",
  network_dir = "data/networks/cap20-current",
  transit_regime = "current",           # stage_transit_feeds default regime
  departure_utc = "2026-09-15T06:00:00+0000",   # 08:00 Europe/Paris, term-time Tuesday (D2 of #25)
  departure_paris_label = "2026-09-15 08:00 Europe/Paris (morning peak)",
  W_m = border_width_m(),               # 25000
  cap_minutes = cap_minutes(),          # 20
  walk_speed = 4,
  bike_speed = 12,
  time_window = 60L,
  percentiles = c(1L, 50L),
  elevation_setting = "TOBLER",         # ADR-0001: native elevation via staged DEM
  chunk_size = 200L,                    # 5 chunks x 4 modes = 20 entries
  n_origins_target = 1000L,
  heap = "-Xmx24G"
)

#' Departure as a POSIXct in UTC (transit requires one non-NA value).
probe_departure <- function() {
  as.POSIXct("2026-09-15 06:00:00", tz = "UTC")
}

#' Recompute the network cache identity from the STAGED BYTES in NET_DIR:
#' idempotent re-staging (copies are skipped when byte-identical), DEM pin
#' over the assembled raster, OSM pin over the keyed Bretagne crop.
probe_identity <- function(verbose = TRUE) {
  say <- function(...) if (verbose) message(...)
  # 1. Transit feeds of the default ("current") regime, integrity-gated.
  block <- stage_transit_feeds(NET_DIR, data_dir = DATA,
                               manifest_path = file.path(DATA, "manifest.json"))
  say(sprintf("staged %d feeds (%s regime) into %s",
              block[["n_feeds"]], block[["regime"]], NET_DIR))
  # 2. The pinned Bretagne crop (merge cache hit; crop cache hit): copied into
  #    the network directory so setup_r5 sees exactly ONE .osm.pbf.
  crop <- read_osm_network(data_dir = DATA,
                           manifest_path = file.path(DATA, "manifest.json"))
  stopifnot(file.exists(crop))
  target_pbf <- file.path(NET_DIR, basename(crop))
  if (!file.exists(target_pbf)) {
    ok <- file.copy(crop, target_pbf, overwrite = FALSE)
    if (!isTRUE(ok)) stop("could not copy the crop into the network dir",
                          call. = FALSE)
  }
  if (!identical(sha256_file(target_pbf), sha256_file(crop))) {
    stop("staged crop pbf differs from the pinned cache entry", call. = FALSE)
  }
  osm_pin <- list(id = basename(crop), sha256 = sha256_file(crop))
  # 3. DEM assembly from the cached SRTM tiles (skips when the merged tif
  #    already sits in the network dir); validated before it can be pinned.
  staged_dem <- stage_full_run_inputs(
    NET_DIR, data_dir = DATA, gtfs_path = NULL,
    dem_path = NULL, require_dem = TRUE)
  dem_tif <- staged_dem[["dem_path"]]
  elevation_pin <- list(id = "srtm_bretagne.tif", sha256 = sha256_file(dem_tif))
  # 4. Identity over every input that can alter the built network.
  identity <- network_cache_identity(osm_pin, block[["feeds"]],
                                     elevation_pin = elevation_pin)
  list(block = block, crop = crop, osm_pin = osm_pin,
       dem_path = dem_tif, elevation_pin = elevation_pin,
       identity = identity)
}

utc_stamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

write_json_atomic_local <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE); unlink(tmp)
  }
  invisible(path)
}
