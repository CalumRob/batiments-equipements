# 22g-measure-transit10k.R — REAL-SCALE marginal-cost measurement (#22
# follow-up, maintainer challenge 2026-08-25: "no way this is 243h").
#
# The probe's ~243 h figure extrapolated linearly from 200-origin chunks,
# where fixed per-call costs dominate. The legacy routed 100k-origin chunks
# overnight on this box. This script routes ONE 10 000-origin transit chunk
# against the full destination universe through run_resumable — production
# path, max_rides = 2 — so per-origin cost is measured at real scale and
# the release estimate rests on data, not extrapolation.
#
# Run detached from E:\batiments-equipements:
#   Start-Process Rscript -ArgumentList '.scratch/.../22g-measure-transit10k.R' ...

options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE)))
  source(f)

MAIN <- "E:/batiments-equipements"
DATA <- file.path(MAIN, "data")
NET_DIR <- file.path(DATA, "networks", "cap20-current")
OUT <- file.path(MAIN, ".scratch", "tracer-matrice", "research", "outputs")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
utc_stamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
write_json_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (!file.rename(tmp, path)) { file.copy(tmp, path, overwrite = TRUE); unlink(tmp) }
}

cat("== #22g: real-scale transit measurement (10k origins x 112k dests) ==\n")
cat("heap:", getOption("java.parameters"), "| rJava loaded:",
    "rJava" %in% loadedNamespaces(), "\n")

# staging + identity (idempotent; network must cache-HIT)
block <- stage_transit_feeds(NET_DIR, data_dir = DATA,
                             manifest_path = file.path(DATA, "manifest.json"))
cat(sprintf("staged %d feeds\n", block[["n_feeds"]]))
crop <- read_osm_network(data_dir = DATA,
                         manifest_path = file.path(DATA, "manifest.json"))
osm_pin <- list(id = basename(crop), sha256 = sha256_file(crop))
staged_dem <- stage_full_run_inputs(NET_DIR, data_dir = DATA, gtfs_path = NULL,
                                    dem_path = NULL, require_dem = TRUE)
elevation_pin <- list(id = "srtm_bretagne.tif",
                      sha256 = sha256_file(staged_dem[["dem_path"]]))
identity <- network_cache_identity(osm_pin, block[["feeds"]],
                                   elevation_pin = elevation_pin)
probe <- probe_network_cache(NET_DIR, identity)
if (!isTRUE(probe$cache_hit)) {
  stop("network not committed - build first (22b): ", probe$reason, call. = FALSE)
}
cat("network cache HIT\n")

# 10k systematic spread over the FULL universe plan (k=142 over 1424208)
bdnb <- read_bdnb_residential_universe(
  departements = c("22", "29", "35", "56"), data_dir = DATA,
  manifest_path = file.path(DATA, "manifest.json"), use_cache = TRUE)
sf_pts <- sf::st_as_sf(as.data.frame(bdnb), coords = c("x_2154", "y_2154"),
                       crs = 2154L, remove = FALSE)
sf_pts <- sf::st_transform(sf_pts, 4326L)
xy <- as.data.frame(sf::st_coordinates(sf_pts))
origins <- data.table::data.table(id = bdnb[["origin_id"]],
                                  lon = xy[["X"]], lat = xy[["Y"]])
ok <- !is.na(origins$lon) & !is.na(origins$lat)
origins <- origins[ok]
op <- coordinate_routing_plan(origins, prefix = "coord_o")
N <- nrow(op$points)
n_target <- 10000L
k <- floor(N / n_target)
idx <- seq.int(1L, by = k, length.out = n_target)
stopifnot(max(idx) <= N)
sample_pts <- op$points[idx]
sel_link <- op$link[op$link$point_id %in% sample_pts$id]
first_id <- sel_link[!duplicated(sel_link$point_id)]
sample <- merge(first_id[, .(id, point_id)],
                sample_pts[, .(point_id = id, lon, lat)],
                by = "point_id")
sample[, point_id := NULL]
cat(sprintf("sampled %d origins (N=%d coords, k=%d)\n",
            nrow(sample), N, k))

git_sha <- suppressWarnings(system2("git", c("rev-parse", "HEAD"),
                                    stdout = TRUE, stderr = FALSE))
dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")

t0 <- proc.time()[["elapsed"]]
res <- run_resumable(
  run_label = "gate-transit-10k",
  modes = "transit",
  chunk_size = 10000L,
  W = border_width_m(),
  walk_speed = 4,
  bike_speed = 12,
  max_trip_duration = cap_minutes(),
  elevation = "TOBLER",
  departure_datetime = dep,
  time_window = 60L,
  percentiles = c(1L, 50L),
  max_rides = max_transit_rides(),
  n_threads = 8,    # operator working alongside - leave half the box   # 16 logical cores - 2: keep the box responsive during test phases
  network_identity = identity,
  network_dir = NET_DIR,
  heap = "-Xmx24G",
  data_dir = DATA,
  out_dir = file.path(DATA, "matrice"),
  origins_provider = function() sample,
  git_sha = as.character(git_sha),
  verbose = TRUE
)
wall_s <- proc.time()[["elapsed"]] - t0

m <- jsonlite::fromJSON(file.path(DATA, "matrice", "gate-transit-10k",
                                  "manifest.json"), simplifyVector = FALSE)
e <- m$entries[[1]]
out <- list(
  recorded_at = utc_stamp(),
  purpose = "real-scale transit marginal cost - replaces the retracted 243h linear extrapolation",
  n_origins = nrow(sample),
  chunk_size = 10000L,
  route_seconds = e$route_seconds,
  s_per_origin_route = e$route_seconds / nrow(sample),
  wall_seconds_total = wall_s,
  n_routed_pairs = e$n_routed_pairs,
  n_identity_pairs = e$n_identity_pairs,
  n_rows = e$n_rows,
  max_rides = max_transit_rides(),
  n_threads = 8,    # operator working alongside - leave half the box   # 16 logical cores - 2: keep the box responsive during test phases
  departure_utc = "2026-08-26T05:00:00+0000",
  fingerprint = identity$fingerprint,
  git_sha = as.character(git_sha)
)
write_json_atomic(out, file.path(OUT, "22g-transit-10k.json"))
cat(sprintf("\nMEASURED: %d origins in %.1f s routing (%.4f s/origin)\n",
            nrow(sample), e$route_seconds, e$route_seconds / nrow(sample)))
cat(sprintf("release projection at this scale: %.1f h for 1424208 coords\n",
            e$route_seconds / nrow(sample) * 1424208 / 3600))
cat("phase G complete\n")
