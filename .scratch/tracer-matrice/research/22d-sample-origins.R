# 22d-sample-origins.R — Phase D of the #22 gate: draw the 1 000-origin probe
# sample. MAINTAINER RULING (brief 2026-08-24): a SYSTEMATIC SPREAD over the
# coordinate_routing_plan order of the FULL bretagne-scope residential
# universe — every k-th unique routing coordinate — NEVER the first 1 000
# (plan order clusters by commune).
#
# Recipe (recorded verbatim in the outputs): N = number of unique origin
# coordinates in plan order; k = floor(N / 1000); indices = seq(1, by = k,
# length.out = 1000); each selected coordinate contributes ONE representative
# BDNB identity (its first in link order — routing is a pure function of the
# coordinate, so the choice of representative is immaterial and recorded).

source(".scratch/tracer-matrice/research/22-common.R")

cat("== #22 phase D: deterministic systematic origin sample ==\n")
run_dir <- matrice_run_dir(OUT_DIR, "cap20-probe-1000")
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

t0 <- proc.time()[["elapsed"]]
all_origins <- bdnb_origin_coordinates(
  scope = "bretagne", data_dir = DATA,
  manifest_path = file.path(DATA, "manifest.json"))[["origins"]]
cat(sprintf("full universe: %d identities (%.1f s)\n", nrow(all_origins),
            proc.time()[["elapsed"]] - t0))

op <- coordinate_routing_plan(all_origins, prefix = "coord_o")
N <- nrow(op$points)
k <- floor(N / PROBE$n_origins_target)
idx <- seq.int(1L, by = k, length.out = PROBE$n_origins_target)
stopifnot(max(idx) <= N, length(idx) == PROBE$n_origins_target, k >= 1L)

sampled_points <- op$points[idx]
link <- op$link
# First identity per selected point_id, in link order (input order):
sel_link <- link[link[["point_id"]] %in% sampled_points[["id"]]]
first_identity <- sel_link[!duplicated(sel_link[["point_id"]])]
sample <- merge(
  first_identity[, .(id, point_id)],
  sampled_points[, .(point_id, lon, lat)],
  by = "point_id")
sample[, point_id := NULL]
setcolorder(sample, c("id", "lon", "lat"))
stopifnot(nrow(sample) == PROBE$n_origins_target,
          !anyDuplicated(sample[["id"]]),
          !anyDuplicated(data.table::data.table(lon = sample[["lon"]],
                                                lat = sample[["lat"]])))

sample_path <- file.path(run_dir, "origin_sample.parquet")
arrow::write_parquet(as.data.frame(sample), sample_path)

recipe <- list(
  recorded_at = utc_stamp(),
  rule = paste("systematic spread over coordinate_routing_plan order:",
               "k = floor(N/1000), indices = seq(1, by = k, length.out = 1000),",
               "one representative identity per selected coordinate",
               "(first in link order)"),
  n_universe_identities = nrow(all_origins),
  n_universe_unique_coordinates = N,
  k = k,
  start_index = 1L,
  stride = k,
  n_sampled = nrow(sample),
  sample_parquet = "data/matrice/cap20-probe-1000/origin_sample.parquet",
  sample_sha256 = sha256_file(sample_path),
  universe_key_full = bdnb_universe_key(all_origins),
  first_10_ids = head(sample[["id"]], 10L)
)
write_json_atomic_local(recipe, file.path(OUTPUTS, "22-origin-sample-recipe.json"))

data.table::fwrite(sample, file.path(OUTPUTS, "22-origin-sample.csv"))
cat(sprintf("sampled %d origins (N=%d coords, k=%d) -> %s\n", nrow(sample), N,
            k, sample_path))
cat("phase D complete\n")
