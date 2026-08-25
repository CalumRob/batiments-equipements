# 22a-stage-identity.R — Phase A of the #22 gate: stage the transit feeds
# (default "current" regime per the maintainer's pre-dispatch ruling), pin the
# OSM crop + assembled DEM, compute the network cache identity, and record the
# FIRST probe result (expected MISS: no identity marker committed yet).
#
# Run: Rscript .scratch/tracer-matrice/research/22a-stage-identity.R  (cwd = worktree root)

source(".scratch/tracer-matrice/research/22-common.R")

cat("== #22 phase A: staging + identity ==\n")
prep <- probe_identity()
identity <- prep$identity

probe1 <- probe_network_cache(NET_DIR, identity)
cat(sprintf("first probe: cache_hit=%s (%s)\n", probe1$cache_hit, probe1$reason))

# --- machine outputs ---------------------------------------------------------

feeds_tbl <- data.table::rbindlist(lapply(prep$block[["feeds"]], function(f) {
  data.table::data.table(
    regime = prep$block[["regime"]], id = f[["id"]], sha256 = f[["sha256"]],
    role = f[["role"]], prefix = ifelse(is.null(f[["prefix"]]), "", f[["prefix"]]),
    staged_file = f[["staged_file"]])
}))
data.table::fwrite(feeds_tbl, file.path(OUTPUTS, "22-staged-feeds.csv"))

state <- list(
  recorded_at = utc_stamp(),
  network_dir = PROBE$network_dir,
  regime = prep$block[["regime"]],
  n_feeds = prep$block[["n_feeds"]],
  osm_pin = prep$osm_pin,
  elevation_pin = prep$elevation_pin,
  dem_path = "data/networks/cap20-current/srtm_bretagne.tif",
  fingerprint = identity$fingerprint,
  canonical_lines = network_identity_canonical_lines(identity$components),
  r5r_versions = r5r_runtime_versions(),
  first_probe = list(cache_hit = probe1$cache_hit, reason = probe1$reason),
  parameters = PROBE
)
write_json_atomic_local(state, file.path(OUTPUTS, "22-staging-state.json"))
write_json_atomic_local(state, file.path(NET_DIR, "staging-state.json"))

cat("fingerprint:", state$fingerprint, "\n")
cat("phase A complete\n")
