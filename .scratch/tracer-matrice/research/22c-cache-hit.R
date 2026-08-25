# 22c-cache-hit.R — Phase C of the #22 gate: prove durable reuse. A SECOND
# independent invocation recomputes the identity from the staged bytes and
# probes the network directory: expected cache HIT with matching fingerprints
# (no rebuild). Also times the cached network.dat reload — the per-child
# startup cost the chunk-size verdict needs — and verifies that a child's
# elevation="NONE" loader call does NOT trigger a rebuild (network.dat mtime
# unchanged).
#
# D6: heap before any rJava load (the reload half of this script loads r5r).
options(java.parameters = "-Xmx24G")
source(".scratch/tracer-matrice/research/22-common.R")

cat("== #22 phase C: cache-hit proof ==\n")
prep <- probe_identity()
identity <- prep$identity

probe <- probe_network_cache(NET_DIR, identity)
cat(sprintf("second-invocation probe: cache_hit=%s | reason=%s\n",
            probe$cache_hit, probe$reason))
cat("expected:", identity$fingerprint, "\n")
cat("found   :", if (isTRUE(probe$cache_hit)) probe$found_fingerprint else NA, "\n")

dat <- file.path(NET_DIR, "network.dat")
mtime_before <- file.info(dat)[["mtime"]]

# The reload a production child performs: default_network_loader's call shape
# (elevation falls back to "NONE" because requests carry dem_path = NULL).
t0 <- proc.time()[["elapsed"]]
net <- link_network(data_path = NET_DIR, elevation = "NONE", verbose = FALSE)
reload_seconds_none <- proc.time()[["elapsed"]] - t0
r5r::stop_r5(net)

t0 <- proc.time()[["elapsed"]]
net2 <- link_network(data_path = NET_DIR, elevation = prep$dem_path,
                     verbose = FALSE)
reload_seconds_dem <- proc.time()[["elapsed"]] - t0
r5r::stop_r5(net2)

mtime_after <- file.info(dat)[["mtime"]]
rebuilt <- !identical(mtime_before, mtime_after)

out <- list(
  recorded_at = utc_stamp(),
  cache_hit = probe$cache_hit,
  reason = probe$reason,
  expected_fingerprint = probe$expected_fingerprint,
  found_fingerprint = if (is.null(probe$found_fingerprint)) NULL else probe$found_fingerprint,
  reload_seconds_loader_none = reload_seconds_none,
  reload_seconds_with_dem_arg = reload_seconds_dem,
  network_dat_rebuilt_during_reload = rebuilt,
  network_dat_bytes = file.info(dat)[["size"]]
)
write_json_atomic_local(out, file.path(OUTPUTS, "22-cache-hit.json"))
cat(sprintf("reload (child 'NONE' call): %.1f s | with DEM arg: %.1f s | rebuilt: %s\n",
            reload_seconds_none, reload_seconds_dem, rebuilt))
cat("phase C complete\n")
