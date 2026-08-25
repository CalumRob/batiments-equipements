# 22b-build-network.R — Phase B of the #22 gate: the release-rehearsal network
# build. SELF-CONTAINED so it survives this agent session: launched detached,
# it probes the cache; on MISS it builds ONCE (r5r 2.4.0, elevation ON via the
# staged DEM, W = 25 km, cap = 20) and COMMITS the identity marker itself.
# A fresh dispatch re-running this script resumes safely by construction:
# HIT -> no rebuild.
#
# D6: the -Xmx24G heap is set BEFORE anything that could load rJava/r5r.
#
# Run detached from the worktree root:
#   Start-Process Rscript -ArgumentList '.scratch/.../22b-build-network.R' `
#     -WorkingDirectory <wt> -RedirectStandardOutput .../22b-build.log ...

options(java.parameters = "-Xmx24G")   # FIRST — before any rJava load

source(".scratch/tracer-matrice/research/22-common.R")

cat("== #22 phase B: network build ==\n")
cat("heap:", getOption("java.parameters"),
    "| rJava loaded:", "rJava" %in% loadedNamespaces(), "\n")

prep <- probe_identity()
identity <- prep$identity

probe <- probe_network_cache(NET_DIR, identity)
cat(sprintf("probe before build: cache_hit=%s (%s)\n", probe$cache_hit,
            probe$reason))
if (isTRUE(probe$cache_hit)) {
  cat("network already built and committed — nothing to do\n")
  quit(save = "no", status = 0L)
}

# A stale/partial network.dat from an interrupted earlier attempt must not be
# trusted (the marker was never committed): rebuild deliberately over it.
stale <- file.path(NET_DIR, "network.dat")
if (file.exists(stale)) {
  cat("removing uncommitted network.dat from an interrupted build\n")
  unlink(stale)
}

t0 <- proc.time()[["elapsed"]]
net <- link_network(data_path = NET_DIR, elevation = prep$dem_path,
                    verbose = TRUE)
build_seconds <- proc.time()[["elapsed"]] - t0

tryCatch(r5r::stop_r5(net), error = function(e)
  cat("stop_r5 note:", conditionMessage(e), "\n"))

marker <- commit_network_cache(NET_DIR, identity)
dat_size <- file.info(file.path(NET_DIR, "network.dat"))[["size"]]
settings <- jsonlite::fromJSON(file.path(NET_DIR, "network_settings.json"),
                               simplifyVector = FALSE)

log <- list(
  recorded_at = utc_stamp(),
  build_seconds = build_seconds,
  build_hours = build_seconds / 3600,
  marker_path = "data/networks/cap20-current/.network-identity.json",
  network_dat_bytes = dat_size,
  network_settings = settings,
  r5r_versions = r5r_runtime_versions(),
  fingerprint = identity$fingerprint,
  elevation_pin = prep$elevation_pin,
  osm_pin = prep$osm_pin,
  n_transit_feeds = prep$block[["n_feeds"]],
  regime = prep$block[["regime"]],
  heap = "-Xmx24G"
)
write_json_atomic_local(log, file.path(OUTPUTS, "22-build-log.json"))
write_json_atomic_local(log, file.path(NET_DIR, "build-log.json"))
cat(sprintf("BUILD COMPLETE in %.1f s (%.2f h); network.dat %.1f MB; marker %s\n",
            build_seconds, build_seconds / 3600, dat_size / 1048576, marker))
quit(save = "no", status = 0L)
