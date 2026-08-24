# 22e-run-probe.R — Phase E of the #22 gate: the 1 000-origin four-mode probe
# through run_resumable (the #21 orchestrator) against the FULL BPE universe,
# cap-20 contract. Launched DETACHED (sequential children, one 24G JVM at a
# time — the orchestrator enforces this); the manifest persists after every
# chunk, so a fresh dispatch re-invoking run_resumable with the SAME arguments
# resumes surgically by construction.
#
# Run detached from the worktree root:
#   Start-Process Rscript -ArgumentList '.scratch/.../22e-run-probe.R' `
#     -WorkingDirectory <wt> -RedirectStandardOutput .../22e-run.log ...

options(java.parameters = "-Xmx24G")   # children set their own; harmless here
source(".scratch/tracer-matrice/research/22-common.R")

cat("== #22 phase E: four-mode cap-20 probe through run_resumable ==\n")

sample_path <- file.path(matrice_run_dir(OUT_DIR, "cap20-probe-1000"),
                         "origin_sample.parquet")
stopifnot(file.exists(sample_path))
sample <- data.table::as.data.table(arrow::read_parquet(sample_path))
cat(sprintf("origin sample loaded: %d identities\n", nrow(sample)))

prep <- probe_identity()
identity <- prep$identity

probe <- probe_network_cache(NET_DIR, identity)
if (!isTRUE(probe$cache_hit)) {
  stop("the probe network is not built/committed — run phase B first (",
       probe$reason, ")", call. = FALSE)
}
cat("network cache: HIT —", probe$reason, "\n")

git_sha <- current_git_sha()
cat("code provenance:", git_sha, "\n")

t0 <- proc.time()[["elapsed"]]
res <- run_resumable(
  run_label = "cap20-probe-1000",
  modes = atomic_modes(),
  chunk_size = PROBE$chunk_size,
  W = border_width_m(),
  walk_speed = PROBE$walk_speed,
  bike_speed = PROBE$bike_speed,
  max_trip_duration = cap_minutes(),
  elevation = PROBE$elevation_setting,
  departure_datetime = probe_departure(),
  time_window = PROBE$time_window,
  percentiles = PROBE$percentiles,
  network_identity = identity,
  network_dir = NET_DIR,
  heap = PROBE$heap,
  data_dir = DATA,
  out_dir = OUT_DIR,
  origins_provider = function() sample,
  git_sha = git_sha
)
wall_seconds <- proc.time()[["elapsed"]] - t0

summary <- list(
  recorded_at = utc_stamp(),
  manifest_path = "data/matrice/cap20-probe-1000/manifest.json",
  complete = res$complete,
  n_complete = res$n_complete,
  n_failed = res$n_failed,
  n_pending = res$n_pending,
  spawned_chunks = res$spawned_chunks,
  wall_seconds_this_invocation = wall_seconds,
  fingerprint = identity$fingerprint,
  parameters = PROBE
)
write_json_atomic_local(summary, file.path(OUTPUTS, "22-run-summary.json"))
str(res)
cat("phase E complete\n")
