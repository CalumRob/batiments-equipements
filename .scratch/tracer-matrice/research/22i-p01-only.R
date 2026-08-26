# 22i-p01-only.R — the maintainer's point, tested directly: the legacy
# routed TRANSIT as one-hour-window -> p01 ONLY (linking_logic.R:
# time_window=60, percentiles=1, r5r 2.3.0). If requesting the first
# percentile alone lets R5 skip distribution sampling entirely, the
# legacy-faithful call shape is dramatically cheaper than anything we
# benchmarked — and #23 faces a real contract question (D3 stores p1+p50).
#
# Variants: our current shape (w60 dpm1 p(1,50)) vs p01-only at dpm1 and
# dpm5 — if dpm stops mattering under p01, the draws are indeed the
# distribution machinery, not the search.

suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"

net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)
pts <- as.data.table(arrow::read_parquet(file.path(RD, "plan/origin_points.parquet")))
dps <- as.data.table(arrow::read_parquet(file.path(RD, "plan/destination_points.parquet")))
oc <- pts[1:50]
dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")
NT <- 8

timed <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  r <- tryCatch(force(expr), error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
  s <- proc.time()[["elapsed"]] - t0
  n <- if (is.null(r)) NA_integer_ else nrow(as.data.table(r))
  data.table(variant = label, seconds = round(s, 1), rows = n,
             s_per_origin = round(s / nrow(oc), 3))
}

rows <- list()
rows[[1]] <- timed("TT w60 dpm1 p(1,50) [current shape]", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 20, walk_speed = 4,
  time_window = 60, percentiles = c(1, 50), draws_per_minute = 1,
  max_rides = 2L, n_threads = NT))
rows[[2]] <- timed("TT w60 dpm1 p01 ONLY [legacy-faithful]", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 20, walk_speed = 4,
  time_window = 60, percentiles = 1, draws_per_minute = 1,
  max_rides = 2L, n_threads = NT))
rows[[3]] <- timed("TT w60 dpm5 p01 ONLY", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 20, walk_speed = 4,
  time_window = 60, percentiles = 1, draws_per_minute = 5,
  max_rides = 2L, n_threads = NT))

out <- rbindlist(rows, use.names = TRUE, fill = TRUE)
print(out)
fwrite(out, ".scratch/tracer-matrice/research/outputs/22i-p01-benchmark.csv")
