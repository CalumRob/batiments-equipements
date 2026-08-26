# 22h-window-benchmark.R — where does transit routing time actually go?
#
# Context: real-scale measurement gave 0.326 s/origin (8 threads, 10k
# chunk). Legacy did the same job at ~25 ms/origin effective. Suspect:
# draws_per_minute (r5r 2.4.0 default 5 -> 300 departure draws per origin
# under the 60-min window). This benchmark times the exact probe call
# shape against sampling variants AND the legacy's accessibility() API,
# one JVM, fixed 8 threads, so every delta is attributable.

suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"

net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)
pts <- as.data.table(arrow::read_parquet(file.path(RD, "plan/origin_points.parquet")))
dps <- as.data.table(arrow::read_parquet(file.path(RD, "plan/destination_points.parquet")))
oc <- pts[1:50]                       # 50 real sampled origins
dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")
NT <- 8                               # match the 22g measurement conditions

timed <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  r <- tryCatch(force(expr), error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
  s <- proc.time()[["elapsed"]] - t0
  n <- if (is.null(r)) NA_integer_ else nrow(as.data.table(r))
  data.table(variant = label, seconds = round(s, 1), rows = n,
             s_per_origin = round(s / nrow(oc), 3))
}

rows <- list()
rows[[1]] <- timed("TT dpm=5 w60 p(1,50) [probe/default]", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT", departure_datetime = dep,
  max_trip_duration = 20, walk_speed = 4, time_window = 60,
  percentiles = c(1, 50), draws_per_minute = 5, max_rides = 2L, n_threads = NT))
rows[[2]] <- timed("TT dpm=1 w60 p(1,50)", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT", departure_datetime = dep,
  max_trip_duration = 20, walk_speed = 4, time_window = 60,
  percentiles = c(1, 50), draws_per_minute = 1, max_rides = 2L, n_threads = NT))
rows[[3]] <- timed("TT dpm=2 w60 p(1,50)", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT", departure_datetime = dep,
  max_trip_duration = 20, walk_speed = 4, time_window = 60,
  percentiles = c(1, 50), draws_per_minute = 2, max_rides = 2L, n_threads = NT))
rows[[4]] <- timed("TT w15 dpm=1 p(1,50)", r5r::travel_time_matrix(
  r5r_network = net, origins = oc, destinations = dps, mode = "TRANSIT", departure_datetime = dep,
  max_trip_duration = 20, walk_speed = 4, time_window = 15,
  percentiles = c(1, 50), draws_per_minute = 1, max_rides = 2L, n_threads = NT))

# legacy-shaped call: opportunity counting inside Java (numbers are not the
# deliverable here - the CLOCK is; a point-count column stands in for TYPEQU
# opportunities)
dest_opp <- copy(dps)
dest_opp[, opportunities := 1L]
rows[[5]] <- timed("accessibility() dpm=5 [legacy API shape]", r5r::accessibility(
  net, origins = oc, destinations = dest_opp,
  opportunities_colnames = "opportunities", mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 20, walk_speed = 4,
  time_window = 60, percentiles = 1, cutoffs = c(5, 10, 15, 20),
  draws_per_minute = 5, max_rides = 2L, n_threads = NT))
rows[[6]] <- timed("accessibility() dpm=1", r5r::accessibility(
  net, origins = oc, destinations = dest_opp,
  opportunities_colnames = "opportunities", mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 20, walk_speed = 4,
  time_window = 60, percentiles = 1, cutoffs = c(5, 10, 15, 20),
  draws_per_minute = 1, max_rides = 2L, n_threads = NT))

out <- rbindlist(rows, use.names = TRUE, fill = TRUE)
setcolorder(out, c("variant", "seconds", "rows", "s_per_origin"))
print(out)
fwrite(out, ".scratch/tracer-matrice/research/outputs/22h-window-benchmark.csv")
jsonlite::write_json(list(recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                          n_origins = nrow(oc), n_dest_coords = nrow(dps),
                          n_threads = NT, variants = out),
                     ".scratch/tracer-matrice/research/outputs/22h-window-benchmark.json",
                     auto_unbox = TRUE, pretty = TRUE)
cat("phase H complete\n")
