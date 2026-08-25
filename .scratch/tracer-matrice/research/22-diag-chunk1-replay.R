suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

pts <- as.data.table(arrow::read_parquet(file.path(RD, "plan/origin_points.parquet")))
dps <- as.data.table(arrow::read_parquet(file.path(RD, "plan/destination_points.parquet")))
oc <- pts[1:10]
dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")

cat("origins:", nrow(oc), "| dest coords:", nrow(dps), "\n")

tr_wrapper <- route_transit_pairs(net, oc, dps, departure_datetime = dep,
                                  max_trip_duration = 20, walk_speed = 4,
                                  n_threads = Inf, time_window = 60,
                                  percentiles = c(1, 50))
cat("\nroute_transit_pairs: pairs", nrow(tr_wrapper),
    "| distinct from:", data.table::uniqueN(tr_wrapper$from_id), "\n")

tr_direct <- data.table::as.data.table(r5r::travel_time_matrix(
  net, origins = as.data.frame(oc), destinations = as.data.frame(dps),
  mode = "TRANSIT", departure_datetime = dep, max_trip_duration = 20,
  walk_speed = 4, time_window = 60, percentiles = c(1, 50),
  verbose = FALSE))
cat("direct r5r identical args:", nrow(tr_direct), "pairs\n")

wk_wrapper <- route_pairs(net, oc, dps, mode = "WALK",
                          max_trip_duration = 20, walk_speed = 4,
                          bike_speed = 12, n_threads = Inf)
cat("route_pairs WALK:", nrow(wk_wrapper), "pairs\n")

# per-origin comparison: which origins have MORE transit than walk pairs?
cmp <- merge(
  tr_wrapper[, .(n_tr = .N), by = from_id],
  wk_wrapper[, .(n_wk = .N), by = from_id], by = "from_id", all = TRUE)
cmp[is.na(n_tr), n_tr := 0L]; cmp[is.na(n_wk), n_wk := 0L]
cmp[, delta := n_tr - n_wk]
print(cmp)
cat("\ntransit-only pairs:", sum(cmp$delta > 0), "\n")
r5r::stop_r5(net)
