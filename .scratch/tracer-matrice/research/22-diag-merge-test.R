suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

pts <- as.data.table(arrow::read_parquet(file.path(RD, "plan/origin_points.parquet")))
dps <- as.data.table(arrow::read_parquet(file.path(RD, "plan/destination_points.parquet")))

# where ARE the first 10 origins?
cat("chunk-1 first 10 origins:\n"); print(pts[1:10])
cat("\nsynthetic Brest-centre origin added\n")
b1 <- data.table(id = "coord_o_999999", lon = -4.4860, lat = 48.3904)
oc <- rbindlist(list(pts[1:10], b1))
dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")

tr <- route_transit_pairs(net, oc, dps, departure_datetime = dep,
                          max_trip_duration = 20, walk_speed = 4,
                          n_threads = Inf, time_window = 60,
                          percentiles = c(1, 50))
wk <- route_pairs(net, oc, dps, mode = "WALK", max_trip_duration = 20,
                  walk_speed = 4, bike_speed = 12, n_threads = Inf)
cat("\nTRANSIT pairs:", nrow(tr), "| WALK pairs:", nrow(wk), "\n")
cmp <- merge(
  tr[, .(n_tr = .N), by = from_id],
  wk[, .(n_wk = .N), by = from_id], by = "from_id", all = TRUE)
cmp[is.na(n_tr), n_tr := 0L]; cmp[is.na(n_wk), n_wk := 0L]
cmp[, delta := n_tr - n_wk]
print(cmp[order(-delta)])
cat("\nb1 transit pairs:",
    sum(tr$from_id == "coord_o_999999"),
    "| b1 walk pairs:", sum(wk$from_id == "coord_o_999999"), "\n")
b1t <- tr[from_id == "coord_o_999999"]
if (nrow(b1t)) {
  cat("b1 transit tt summary:\n"); print(summary(b1t$travel_time_p50))
}
r5r::stop_r5(net)
