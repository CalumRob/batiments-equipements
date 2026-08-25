suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"
cat("loading network WITH elevation=TOBLER exactly as probe children do...\n")
net <- link_network(data_path = NET, elevation = "TOBLER", verbose = FALSE)

dps <- as.data.table(arrow::read_parquet(file.path(RD, "plan/destination_points.parquet")))
b1 <- data.table(id = "coord_o_999999", lon = -4.4860, lat = 48.3904)
dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")

tr <- route_transit_pairs(net, b1, dps, departure_datetime = dep,
                          max_trip_duration = 20, walk_speed = 4,
                          n_threads = Inf, time_window = 60,
                          percentiles = c(1, 50))
wk <- route_pairs(net, b1, dps, mode = "WALK", max_trip_duration = 20,
                  walk_speed = 4, bike_speed = 12, n_threads = Inf)
cat("\n[TOBLER-loaded] b1 TRANSIT pairs:", nrow(tr),
    "| WALK pairs:", nrow(wk), "\n")
if (nrow(tr)) print(summary(tr$travel_time_p50))
r5r::stop_r5(net)
