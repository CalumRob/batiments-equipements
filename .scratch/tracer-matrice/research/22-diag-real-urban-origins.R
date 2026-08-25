suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

pts <- as.data.table(arrow::read_parquet(file.path(RD, "plan/origin_points.parquet")))
dps <- as.data.table(arrow::read_parquet(file.path(RD, "plan/destination_points.parquet")))

# find the urban-ish sampled origins: closest to Rennes centre and Brest centre
rennes <- pts[which.min((lon + 1.6778)^2 + (lat - 48.1173)^2)]
brest  <- pts[which.min((lon + 4.4860)^2 + (lat - 48.3904)^2)]
cat("closest-to-Rennes sample:", rennes$id,
    sprintf("(%.4f, %.4f)", rennes$lon, rennes$lat), "\n")
cat("closest-to-Brest sample :", brest$id,
    sprintf("(%.4f, %.4f)", brest$lon, brest$lat), "\n")

dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")
for (o in list(brest, rennes)) {
  tr <- route_transit_pairs(net, o, dps, departure_datetime = dep,
                            max_trip_duration = 20, walk_speed = 4,
                            n_threads = Inf, time_window = 60,
                            percentiles = c(1, 50))
  wk <- route_pairs(net, o, dps, mode = "WALK", max_trip_duration = 20,
                    walk_speed = 4, bike_speed = 12, n_threads = Inf)
  nt <- nrow(tr); nw <- nrow(wk)
  cat(sprintf("%s (%.3f,%.3f): TRANSIT %d | WALK %d | delta %+d\n",
              o$id, o$lon, o$lat, nt, nw, nt - nw))
}
r5r::stop_r5(net)
