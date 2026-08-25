options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

# one Rennes-centre origin, a handful of destinations spread across Rennes
o <- data.table::data.table(id = "o1", lon = -1.6778, lat = 48.1173)
d <- data.table::data.table(
  id = paste0("d", 1:6),
  lon = c(-1.6778, -1.6294, -1.5980, -1.7090, -1.6493, -1.5550),
  lat = c(48.1173, 48.1119, 48.1329, 48.1025, 48.0861, 48.0972))

for (dep in list(as.POSIXct("2026-09-15 06:00:00", tz = "UTC"),
                 as.POSIXct("2026-08-26 06:00:00", tz = "UTC"))) {
  cat("\n=== departure:", format(dep, "%Y-%m-%dT%H:%M%z"), "===\n")
  tt <- tryCatch(r5r::travel_time_matrix(
    net, origins = o, destinations = d, mode = "TRANSIT",
    departure_datetime = dep, max_trip_duration = 20,
    time_window = 60, percentiles = c(1, 50),
    verbose = FALSE),
    error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
  if (!is.null(tt)) {
    print(as.data.table(tt))
  }
  ww <- tryCatch(r5r::travel_time_matrix(
    net, origins = o, destinations = d, mode = "WALK",
    max_trip_duration = 20, verbose = FALSE),
    error = function(e) { cat("WALK ERR:", conditionMessage(e), "\n"); NULL })
  if (!is.null(ww)) { cat("-- walk --\n"); print(as.data.table(ww)) }
}
r5r::stop_r5(net)
