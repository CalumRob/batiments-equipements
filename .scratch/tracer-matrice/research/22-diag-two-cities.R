suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

o <- data.table(id = c("rennes", "brest"),
                lon = c(-1.6778, -4.4860), lat = c(48.1173, 48.3904))
d <- data.table(
  id = c("villejean", "bréquigny", "gouesnou", "guipavas", "lamorlais"),
  lon = c(-1.7140, -1.6370, -4.4695, -4.4310, -1.6260),
  lat = c(48.1090, 48.0840, 48.4310, 48.4440, 48.1140))

for (dep in list(as.POSIXct("2026-09-03 07:30:00", tz = "UTC"),
                 as.POSIXct("2026-08-26 05:30:00", tz = "UTC"))) {
  cat("\n### both cities, departure", format(dep, "%Y-%m-%dT%H:%M%z"),
      "TRANSIT plain cap45\n")
  tt <- tryCatch(r5r::travel_time_matrix(
    net, origins = o, destinations = d, mode = "TRANSIT",
    departure_datetime = dep, max_trip_duration = 45,
    time_window = 60, percentiles = c(50), verbose = FALSE),
    error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
  print(if (is.null(tt)) NULL else as.data.table(tt))
}
r5r::stop_r5(net)
