suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

o <- data.table(id = "r1", lon = -1.6778, lat = 48.1173)   # Rennes centre
d <- data.table(
  id = c("villejean", "bréquigny", "maurepas", "cesson", "pacé"),
  lon = c(-1.7140, -1.6370, -1.6520, -1.5990, -1.7390),
  lat = c(48.1090, 48.0840, 48.1330, 48.1500, 48.1270))

for (args in list(
  list(dep = as.POSIXct("2026-09-03 07:30:00", tz = "UTC"), cap = 60, lbl = "Sep-03 plain cap60 (STAR autumn window LIVE)"),
  list(dep = as.POSIXct("2026-09-03 07:30:00", tz = "UTC"), cap = 20, lbl = "Sep-03 plain cap20"),
  list(dep = as.POSIXct("2026-09-03 07:00:00", tz = "UTC"), cap = 20,
       lbl = "Sep-03 percentiles+window cap20 (probe shape)"))) {
  cat("\n###", args$lbl, "\n")
  tt <- tryCatch(r5r::travel_time_matrix(
    net, origins = o, destinations = d, mode = "TRANSIT",
    departure_datetime = args$dep, max_trip_duration = args$cap,
    time_window = if (is.null(args$cap) || args$cap != 20) NULL else 60,
    percentiles = if (is.null(args$cap) || args$cap != 20) NULL else c(1, 50),
    verbose = FALSE),
    error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
  print(if (is.null(tt)) NULL else as.data.table(tt))
}
r5r::stop_r5(net)
