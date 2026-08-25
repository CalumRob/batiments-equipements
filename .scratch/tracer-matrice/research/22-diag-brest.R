suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
NET <- "E:/batiments-equipements/data/networks/cap20-current"
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)

# origin and destinations placed AT known Bibus (Brest) stops region:
# Brest centre -> suburbs, well beyond walking range
o <- data.table(id = "b1", lon = -4.4860, lat = 48.3904)
d <- data.table(
  id = c("gouesnou", "guipavas", "plougastel", "relecq", "bohars", "le_relecq2"),
  lon = c(-4.4695, -4.4310, -4.3700, -4.5570, -4.5090, -4.5560),
  lat = c(48.4310, 48.4440, 48.3720, 48.3660, 48.4020, 48.3590))
dep <- as.POSIXct("2026-08-26 07:30:00", tz = "UTC")

cat("### TRANSIT plain (no percentiles/window), cap 60\n")
t1 <- tryCatch(r5r::travel_time_matrix(
  net, origins = o, destinations = d, mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 60, verbose = FALSE),
  error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
print(if (is.null(t1)) NULL else as.data.table(t1))

cat("\n### TRANSIT with percentiles + window (our call shape), cap 20\n")
t2 <- tryCatch(r5r::travel_time_matrix(
  net, origins = o, destinations = d, mode = "TRANSIT",
  departure_datetime = dep, max_trip_duration = 20,
  time_window = 60, percentiles = c(1, 50), verbose = FALSE),
  error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
print(if (is.null(t2)) NULL else as.data.table(t2))

cat("\n### BUS only, plain, cap 60\n")
t3 <- tryCatch(r5r::travel_time_matrix(
  net, origins = o, destinations = d, mode = "BUS",
  departure_datetime = dep, max_trip_duration = 60, verbose = FALSE),
  error = function(e) { cat("ERR:", conditionMessage(e), "\n"); NULL })
print(if (is.null(t3)) NULL else as.data.table(t3))

r5r::stop_r5(net)
