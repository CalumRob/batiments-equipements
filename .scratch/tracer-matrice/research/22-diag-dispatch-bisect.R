suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"
NET <- "E:/batiments-equipements/data/networks/cap20-current"

req_json <- jsonlite::fromJSON(file.path(RD, "requests/chunk_3.json"),
                               simplifyVector = TRUE)
req <- as_chunk_request(req_json)
rt <- req$routing
cat("== request$routing after as_chunk_request ==\n")
str(rt)
cat("departure class:", class(rt$departure_datetime),
    "| value:", format(rt$departure_datetime, "%Y-%m-%d %H:%M %Z"), "\n")
cat("percentiles:", rt$percentiles, "class", class(rt$percentiles), "\n")
cat("time_window:", rt$time_window, "| n_threads:", rt$n_threads, "\n")

net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)
pts <- as.data.table(arrow::read_parquet(req$paths$origin_points))
dps <- as.data.table(arrow::read_parquet(req$paths$destination_points))
o537 <- pts[id == "coord_o_000537"]

dispatch <- default_mode_dispatch(rt)
tr_via_req <- dispatch(net, o537, dps, mode = "transit")
cat("\n[dispatch(rt)] o537 TRANSIT pairs:", nrow(tr_via_req), "\n")

dep_manual <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")
tr_manual <- route_transit_pairs(net, o537, dps,
                                 departure_datetime = dep_manual,
                                 max_trip_duration = 20, walk_speed = 4,
                                 n_threads = Inf, time_window = 60,
                                 percentiles = c(1, 50))
cat("[manual dep]        o537 TRANSIT pairs:", nrow(tr_manual), "\n")

# if they differ, mutate rt field-by-field toward manual until it flips
if (nrow(tr_via_req) != nrow(tr_manual)) {
  cat("\ndispatch differs from manual -> inspecting rt fields...\n")
} else {
  cat("\nidentical -> dispatch fine; bug must be elsewhere in worker\n")
}
r5r::stop_r5(net)
