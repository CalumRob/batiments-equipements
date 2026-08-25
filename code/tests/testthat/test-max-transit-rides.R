# max_transit_rides contract (#23 preflight, maintainer ruling 2026-08-25):
# legacy-faithful 2 — a third boarding is marginal inside the 20-minute
# cap, and a different ride bound than the legacy snapshot would inject
# unexplained drift into the deltas comparison (D5 named-parameter rule).

test_that("max_transit_rides is the legacy-faithful bound of 2", {
  expect_identical(max_transit_rides(), 2L)
})

test_that("route_transit_pairs passes max_rides through to r5r", {
  captured <- NULL
  fake <- function(...) {
    captured <<- list(...)
    data.table::data.table(from_id = "o", to_id = "d",
                           travel_time_p1 = 5, travel_time_p50 = 7)
  }
  net <- structure(list(), class = "r5r_network")
  o <- data.table::data.table(id = "o", lon = 0, lat = 0)
  d <- data.table::data.table(id = "d", lon = 0.01, lat = 0)
  testthat::local_mocked_bindings(travel_time_matrix = fake, .package = "r5r")
  route_transit_pairs(net, o, d,
                      departure_datetime = as.POSIXct("2026-08-26 05:00:00",
                                                      tz = "UTC"),
                      max_trip_duration = 20, time_window = 60,
                      percentiles = c(1, 50), max_rides = 2L)
  expect_identical(as.integer(captured$max_rides), 2L)
})

test_that("request normalization defaults max_rides to the named constant", {
  req <- list(kind = "matrice-chunk-request", chunk_id = 1L,
              chunk_size = 10L, n_origin_coords = 10L, modes = c("transit"),
              paths = list(), routing = list())
  norm <- as_chunk_request(req)
  expect_identical(as.integer(norm$routing$max_rides), max_transit_rides())
})
