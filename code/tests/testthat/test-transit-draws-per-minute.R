# transit_draws_per_minute contract (#22h benchmark, maintainer ruling
# 2026-08-26): 1 draw per minute of the window — schedule-faithful (GTFS
# timetables are minute-resolution), legacy-comparable (r5r 2.3.0 sampled at
# about this density and exposed no knob), and 1.66x cheaper than the r5r
# >= 2.4.0 default of 5.

test_that("transit_draws_per_minute is the schedule-faithful 1", {
  expect_identical(transit_draws_per_minute(), 1L)
})

test_that("route_transit_pairs passes draws_per_minute through to r5r", {
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
                      percentiles = c(1, 50), draws_per_minute = 1L)
  expect_identical(as.integer(captured$draws_per_minute), 1L)
})

test_that("request normalization defaults draws_per_minute to the constant", {
  req <- list(kind = "matrice-chunk-request", chunk_id = 1L,
              chunk_size = 10L, n_origin_coords = 10L, modes = c("transit"),
              paths = list(), routing = list())
  norm <- as_chunk_request(req)
  expect_identical(as.integer(norm$routing$draws_per_minute),
                   transit_draws_per_minute())
})
