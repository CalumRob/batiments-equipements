# r5r 2.4.0 returns travel_time_matrix endpoints in destination -> origin
# orientation (`from_id` is the supplied destination id and `to_id` is the
# supplied origin id).  The project contract is origin -> destination, so the
# adapter at link.R's boundary must normalize both direct and transit results.

test_that("route_pairs normalizes r5r's reversed endpoint orientation", {
  fake <- function(...) {
    data.table::data.table(from_id = "d1", to_id = "o1",
                           travel_time_p50 = 7L)
  }
  net <- structure(list(), class = "r5r_network")
  origins <- data.table::data.table(id = "o1", lon = 0, lat = 0)
  destinations <- data.table::data.table(id = "d1", lon = 0.01, lat = 0)
  testthat::local_mocked_bindings(travel_time_matrix = fake, .package = "r5r")

  got <- route_pairs(net, origins, destinations, "WALK")

  expect_identical(got$from_id, "o1")
  expect_identical(got$to_id, "d1")
  expect_identical(got$travel_time_p50, 7L)
})

test_that("route_pairs preserves an already canonical orientation", {
  fake <- function(...) {
    data.table::data.table(from_id = "o1", to_id = "d1",
                           travel_time_p50 = 7L)
  }
  net <- structure(list(), class = "r5r_network")
  origins <- data.table::data.table(id = "o1", lon = 0, lat = 0)
  destinations <- data.table::data.table(id = "d1", lon = 0.01, lat = 0)
  testthat::local_mocked_bindings(travel_time_matrix = fake, .package = "r5r")

  got <- route_pairs(net, origins, destinations, "WALK")

  expect_identical(got$from_id, "o1")
  expect_identical(got$to_id, "d1")
})

test_that("route_transit_pairs normalizes r5r's reversed endpoint orientation", {
  fake <- function(...) {
    data.table::data.table(from_id = "d1", to_id = "o1",
                           travel_time_p1 = 5L, travel_time_p50 = 7L)
  }
  net <- structure(list(), class = "r5r_network")
  origins <- data.table::data.table(id = "o1", lon = 0, lat = 0)
  destinations <- data.table::data.table(id = "d1", lon = 0.01, lat = 0)
  testthat::local_mocked_bindings(travel_time_matrix = fake, .package = "r5r")

  got <- route_transit_pairs(
    net, origins, destinations,
    departure_datetime = as.POSIXct("2026-08-26 05:00:00", tz = "UTC")
  )

  expect_identical(got$from_id, "o1")
  expect_identical(got$to_id, "d1")
  expect_identical(got$travel_time_p1, 5L)
  expect_identical(got$travel_time_p50, 7L)
})
