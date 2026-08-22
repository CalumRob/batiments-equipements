library(testthat)
library(data.table)

# The once-run's metadata contract (#17): every run-metadata entry describes
# the authoritative 20-minute cap and the exact 5/10/15/20 ladder. Exercised
# through run_tracer's dry_run seam (no JVM/network) plus the routing entry
# points' boundary guard.
source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/full-run-inputs.R"), local = TRUE)
source(testthat::test_path("../../R/link.R"), local = TRUE)
source(testthat::test_path("../../R/run-tracer.R"), local = TRUE)

toy_pbf <- function() {
  path <- file.path(tempdir(), "toy-network.osm.pbf")
  file.create(path)
  path
}

test_that("the run metadata carries the authoritative cap and exact ladder", {
  pbf <- toy_pbf()
  on.exit(unlink(pbf), add = TRUE)
  dry <- run_tracer(pbf, dry_run = TRUE)

  expect_identical(dry$cap_minutes, 20L)
  expect_identical(dry$ladder_rungs, c(5L, 10L, 15L, 20L))
  expect_identical(dry$ladder_cols, c("count_5", "count_10", "count_15", "count_20"))
  expect_identical(dry$routing_parameters$max_trip_duration, cap_minutes())
})

test_that("the metadata JSON round-trips the cap-and-ladder contract", {
  pbf <- toy_pbf()
  on.exit(unlink(pbf), add = TRUE)
  dry <- run_tracer(pbf, dry_run = TRUE)

  # Mirror write_json()'s serialization of the run summary.
  meta <- list(cap_minutes = dry$cap_minutes,
               ladder_rungs = dry$ladder_rungs,
               ladder_cols = dry$ladder_cols,
               routing_parameters = dry$routing_parameters)
  rt <- jsonlite::fromJSON(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null"))
  expect_equal(as.integer(rt$cap_minutes), 20L)
  expect_equal(as.integer(rt$ladder_rungs), c(5L, 10L, 15L, 20L))
  expect_false(30L %in% as.integer(rt$ladder_rungs))
  expect_identical(as.character(rt$ladder_cols),
                   c("count_5", "count_10", "count_15", "count_20"))
  expect_equal(as.numeric(rt$routing_parameters$max_trip_duration), 20)
})

test_that("a 30-minute window is refused by the driver and the routing entries", {
  pbf <- toy_pbf()
  on.exit(unlink(pbf), add = TRUE)
  expect_error(run_tracer(pbf, max_trip_duration = 30, dry_run = TRUE),
               "authoritative cap")
  expect_error(run_tracer(pbf, max_trip_duration = cap_minutes() + 1,
                          dry_run = TRUE),
               "authoritative cap")

  origins <- data.table(id = "o1", lon = -1.32, lat = 48.35)
  dests <- data.table(id = "d1", TYPEQU = "B104")
  expect_error(route_pairs(NULL, origins, dests, "WALK", max_trip_duration = 30),
               "authoritative cap")
  expect_error(
    route_transit_pairs(NULL, origins, dests,
                        departure_datetime = as.POSIXct("2026-02-17 08:00", tz = "UTC"),
                        max_trip_duration = 30),
    "authoritative cap"
  )
})
