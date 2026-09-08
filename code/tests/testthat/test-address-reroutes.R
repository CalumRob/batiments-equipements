library(testthat)
library(data.table)

source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/route-coordinates.R"), local = TRUE)
source(testthat::test_path("../../R/address-reroutes.R"), local = TRUE)

test_that("address reroute eligibility is non-dependence construction based and exact", {
  origins <- data.table(
    origin_id = c("c1", "c2", "c3"),
    batiment_groupe_id = c("g1", "g1", "g2"),
    is_dependance_candidate = c(FALSE, TRUE, FALSE)
  )
  construction_addresses <- data.table(
    batiment_construction_id = c("c1", "c2", "c1", "c3"),
    cle_interop_adr = c("a1", "a2", "a2", "a3")
  )
  address_points <- data.table(
    address_id = c("a1", "a2", "a3", "not-eligible"),
    lon = c(-1.35, -1.36, -1.37, -1.38),
    lat = c(48.10, 48.11, 48.12, 48.13)
  )
  existing_points <- data.table(
    id = "coord_o_000001", lon = -1.35, lat = 48.10
  )

  got <- build_address_reroute_universe(
    origins, construction_addresses, address_points, existing_points
  )

  # a2 is retained through c1 but not through the dependence-only c2; every
  # eligible address identity is represented once.
  expect_setequal(got$addresses$address_id, c("a1", "a2", "a3"))
  expect_false("not-eligible" %in% got$addresses$address_id)
  expect_equal(got$counts$n_eligible_constructions, 2L)
  expect_equal(got$counts$n_eligible_addresses, 3L)
  expect_equal(got$counts$n_new_address_identities, 2L)
  expect_equal(got$counts$n_new_coordinates, 2L)
  expect_true(all(grepl("^coord_o_", got$new_origin_plan$points$id)))
  expect_identical(
    got$addresses[address_id == "a1", routing_source], "existing"
  )
  expect_setequal(
    got$address_construction_link$construction_id, c("c1", "c3")
  )
  expect_false("c2" %in% got$address_construction_link$construction_id)
})

test_that("conflicting address geometry is refused instead of guessed", {
  address_points <- data.table(
    cle_interop_adr = c("a1", "a1"),
    WKT = c("POINT (0 0)", "POINT (1 1)")
  )
  expect_error(
    address_reroute_address_points_from_wkt(address_points),
    "conflicting geometries"
  )
})
