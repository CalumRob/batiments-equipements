library(testthat)
library(data.table)

source(testthat::test_path("../../R/read-bdnb.R"), local = TRUE)

geometry_fixture <- function() {
  data.table(
    x_2154 = c(10, NA, NA, 40, NA), y_2154 = c(11, NA, NA, 41, NA),
    gx_2154 = c(20, 21, NA, 42, NA), gy_2154 = c(21, 22, NA, 43, NA),
    ax_2154 = c(30, 31, NA, 44, NA), ay_2154 = c(31, 32, NA, 45, NA),
    fictive_geom_cstr = c(0L, 0L, NA_integer_, 1L, NA_integer_),
    contient_fictive_geom_groupe = c(0L, 0L, 0L, 1L, 1L),
    fiabilite = c(20, 20, NA, 20, NA)
  )
}

test_that("BAN address is primary, including for a construction origin", {
  got <- bdnb_resolve_geometry(geometry_fixture())
  expect_equal(got[["geometry_source"]][1], "geom_adresse")
  expect_equal(got[["x_2154"]][1], 30)
})

test_that("fallback precedence is construction then real group centroid", {
  got <- bdnb_resolve_geometry(geometry_fixture(), min_fiabilite = 21)
  expect_equal(got[["geometry_source"]][1:3],
               c("geom_cstr", "geom_cstr", "geom_groupe"))
  expect_equal(got[["x_2154"]][2], 10) # address rejected by the gate
  expect_equal(got[["x_2154"]][3], 21)
})

test_that("fictive geometry is explicit and can be dropped", {
  got <- bdnb_resolve_geometry(geometry_fixture(), min_fiabilite = 21)
  expect_equal(got[["geometry_source"]][4:5], c("fictive", "fictive"))
  expect_true(all(got[["geometry_resolved"]]))
  expect_equal(sum(got[["geometry_source"]] != "fictive"), 3)
})

test_that("geometry policy metadata makes the BAN contract cache-visible", {
  policy <- bdnb_geometry_policy()
  expect_identical(policy[["name"]], "ban_address_primary_v1")
  expect_identical(policy[["precedence"]],
                   c("geom_adresse", "geom_cstr", "geom_groupe", "fictive"))
  expect_match(policy[["address"]], "highest_fiabilite")
})
