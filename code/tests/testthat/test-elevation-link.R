library(testthat)

source(testthat::test_path("../../R/link.R"), local = TRUE)

test_that("a validated native DEM selects r5r's supported elevation model", {
  expect_equal(normalize_r5r_elevation("TOBLER"), "TOBLER")
  expect_equal(normalize_r5r_elevation("native"), "TOBLER")
  expect_equal(normalize_r5r_elevation("NONE"), "NONE")
})
