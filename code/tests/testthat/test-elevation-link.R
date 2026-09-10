library(testthat)

source_project_r("link.R")

test_that("a validated native DEM selects r5r's supported elevation model", {
  expect_equal(normalize_r5r_elevation("TOBLER"), "TOBLER")
  expect_equal(normalize_r5r_elevation("native"), "TOBLER")
  expect_equal(normalize_r5r_elevation("NONE"), "NONE")
})
