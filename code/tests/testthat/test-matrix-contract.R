library(testthat)
library(data.table)

# The cap-and-ladder contract (#17): 20 minutes is the authoritative once-run
# cap; the count ladder is exactly 5/10/15/20. Source the modules directly so
# the contract is exercised without loading routing dependencies.
source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/validate-matrix.R"), local = TRUE)
source(testthat::test_path("../../R/derive.R"), local = TRUE)
source(testthat::test_path("../../R/fixture.R"), local = TRUE)

valid_matrix <- function(tt = 8, counts = c(0L, 1L, 1L, 2L)) {
  data.table::data.table(
    batiment_id = "b1", TYPEQU = "B104", mode = "walk", tt_nearest = tt,
    count_5 = counts[1], count_10 = counts[2], count_15 = counts[3],
    count_20 = counts[4]
  )
}

test_that("the authoritative cap is 20 minutes and the ladder is exactly 5/10/15/20", {
  expect_identical(ladder_rungs(), c(5L, 10L, 15L, 20L))
  expect_identical(cap_minutes(), 20L)
  expect_identical(ladder_cols(), c("count_5", "count_10", "count_15", "count_20"))
  expect_false(30L %in% ladder_rungs())
})

test_that("a matrix on the contract ladder validates", {
  expect_silent(validate_matrix(valid_matrix()))
})

test_that("the validator refuses travel times beyond the cap", {
  expect_error(validate_matrix(valid_matrix(tt = 21)), "cap")
})

test_that("a 30-minute ladder claim cannot pass as release-grade (#17)", {
  legacy <- valid_matrix()
  legacy[, count_30 := 2L]
  expect_error(validate_matrix(legacy), "off the ladder")
})

test_that("an off-ladder transit p50 count claim is refused too", {
  legacy_tx <- valid_matrix()
  legacy_tx[, count_30_p50 := 2L]
  expect_error(validate_matrix(legacy_tx), "off the ladder")
})

test_that("routing windows beyond the cap are refused at the boundary", {
  expect_error(assert_within_cap(30), "authoritative cap")
  expect_error(assert_within_cap(21), "authoritative cap")
  expect_silent(assert_within_cap(20))
  expect_silent(assert_within_cap(cap_minutes()))
  expect_error(assert_within_cap(0), "positive")
  expect_error(assert_within_cap(-5), "positive")
})

test_that("derivation thresholds are ladder rungs — 30 minutes is not one anymore", {
  expect_error(
    derive_building_metrics(valid_matrix(), kept = "B104", threshold = 30),
    "ladder rung"
  )
  expect_silent(derive_building_metrics(valid_matrix(), kept = "B104", threshold = 20))
})

test_that("the fixture lives on the authoritative cap-and-ladder (#17)", {
  f <- make_fixture()
  expect_silent(validate_matrix(f$matrix))
  count_cols <- grep("^count_", names(f$matrix), value = TRUE)
  expect_setequal(count_cols, ladder_cols())
  expect_false("count_30" %in% names(f$matrix))
  expect_true(all(f$matrix$tt_nearest <= cap_minutes()))
  expect_true(max(f$matrix$tt_nearest) == cap_minutes()) # the cap edge is exercised
})

test_that("the fixture keeps its sparse worked-example shape under the cap", {
  m <- make_fixture()$matrix
  # b3 and b4 are walk-isolated: no walk rows exist within the cap.
  expect_equal(nrow(m[batiment_id %in% c("b3", "b4") & mode == "walk"]), 0L)
  # b2 is walk-poor: one walk row, exactly at the cap edge.
  b2_walk <- m[batiment_id == "b2" & mode == "walk"]
  expect_equal(nrow(b2_walk), 1L)
  expect_equal(b2_walk$tt_nearest, cap_minutes())
  # every building keeps car readings; unreachable types simply have no rows.
  expect_setequal(unique(m$batiment_id), c("b1", "b2", "b3", "b4"))
})

test_that("the derivation layer reads the new ladder on the fixture", {
  f <- make_fixture()
  got <- derive_building_metrics(f$matrix, kept = f$kept, threshold = 20,
                                 ref_mode = "car")
  b1_car <- got[batiment_id == "b1" & mode == "car"]
  expect_equal(b1_car$diversity, 7L)   # all 7 kept types by car
  expect_equal(b1_car$total, 8L)       # B104 hosts 2 establishments
  expect_equal(b1_car$div_loss, 0)     # the ref-mode row loses nothing
  # b4 deep rural: nothing by walk (zero-filled), two kept types by car.
  b4 <- got[batiment_id == "b4"]
  expect_equal(b4[mode == "walk", diversity], 0L)
  expect_equal(b4[mode == "walk", div_loss], 2L)
  expect_equal(b4[mode == "car", diversity], 2L)
})
