library(testthat)

source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/link.R"), local = TRUE)

test_that("unreachable transit percentile pairs are omitted as sparse rows", {
  pairs <- data.table::data.table(
    from_id = c("b1", "b1"), to_id = c("d1", "d2"),
    travel_time_p1 = c(4, NA_real_),
    travel_time_p50 = c(8, NA_real_)
  )
  destinations <- data.table::data.table(id = c("d1", "d2"), TYPEQU = c("A", "B"))
  out <- derive_transit_matrix_rows(pairs, destinations)
  expect_equal(nrow(out), 1L)
  expect_equal(out$TYPEQU, "A")
  expect_false(anyNA(out$travel_time_p1))
})
