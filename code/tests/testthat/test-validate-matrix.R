# S1 — validate_matrix(): the matrix contract (run-strategy §"Matrix contract", ADR-0002).
# Red before green: these fail until validate_matrix() exists.

# A minimal conforming artifact (3 rows, 2 modes, hand-checkable consistency:
# count_r >= 1 iff tt_nearest <= r, for every ladder rung r).
mini_valid <- data.frame(
  batiment_id = c("b1", "b1", "b2"),
  TYPEQU      = c("B104", "D265", "B104"),
  mode        = c("walk", "walk", "car"),
  tt_nearest  = c(8, 15, 12),
  count_5     = c(0L, 0L, 0L),
  count_10    = c(1L, 0L, 0L),
  count_15    = c(1L, 1L, 1L),
  count_20    = c(2L, 1L, 1L),
  count_30    = c(2L, 1L, 1L),
  stringsAsFactors = FALSE
)

test_that("a conforming artifact passes", {
  expect_true(validate_matrix(mini_valid))
})

test_that("missing required columns are rejected", {
  bad <- mini_valid[, setdiff(names(mini_valid), "tt_nearest")]
  expect_error(validate_matrix(bad), "tt_nearest")
  bad2 <- mini_valid[, setdiff(names(mini_valid), "count_30")]
  expect_error(validate_matrix(bad2), "count_30")
})

test_that("a non-atomic mode is rejected", {
  bad <- mini_valid
  bad$mode[1] <- "hoverboard"
  expect_error(validate_matrix(bad), "mode")
})

test_that("negative travel times are rejected", {
  bad <- mini_valid
  bad$tt_nearest[1] <- -1
  expect_error(validate_matrix(bad), "tt_nearest")
})

test_that("travel times beyond the cap are rejected", {
  bad <- mini_valid
  bad$tt_nearest[1] <- 35   # cap = 30
  expect_error(validate_matrix(bad), "tt_nearest")
})

test_that("fractional or negative counts are rejected", {
  bad <- mini_valid
  bad$count_20[1] <- 1.5
  expect_error(validate_matrix(bad), "count")
  bad <- mini_valid
  bad$count_10[1] <- -1
  expect_error(validate_matrix(bad), "count")
})

test_that("duplicate (batiment_id, TYPEQU, mode) keys are rejected", {
  bad <- rbind(mini_valid, mini_valid[1, ])
  expect_error(validate_matrix(bad), "duplicate|unique")
})

test_that("count/tt inconsistencies are rejected (count_r >= 1 iff tt <= r)", {
  # a row exists (reachable within cap) but count_20 says nothing within 20
  # while tt_nearest is 25: count_20 must be 0, not 1
  bad <- mini_valid
  bad$tt_nearest[1] <- 25
  expect_error(validate_matrix(bad), "count_20")
  # count_30 = 0 on an existing row: rows exist only where reachable within the cap
  bad <- mini_valid
  bad$count_30[1] <- 0
  expect_error(validate_matrix(bad), "count_30")
})

test_that("a non-monotone count ladder is rejected", {
  bad <- mini_valid
  bad$count_5[1] <- 2
  bad$count_10[1] <- 1
  expect_error(validate_matrix(bad), "monotone|ladder")
})

test_that("extensions are accepted, not schema breaks (axis-extension rule)", {
  # extra percentile columns (transit p1/p50 axis)
  ext <- mini_valid
  ext$tt_nearest_p1 <- ext$tt_nearest - 1
  ext$tt_nearest_p50 <- ext$tt_nearest
  expect_true(validate_matrix(ext))
  # a TYPEQU outside any kept list (full-universe axis, ADR-0002)
  ext <- mini_valid
  ext$TYPEQU[1] <- "Z999"
  expect_true(validate_matrix(ext))
  # a subset of modes is fine (tracer: walk + car only)
  expect_true(validate_matrix(mini_valid))
})

test_that("parquet round-trip preserves a conforming artifact", {
  path <- tempfile(fileext = ".parquet")
  arrow::write_parquet(mini_valid, path)
  on.exit(unlink(path))
  m <- read_matrix(path)
  expect_true(validate_matrix(m))
})
