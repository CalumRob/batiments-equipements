library(testthat)
library(data.table)

test_that("net transit gains compare p1 and p50 counts against walk", {
  transit <- data.table(
    batiment_id = c("b1", "b2"),
    TYPEQU = c("A", "B"),
    mode = "transit",
    tt_nearest = c(7, 4),
    travel_time_p1 = c(7, 4),
    travel_time_p50 = c(9, 6),
    count_5 = c(0L, 1L), count_10 = c(1L, 2L),
    count_15 = c(2L, 3L), count_20 = c(3L, 4L),
    count_5_p50 = c(0L, 1L), count_10_p50 = c(1L, 2L),
    count_15_p50 = c(1L, 3L), count_20_p50 = c(2L, 4L)
  )
  walk <- data.table(
    batiment_id = "b1", TYPEQU = "A", mode = "walk",
    tt_nearest = 8,
    count_5 = 0L, count_10 = 1L, count_15 = 1L, count_20 = 2L
  )

  got <- derive_transit_gain_rows(transit, walk)

  expect_identical(got$mode, c("transit_gain", "transit_gain"))
  expect_identical(got$walk_available, c(TRUE, FALSE))
  expect_identical(got$time_gain_p1, c(1, NA_real_))
  expect_identical(got$time_gain_p50, c(-1, NA_real_))
  expect_identical(got$count_gain_5_p1, c(0L, 1L))
  expect_identical(got$count_gain_10_p1, c(0L, 2L))
  expect_identical(got$count_gain_15_p1, c(1L, 3L))
  expect_identical(got$count_gain_20_p1, c(1L, 4L))
  expect_identical(got$count_gain_5_p50, c(0L, 1L))
  expect_identical(got$count_gain_10_p50, c(0L, 2L))
  expect_identical(got$count_gain_15_p50, c(0L, 3L))
  expect_identical(got$count_gain_20_p50, c(0L, 4L))
})

test_that("a transit-gain chunk is written and validated as a derived artifact", {
  transit <- data.table(
    batiment_id = "b1", TYPEQU = "A", mode = "transit",
    tt_nearest = 7, travel_time_p1 = 7, travel_time_p50 = 9,
    count_5 = 0L, count_10 = 1L, count_15 = 2L, count_20 = 3L,
    count_5_p50 = 0L, count_10_p50 = 1L, count_15_p50 = 1L,
    count_20_p50 = 2L
  )
  walk <- data.table(
    batiment_id = "b1", TYPEQU = "A", mode = "walk",
    tt_nearest = 8, count_5 = 0L, count_10 = 1L,
    count_15 = 1L, count_20 = 2L
  )
  root <- tempfile("transit-gain-")
  dir.create(root)
  transit_path <- file.path(root, "transit_1.parquet")
  walk_path <- file.path(root, "walk_1.parquet")
  arrow::write_parquet(transit, transit_path)
  arrow::write_parquet(walk, walk_path)

  out_dir <- file.path(root, "derived")
  path <- derive_transit_gain_chunk(
    transit_path, walk_path, chunk_id = 1L, out_dir = out_dir
  )

  expect_true(file.exists(path))
  expect_identical(basename(path), "transit_gain_1.parquet")
  got <- data.table::as.data.table(arrow::read_parquet(path))
  expect_silent(validate_transit_gain_rows(got))
  expect_identical(got$count_gain_15_p1, 1L)
  expect_identical(got$time_gain_p1, 1)
})

test_that("reroute transit gains pair matching walk and transit chunks", {
  root <- tempfile("reroute-gains-")
  dir.create(file.path(root, "walk"), recursive = TRUE)
  dir.create(file.path(root, "transit"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  transit <- data.table(
    batiment_id = "address-1", TYPEQU = "A", mode = "transit",
    tt_nearest = 7, travel_time_p1 = 7, travel_time_p50 = 9,
    count_5 = 0L, count_10 = 1L, count_15 = 2L, count_20 = 3L,
    count_5_p50 = 0L, count_10_p50 = 1L, count_15_p50 = 1L,
    count_20_p50 = 2L
  )
  walk <- data.table(
    batiment_id = "address-1", TYPEQU = "A", mode = "walk",
    tt_nearest = 8, count_5 = 0L, count_10 = 1L,
    count_15 = 1L, count_20 = 2L
  )
  arrow::write_parquet(transit, file.path(root, "transit", "transit_1.parquet"))
  arrow::write_parquet(walk, file.path(root, "walk", "walk_1.parquet"))

  out <- derive_reroute_transit_gains(
    root, chunk_ids = 1L, verbose = FALSE
  )

  expect_identical(out$n_chunks, 1L)
  expect_true(file.exists(file.path(
    root, "derived", "transit-gain", "transit_gain_1.parquet"
  )))
  got <- arrow::read_parquet(out$paths[[1L]])
  expect_silent(validate_transit_gain_rows(got))
  expect_identical(got$batiment_id, "address-1")
  expect_identical(got$time_gain_p1, 1)
})
