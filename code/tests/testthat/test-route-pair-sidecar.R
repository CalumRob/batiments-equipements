library(testthat)
library(data.table)

source(testthat::test_path("../../R/link.R"), local = TRUE)
source(testthat::test_path("../../R/run-tracer.R"), local = TRUE)

test_that("raw route-pair writer preserves duplicates and metadata", {
  out_dir <- tempfile("pairs-")
  pairs <- data.table(
    from_id = c("o1", "o1", "o1"),
    to_id = c("d1", "d1", "d2"),
    travel_time = c(4, 3, 9)
  )

  path <- write_route_pairs_chunk(pairs, "car", 2L, "legacy", out_dir)
  expect_true(file.exists(path))
  expect_match(basename(path), "^legacy_car_2\\.parquet$")
  got <- as.data.table(arrow::read_parquet(path))
  expect_named(got, c("from_id", "to_id", "travel_time", "mode",
                      "chunk_id", "run_label"))
  expect_equal(nrow(got), 3L)
  expect_equal(got[, .N, by = .(from_id, to_id)][N > 1L, N], 2L)
  expect_equal(unique(got[["mode"]]), "car")
  expect_equal(unique(got[["chunk_id"]]), 2L)
  expect_equal(unique(got[["run_label"]]), "legacy")
})

test_that("raw route-pair writer validates its input schema and types", {
  expect_error(
    write_route_pairs_chunk(data.table(from_id = "o1", to_id = "d1"),
                            "walk", 1L, "current", tempfile()),
    "travel_time"
  )
  expect_error(
    write_route_pairs_chunk(data.table(from_id = 1L, to_id = "d1",
                                       travel_time = 2),
                            "walk", 1L, "current", tempfile()),
    "must be character"
  )
})

test_that("raw capture precedes the matrix pair collapse", {
  pairs <- data.table(
    from_id = c("o1", "o1", "o1"), to_id = c("d1", "d1", "d2"),
    travel_time = c(4, 3, 9)
  )
  views <- route_pair_views(pairs)
  expect_equal(nrow(views$raw), 3L)
  expect_equal(nrow(views$collapsed), 2L)
  expect_equal(views$collapsed[from_id == "o1" & to_id == "d1", travel_time], 3)
  expect_equal(nrow(views$raw[from_id == "o1" & to_id == "d1"]), 2L)
})

test_that("origin selection is opt-in and reports requested and selected counts", {
  origins <- data.table(origin_id = c("o1", "o2", "o3"), value = 1:3)
  all_origins <- select_origin_ids(origins)
  expect_equal(all_origins$n_requested, 3L)
  expect_equal(all_origins$n_selected, 3L)
  selected <- select_origin_ids(origins, c("o3", "missing"))
  expect_equal(selected$origins[["origin_id"]], "o3")
  expect_equal(selected$n_requested, 2L)
  expect_equal(selected$n_selected, 1L)
  expect_error(select_origin_ids(origins, "missing"), "none.*match")
})
