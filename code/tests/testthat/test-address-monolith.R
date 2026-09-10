library(testthat)
library(data.table)

source_project_r("constants.R")
source_project_r("address-monolith.R")

test_that("sequential monolith upserts add sparse keys and fill metric families", {
  walk <- data.table(
    address_id = "a1", TYPEQU = "A",
    lon = -1.5, lat = 48.1,
    n_linked_constructions = 1L, n_residential_groups = 1L,
    nearest_walk = 7, count_5_walk = 1L
  )
  bike <- data.table(
    address_id = c("a1", "a2"), TYPEQU = c("A", "B"),
    lon = c(-1.5, -1.6), lat = c(48.1, 48.2),
    n_linked_constructions = c(1L, 2L),
    n_residential_groups = c(1L, 1L),
    nearest_bike = c(8, 12), count_5_bike = c(0L, 0L)
  )

  got <- merge_address_monolith_rows(walk, bike)

  expect_equal(nrow(got), 2L)
  expect_equal(got[address_id == "a1" & TYPEQU == "A", nearest_walk], 7)
  expect_equal(got[address_id == "a1" & TYPEQU == "A", nearest_bike], 8)
  expect_equal(got[address_id == "a2" & TYPEQU == "B", nearest_bike], 12)
  expect_equal(got[address_id == "a2" & TYPEQU == "B", nearest_walk], NA_real_)
  expect_true(all(!duplicated(got, by = c("address_id", "TYPEQU"))))
})

test_that("identical duplicate rows collapse without double-counting", {
  rows <- data.table(
    address_id = c("a1", "a1"), TYPEQU = c("A", "A"),
    nearest_walk = c(7, 7), count_5_walk = c(1L, 1L)
  )

  got <- collapse_address_monolith_rows(rows)

  expect_equal(nrow(got), 1L)
  expect_equal(got$nearest_walk, 7)
  expect_equal(got$count_5_walk, 1L)
})

test_that("conflicting duplicate values fail loudly", {
  rows <- data.table(
    address_id = c("a1", "a1"), TYPEQU = c("A", "A"),
    nearest_walk = c(7, 9), count_5_walk = c(1L, 1L)
  )

  expect_error(
    collapse_address_monolith_rows(rows),
    "conflicting values for duplicate monolith key"
  )
})

test_that("an NA versus a value is also a duplicate conflict", {
  rows <- data.table(
    address_id = c("a1", "a1"), TYPEQU = c("A", "A"),
    time_gain_p1 = c(NA_real_, 1)
  )

  expect_error(
    collapse_address_monolith_rows(rows),
    "conflicting values for duplicate monolith key"
  )
})

test_that("an incoming conflict fails instead of silently winning", {
  existing <- data.table(
    address_id = "a1", TYPEQU = "A", nearest_walk = 7
  )
  incoming <- data.table(
    address_id = "a1", TYPEQU = "A", nearest_walk = 8
  )

  expect_error(
    merge_address_monolith_rows(existing, incoming),
    "conflicting values for monolith key"
  )
})

test_that("base rows project through routing points and deduplicate buildings", {
  matrix_rows <- data.table(
    batiment_id = c("b1", "b2"), TYPEQU = c("A", "A"),
    nearest_walk = c(7, 7), count_5_walk = c(1L, 1L)
  )
  origin_link <- data.table(
    id = c("b1", "b2"), point_id = c("p1", "p1")
  )
  address_origins <- data.table(
    address_id = "a1", routing_point_id = "p1", routing_source = "existing"
  )

  got <- project_base_address_rows(
    matrix_rows, origin_link, address_origins
  )

  expect_equal(got[, .(address_id, TYPEQU, nearest_walk, count_5_walk)],
               data.table(address_id = "a1", TYPEQU = "A",
                          nearest_walk = 7, count_5_walk = 1L))
})

test_that("base rows with conflicting values at one routing point fail", {
  matrix_rows <- data.table(
    batiment_id = c("b1", "b2"), TYPEQU = c("A", "A"),
    nearest_walk = c(7, 8), count_5_walk = c(1L, 1L)
  )
  expect_error(
    project_base_address_rows(
      matrix_rows,
      data.table(id = c("b1", "b2"), point_id = c("p1", "p1")),
      data.table(address_id = "a1", routing_point_id = "p1",
                 routing_source = "existing")
    ),
    "conflicting values for duplicate monolith key"
  )
})

test_that("base projection refuses an origin that has no routing-point link", {
  matrix_rows <- data.table(
    batiment_id = "unlinked", TYPEQU = "A", nearest_walk = 7,
    count_5_walk = 1L
  )

  expect_error(
    project_base_address_rows(
      matrix_rows,
      data.table(id = "other", point_id = "p1"),
      data.table(address_id = "a1", routing_point_id = "p1",
                 routing_source = "existing")
    ),
    "base matrix rows missing origin-point links"
  )
})

test_that("atomic rows are renamed into the wide monolith family", {
  rows <- data.table(
    address_id = "a1", TYPEQU = "A", mode = "walk", tt_nearest = 7,
    count_5 = 1L, count_10 = 2L, count_15 = 3L, count_20 = 4L
  )

  got <- normalize_address_monolith_atomic_rows(rows, mode = "walk")

  expect_identical(
    names(got),
    c("address_id", "TYPEQU", "nearest_walk", "count_5_walk",
      "count_10_walk", "count_15_walk", "count_20_walk")
  )
  expect_equal(got$nearest_walk, 7)
  expect_equal(got$count_20_walk, 4L)
})

test_that("transit gains keep both percentile axes in the wide family", {
  rows <- data.table(
    address_id = "a1", TYPEQU = "A", mode = "transit_gain",
    walk_available = TRUE, time_gain_p1 = 1, time_gain_p50 = -1,
    count_gain_5_p1 = 0L, count_gain_10_p1 = 1L,
    count_gain_15_p1 = 2L, count_gain_20_p1 = 3L,
    count_gain_5_p50 = 0L, count_gain_10_p50 = 1L,
    count_gain_15_p50 = 1L, count_gain_20_p50 = 2L
  )

  got <- normalize_address_monolith_transit_gain_rows(rows)

  expect_true(all(grepl("^transit_gain_", names(got)[-c(1, 2)])))
  expect_equal(got$transit_gain_time_p50, -1)
  expect_equal(got$transit_gain_count_20_p1, 3L)
})

test_that("finalization joins address attributes onto sparse rows and zero-fills counts", {
  metrics <- data.table(
    address_id = "a1", TYPEQU = "A", nearest_walk = 7,
    count_5_walk = 1L
  )
  address_origins <- data.table(
    address_id = c("a1", "a2"), lon = c(-1.5, -1.6), lat = c(48.1, 48.2),
    n_non_dependance_constructions = c(1L, 2L),
    n_residential_groups = c(1L, 1L),
    routing_source = c("existing", "reroute"),
    routing_point_id = c("p1", "p2")
  )

  got <- finalize_address_monolith_rows(metrics, address_origins)

  expect_equal(nrow(got), 1L)
  expect_equal(got$address_id, "a1")
  expect_equal(got$n_linked_constructions, 1L)
  expect_equal(got$count_5_bike, 0L)
  expect_equal(got$nearest_bike, NA_real_)
  expect_false("routing_point_id" %in% names(got))
})

test_that("the filesystem builder combines base and reroute families into one file", {
  root <- tempfile("address-monolith-")
  dir.create(file.path(root, "plan"), recursive = TRUE)
  dir.create(file.path(root, "chunks"), recursive = TRUE)
  reroute_root <- file.path(root, "chunks", "reroutes")
  dir.create(file.path(reroute_root, "plan"), recursive = TRUE)
  dir.create(file.path(reroute_root, "walk"), recursive = TRUE)
  dir.create(file.path(reroute_root, "car"), recursive = TRUE)
  dir.create(file.path(reroute_root, "derived", "transit-gain"),
            recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  arrow::write_parquet(
    data.table(id = c("b1", "b2"), point_id = c("p1", "p1")),
    file.path(root, "plan", "origin_link.parquet")
  )
  arrow::write_parquet(
    data.table(
      address_id = c("a1", "a2"), lon = c(-1.5, -1.6), lat = c(48.1, 48.2),
      n_non_dependance_constructions = c(2L, 1L),
      n_residential_groups = c(1L, 1L),
      routing_source = c("existing", "reroute"),
      routing_point_id = c("p1", "p2")
    ),
    file.path(reroute_root, "plan", "address_origins.parquet")
  )
  arrow::write_parquet(
    data.table(
      batiment_id = c("b1", "b2"), TYPEQU = c("A", "A"), mode = "walk",
      tt_nearest = c(7, 7), count_5 = c(1L, 1L), count_10 = c(2L, 2L),
      count_15 = c(3L, 3L), count_20 = c(4L, 4L)
    ),
    file.path(root, "chunks", "walk_1.parquet")
  )
  arrow::write_parquet(
    data.table(
      batiment_id = "a2", TYPEQU = "B", mode = "walk", tt_nearest = 12,
      count_5 = 0L, count_10 = 0L, count_15 = 1L, count_20 = 2L
    ),
    file.path(reroute_root, "walk", "walk_1.parquet")
  )
  arrow::write_parquet(
    data.table(
      batiment_id = "a2", TYPEQU = "B", mode = "car", tt_nearest = 9,
      count_5 = 1L, count_10 = 1L, count_15 = 1L, count_20 = 1L
    ),
    file.path(reroute_root, "car", "car_2.parquet")
  )
  arrow::write_parquet(
    data.table(
      batiment_id = "a2", TYPEQU = "B", mode = "transit_gain",
      walk_available = TRUE, time_gain_p1 = 0, time_gain_p50 = 0,
      count_gain_5_p1 = 0L, count_gain_10_p1 = 0L,
      count_gain_15_p1 = 0L, count_gain_20_p1 = 0L,
      count_gain_5_p50 = 0L, count_gain_10_p50 = 0L,
      count_gain_15_p50 = 0L, count_gain_20_p50 = 0L
    ),
    file.path(reroute_root, "derived", "transit-gain", "transit_gain_1.parquet")
  )
  arrow::write_parquet(
    data.table(
      batiment_id = "a2", TYPEQU = "B", mode = "transit_gain",
      walk_available = FALSE, time_gain_p1 = NA_real_,
      time_gain_p50 = NA_real_, count_gain_5_p1 = 1L,
      count_gain_10_p1 = 1L, count_gain_15_p1 = 1L,
      count_gain_20_p1 = 1L, count_gain_5_p50 = 1L,
      count_gain_10_p50 = 1L, count_gain_15_p50 = 1L,
      count_gain_20_p50 = 1L
    ),
    file.path(reroute_root, "derived", "transit-gain", "transit_gain_2.parquet")
  )

  output_path <- file.path(root, "address-accessibility.parquet")
  build_address_accessibility_monolith(root, output_path)
  got <- data.table::as.data.table(arrow::read_parquet(output_path))

  expect_equal(nrow(got), 2L)
  expect_equal(got[address_id == "a1" & TYPEQU == "A", nearest_walk], 7)
  expect_equal(got[address_id == "a2" & TYPEQU == "B", nearest_walk], 12)
  expect_equal(got[address_id == "a2" & TYPEQU == "B", nearest_car], 9)
  expect_true(got[address_id == "a2" & TYPEQU == "B",
                  transit_gain_walk_available])
  expect_equal(got[address_id == "a2" & TYPEQU == "B",
                   transit_gain_count_5_p1], 0L)
  expect_equal(got[address_id == "a1" & TYPEQU == "A", count_5_bike], 0L)
  expect_true(all(!duplicated(got, by = c("address_id", "TYPEQU"))))
  expect_true(file.exists(paste0(output_path, ".checkpoints", "/base_0001.parquet")))
  expect_true(file.exists(paste0(
    output_path, ".checkpoints", "/reroute-metrics_0001.parquet"
  )))
})
