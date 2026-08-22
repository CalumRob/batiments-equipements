library(testthat)
library(data.table)

# Driver-level consumption of the #20 seam, exercised WITHOUT the JVM:
#   * the chunk loop iterates over UNIQUE origin coordinates (global plan);
#   * every chunk's pairs expand back to identities before derivation;
#   * the dry-run metadata records the expected full-universe census.
# The routing itself goes through helper-stub-router.R (a pure function of
# the coordinates — the property that makes exact-coordinate dedup lossless).
source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/validate-matrix.R"), local = TRUE)
source(testthat::test_path("../../R/link.R"), local = TRUE)
source(testthat::test_path("../../R/full-run-inputs.R"), local = TRUE)
source(testthat::test_path("../../R/run-tracer.R"), local = TRUE)
source(testthat::test_path("../../R/prepare-destinations.R"), local = TRUE)
source(testthat::test_path("../../R/route-coordinates.R"), local = TRUE)

make_driver_origins <- function() {
  # b1a and b1b share an exact coordinate and sit in different positions so
  # the global plan is what keeps their coordinate in one chunk.
  data.table::data.table(
    id = c("b1a", "bx", "b1b", "b2", "b3"),
    lon = c(-1.35, -1.38, -1.35, -1.42, -2.00),
    lat = c(48.11, 48.12, 48.11, 48.13, 48.60)
  )
}

make_driver_destinations <- function() {
  p1 <- c(-1.35, 48.10)
  data.table::data.table(
    id = sprintf("bpe_listing_%06d", 1:3),
    lon = c(p1[[1L]], p1[[1L]], -1.40),
    lat = c(p1[[2L]], p1[[2L]], 48.12),
    TYPEQU = c("B104", "D265", "B204")
  )
}

test_that("chunking over unique origin coordinates stays equivalent to reference routing", {
  origins <- make_driver_origins()
  dests <- make_driver_destinations()

  # Reference: every identity against every listing, one pass.
  reference_rows <- derive_matrix_rows(
    stub_route_pairs(origins, dests[, .(id, lon, lat)]), dests[, .(id, TYPEQU)],
    "walk"
  )

  # Driver shape: ONE global plan; chunks over unique coordinates (chunk_size
  # 1 forces maximal fragmentation); expansion per chunk.
  origin_plan <- coordinate_routing_plan(origins, prefix = "coord_o")
  dest_plan <- coordinate_routing_plan(dests[, .(id, lon, lat)],
                                       prefix = "coord_d")
  chunk_pairs <- vector("list", nrow(origin_plan$points))
  for (i in seq_len(nrow(origin_plan$points))) {
    chunk <- origin_plan$points[i]
    routed <- stub_route_pairs(chunk, dest_plan$points)
    chunk_pairs[[i]] <- if (nrow(routed) > 0L) {
      expand_pairs_to_identities(routed, origin_plan$link, dest_plan$link)
    } else {
      routed
    }
  }
  pairs <- data.table::rbindlist(chunk_pairs, use.names = TRUE)
  chunked_rows <- derive_matrix_rows(pairs, dests[, .(id, TYPEQU)], "walk")

  expect_equal(chunked_rows, reference_rows)
  # The reduction held across chunk boundaries: 5 identities -> 4 unique
  # origin coordinates; 3 listings -> 2 unique destination coordinates.
  expect_equal(nrow(origin_plan$points), 4L)
  expect_equal(nrow(dest_plan$points), 2L)
})

test_that("the dry-run summary records the expected coordinate-dedup census (#20)", {
  pbf <- file.path(tempdir(), "dry-run-network.osm.pbf")
  writeLines(character(0), pbf)
  on.exit(unlink(pbf), add = TRUE)

  out <- run_tracer(network_pbf = pbf, modes = "walk", dry_run = TRUE)
  census <- out$coordinate_dedup$expected_full_run
  expect_equal(census$origins$rows, 1664221L)
  expect_equal(census$origins$unique_coordinates, 1424208L)
  expect_equal(census$destinations$listings, 154417L)
  expect_equal(census$destinations$unique_coordinates, 112073L)
  expect_identical(out$coordinate_dedup$rule, "exact (lon, lat) equality")
  # The cap-and-ladder contract rides alongside, unchanged (#17).
  expect_identical(out$cap_minutes, cap_minutes())
})
