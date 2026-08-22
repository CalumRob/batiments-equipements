library(testthat)
library(data.table)

# Coordinate-level routing (#20): each exact origin coordinate and each exact
# destination coordinate is routed ONCE, then the pairs expand back to every
# identity BEFORE derive-layer aggregation. Deduplication is exact
# coordinate-equality ONLY — no approximate snapping, no rounding-based
# grouping, no SIRET/NOMRS identity grouping. Source the modules directly so
# no routing dependency (r5r/JVM) is needed.
source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/route-coordinates.R"), local = TRUE)

test_that("co-located identities collapse to one routing point with a lossless link", {
  points <- data.table(
    id = c("b1a", "b1b", "b2"),
    lon = c(-1.35, -1.35, -1.40),
    lat = c(48.10, 48.10, 48.12)
  )
  plan <- coordinate_routing_plan(points)
  expect_equal(nrow(plan$points), 2L) # b1a/b1b share an exact coordinate
  expect_setequal(plan$points$lon, c(-1.35, -1.40))
  expect_equal(sort(plan$link$id), c("b1a", "b1b", "b2"))
  expect_equal(plan$link[id == "b1a", point_id], plan$link[id == "b1b", point_id])
  expect_true(plan$link[id == "b2", point_id] != plan$link[id == "b1a", point_id])
  # The routing table itself carries only point ids + coordinates.
  expect_named(plan$points, c("point_id", "lon", "lat"))
})

test_that("near-identical but distinct coordinates stay distinct — no rounding, no snapping", {
  eps <- 1e-12
  points <- data.table(
    id = c("p1", "p2", "p3"),
    lon = c(-1.35, -1.35 + eps, -1.36),
    lat = c(48.10, 48.10, 48.11)
  )
  plan <- coordinate_routing_plan(points)
  expect_equal(nrow(plan$points), 3L)
  expect_equal(length(unique(plan$link$point_id)), 3L)
})

test_that("the plan refuses NA coordinates and duplicate identity ids", {
  expect_error(
    coordinate_routing_plan(data.table(id = "x", lon = NA_real_, lat = 48)),
    "NA"
  )
  expect_error(
    coordinate_routing_plan(data.table(id = c("x", "x"), lon = c(1, 2), lat = c(48, 49))),
    "unique"
  )
})

test_that("pairs expand losslessly back to every identity on both sides", {
  origin_link <- data.table(
    id = c("oA1", "oA2", "oB"),
    point_id = c("coord_000001", "coord_000001", "coord_000002")
  )
  dest_link <- data.table(
    id = c("dX|T1", "dY|T1"),
    point_id = rep("coord_000003", 2L)
  )
  pairs <- data.table(
    from_id = c("coord_000001", "coord_000002"),
    to_id = rep("coord_000003", 2L),
    travel_time = c(7, 9)
  )

  got <- expand_pairs_to_identities(pairs, origin_link, dest_link)
  # 2 routed pairs x {2,1} origin identities x {2} destination identities.
  expect_equal(nrow(got), 6L)
  expect_setequal(got[from_id == "oA1" & to_id == "dX|T1", travel_time], 7)
  expect_setequal(got[from_id == "oB" & to_id == "dY|T1", travel_time], 9)
  expect_equal(nrow(got[to_id != "dX|T1" & to_id != "dY|T1"]), 0L)
  # No coordinate-level point id may leak into the expanded pairs.
  expect_false(any(grepl("^coord_", c(got$from_id, got$to_id))))
})

test_that("expansion preserves sparsity and carries extra pair columns through", {
  origin_link <- data.table(id = c("o1", "o2"), point_id = c("c1", "c1"))
  dest_link <- data.table(id = c("d1", "d2"), point_id = c("c9", "c9"))
  pairs <- data.table(
    from_id = "c1", to_id = "c9",
    travel_time_p1 = 6, travel_time_p50 = 8
  )
  got <- expand_pairs_to_identities(pairs, origin_link, dest_link)
  expect_equal(nrow(got), 4L)
  expect_true(all(got$travel_time_p1 == 6 & got$travel_time_p50 == 8))

  # A routed point id absent from its link is an internal inconsistency the
  # seam refuses loudly (it would otherwise silently lose pairs).
  expect_error(
    expand_pairs_to_identities(
      data.table(from_id = "c2", to_id = "c9", travel_time = 5L),
      origin_link, dest_link
    ),
    "absent from the origin link"
  )

  # A fully unreachable pass expands to a zero-row table with the right shape.
  empty <- expand_pairs_to_identities(pairs[0], origin_link, dest_link)
  expect_equal(nrow(empty), 0L)
  expect_named(empty, c("from_id", "to_id", "travel_time_p1", "travel_time_p50"))
})

test_that("route_unique_coordinates routes each exact coordinate once and returns identities", {
  origins <- data.table(
    id = c("b1a", "b1b", "b2"),
    lon = c(-1.35, -1.35, -1.40),
    lat = c(48.10, 48.10, 48.12)
  )
  destinations <- data.table(
    id = paste0("bpe_listing_00000", 1:3),
    lon = c(-1.35, -1.35, -1.50),
    lat = c(48.10, 48.10, 48.20)
  )

  seen <- list()
  route_fn <- function(o, d) {
    seen[[length(seen) + 1L]] <<- list(o = o, d = d)
    stub_route_pairs(o, d)
  }
  out <- route_unique_coordinates(origins, destinations, route_fn)

  # The router was called exactly once and saw the DEDUPLICATED tables:
  # 2 unique origin coordinates x 2 unique destination coordinates — never
  # the raw identities.
  expect_length(seen, 1L)
  expect_equal(nrow(seen[[1]]$o), 2L)
  expect_equal(nrow(seen[[1]]$d), 2L)

  # The returned stats record the reduction (#5).
  expect_equal(out$n_origins_input, 3L)
  expect_equal(out$n_origins_routed, 2L)
  expect_equal(out$n_destinations_input, 3L)
  expect_equal(out$n_destinations_routed, 2L)

  # Expansion restored every identity pair: co-located b1a/b1b both reach the
  # two co-located listings with identical (pure-function) travel times.
  expect_true(all(c("from_id", "to_id", "travel_time") %in% names(out$pairs)))
  expect_setequal(out$pairs$from_id, c("b1a", "b1b", "b2"))
  b1a <- out$pairs[from_id == "b1a"]
  b1b <- out$pairs[from_id == "b1b"]
  expect_setequal(b1a$to_id, b1b$to_id)
  expect_equal(b1a[order(to_id), travel_time], b1b[order(to_id), travel_time])
  expect_false(any(grepl("^coord_", c(out$pairs$from_id, out$pairs$to_id))))
})

test_that("the full-universe coordinate census records the expected reduction (#5)", {
  counts <- full_run_coordinate_counts()
  expect_equal(counts$origins$rows, 1664221L)
  expect_equal(counts$origins$unique_coordinates, 1424208L)
  expect_equal(counts$destinations$listings, 154417L)
  expect_equal(counts$destinations$unique_coordinates, 112073L)
  expect_true(counts$origins$unique_coordinates < counts$origins$rows)
  expect_true(counts$destinations$unique_coordinates < counts$destinations$listings)
})
