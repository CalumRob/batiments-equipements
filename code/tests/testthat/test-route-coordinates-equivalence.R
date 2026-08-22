library(testthat)
library(data.table)

# The #20 equivalence proof: routing each exact coordinate ONCE then
# expanding back to identities must produce a matrix semantically identical
# to reference (non-deduplicated) routing — same rows, same values, same
# sparse reachability. Empty-SIRET and co-located distinct listings stay
# distinct; exact FULL duplicates collapse at listing creation only.
source(testthat::test_path("../../R/constants.R"), local = TRUE)
source(testthat::test_path("../../R/validate-matrix.R"), local = TRUE)
source(testthat::test_path("../../R/link.R"), local = TRUE)
source(testthat::test_path("../../R/prepare-destinations.R"), local = TRUE)
source(testthat::test_path("../../R/route-coordinates.R"), local = TRUE)

# A representative BPE universe slice:
#   * P1 = (-1.35, 48.10) hosts FOUR co-located listings with full identity
#     spread: two establishments (distinct SIRET/NOMRS), one of which hosts
#     two TYPEQU; plus two empty-SIRET listings with distinct TYPEQU;
#   * L1 is also present as an EXACT FULL duplicate universe row;
#   * L5/L6 are ordinary spread points; L6 sits far out (unreachable within
#     the stub cap -> sparse reachability).
make_dedup_fixture <- function() {
  p1 <- c(-1.35, 48.10)
  data.table::data.table(
    SIRET = c("11111111111111", "22222222222222", "", "",
              "11111111111111", "33333333333333", ""),
    NOMRS = c("Alpha", "Beta", "Gamma", "Delta",
              "Alpha", "Epsilon", "Zeta"),
    TYPEQU = c("B104", "D265", "C108", "A129",
               "B104", "B104", "B204"),
    LONGITUDE = c(rep(p1[[1L]], 5L), -1.40, -1.60),
    LATITUDE = c(rep(p1[[2L]], 5L), 48.12, 48.30),
    DEP = c("35", "35", "35", "35", "35", "35", "56"),
    DEPCOM = c("35101", "35101", "35101", "35101",
               "35101", "35101", "56101"),
    zone = c(rep("bretagne", 6L), "zone_frontaliere")
  )
}

make_dedup_origins <- function() {
  data.table::data.table(
    id = c("b1a", "b1b", "b2", "b3"),
    lon = c(-1.35, -1.35, -1.42, -2.00),
    lat = c(48.11, 48.11, 48.13, 48.60)
  )
}

derive_from_pairs <- function(pairs, dest_map, mode = "walk") {
  derive_matrix_rows(
    pairs[, .(from_id, to_id, travel_time)], dest_map, mode
  )
}

test_that("deduplicated routing equals reference routing on the representative fixture", {
  bpe <- make_dedup_fixture()
  origins <- make_dedup_origins()
  prep <- prepare_bpe_destinations_from_universe(bpe)

  # Reference path: route EVERY listing identity separately.
  reference_rows <- derive_from_pairs(
    stub_route_pairs(origins, prep$destinations), prep$dest_map
  )
  # Deduplicated path: route each exact coordinate once, expand, derive.
  dedup_out <- route_unique_coordinates(origins, prep$destinations,
                                        stub_route_pairs)
  dedup_rows <- derive_from_pairs(dedup_out$pairs, prep$dest_map)

  # Identical matrix semantics: same rows, same values (expect_equal on the
  # ordered contract shape covers keys, tt_nearest and every ladder count).
  expect_identical(names(reference_rows), names(dedup_rows))
  expect_equal(dedup_rows, reference_rows)

  # Identical sparse reachability: exactly the same (building, type) pairs
  # exist — nothing invented by expansion, nothing lost by dedup.
  ref_keys <- reference_rows[, .(batiment_id, TYPEQU)]
  dedup_keys <- dedup_rows[, .(batiment_id, TYPEQU)]
  expect_true(setequal(ref_keys, dedup_keys))
  expect_equal(anyDuplicated(ref_keys), 0L)
  # b2's only reachable types come through the cap edge; b3 reaches nothing.
  expect_equal(nrow(dedup_keys[batiment_id == "b3"]), 0L)
  expect_true(nrow(dedup_keys[batiment_id == "b2"]) > 0L)

  expect_silent(validate_matrix(reference_rows))
  expect_silent(validate_matrix(dedup_rows))

  # The reduction actually happened (#5): fewer coordinates routed than
  # identities carried.
  expect_equal(dedup_out$n_origins_input, 4L)
  expect_equal(dedup_out$n_origins_routed, 3L)
  expect_equal(dedup_out$n_destinations_input, 6L)
  expect_equal(dedup_out$n_destinations_routed, 3L)
})

test_that("co-located distinct listings and empty-SIRET rows keep their matrix identity", {
  prep <- prepare_bpe_destinations_from_universe(make_dedup_fixture())
  origins <- make_dedup_origins()
  rows <- derive_from_pairs(
    route_unique_coordinates(origins, prep$destinations, stub_route_pairs)$pairs,
    prep$dest_map
  )

  # P1 hosts Alpha(B104) + Beta(D265) + empty-SIRET Gamma(C108) and
  # Delta(A129); Epsilon(B104) sits alone at the spread point. A building
  # reaching P1 counts each co-located listing individually — and B104
  # twice in total: Alpha@P1 (its exact duplicate collapsed to one listing)
  # plus Epsilon@spread.
  b1a <- rows[batiment_id == "b1a"]
  expect_equal(b1a[TYPEQU == "B104", count_20], 2L)
  expect_equal(b1a[TYPEQU == "D265", count_20], 1L) # Beta@P1 alone
  expect_equal(b1a[TYPEQU == "C108", count_20], 1L) # empty-SIRET Gamma@P1
  expect_equal(b1a[TYPEQU == "A129", count_20], 1L) # empty-SIRET Delta@P1

  # Co-located buildings b1a/b1b read identically (modulo the id column).
  b1b <- rows[batiment_id == "b1b"]
  expect_equal(b1b[, .SD, .SDcols = -"batiment_id"],
               b1a[, .SD, .SDcols = -"batiment_id"])

  # The registry stays lossless: both universe copies of the exact full
  # duplicate link to ONE listing id.
  reg <- prep$registry
  dup_ids <- reg[SIRET == "11111111111111" & TYPEQU == "B104", unique(id)]
  expect_length(dup_ids, 1L)
  expect_equal(nrow(reg[SIRET == "11111111111111" & TYPEQU == "B104"]), 2L)
})
