library(testthat)
library(data.table)

# Resume-compatibility identity (#21): COMPOSES existing seams rather than
# inventing a second system. The network_cache_identity fingerprint rides
# VERBATIM, plus the routing parameters (refusal-grade per D5 — a speed
# change is a re-run), the universe content keys, the plan census and the
# checkout git SHA. Refusal names the FIRST mismatched component (the
# probe_network_cache miss-reason pattern); allow_code_drift = TRUE documents
# deliberate continuation past a code change and NOTHING else.

identity_fixture <- function(git_sha = strrep("7", 40)) {
  list(
    version = 1L,
    network_fingerprint = strrep("a", 64),
    routing_parameters = list(
      walk_speed = 4,
      bike_speed = 12,
      max_trip_duration = cap_minutes(),
      elevation = "NONE",
      departure_datetime = NULL,
      time_window = 60L,
      percentiles = c(1L, 50L)
    ),
    universes = list(
      bdnb_residential = strrep("1", 64),
      bpe_destinations = strrep("2", 64)
    ),
    plan_census = list(chunk_size = 2L, n_chunks = 2L, n_origins = 5L,
                       n_origin_coords = 4L, n_destinations = 3L,
                       n_dest_coords = 2L),
    git_sha = git_sha
  )
}

test_that("an identical identity composes and raises no refusal", {
  expect_null(first_resume_mismatch(identity_fixture(), identity_fixture()))
})

test_that("the network fingerprint mismatch is named first", {
  found <- identity_fixture()
  found$network_fingerprint <- strrep("f", 64)
  hit <- first_resume_mismatch(identity_fixture(), found)
  expect_identical(hit$component, "network_fingerprint")
  expect_match(hit$reason, "network cache fingerprint")
  # The miss-reason pattern spells out both sides (probe_network_cache).
  expect_match(hit$reason, strrep("a", 64))
  expect_match(hit$reason, strrep("f", 64))
})

test_that("routing parameters are refusal-grade identity (D5)", {
  base <- identity_fixture()
  for (case in list(
    list(field = "walk_speed", value = 5),
    list(field = "bike_speed", value = 15),
    list(field = "max_trip_duration", value = 30L),
    list(field = "elevation", value = "native"),
    list(field = "time_window", value = 90L),
    list(field = "percentiles", value = c(10L, 50L)),
    list(field = "departure_datetime", value = "2026-03-03T08:00:00+0000")
  )) {
    found <- identity_fixture()
    found$routing_parameters[[case$field]] <- case$value
    hit <- first_resume_mismatch(base, found)
    expect_identical(hit$component,
                     paste0("routing_parameters.", case$field))
  }
  # The canonical departure string: NULL -> a datetime is a change too.
  found <- identity_fixture()
  found$routing_parameters$departure_datetime <- "2026-03-01T08:00:00+0000"
  hit <- first_resume_mismatch(base, found)
  expect_match(hit$component, "departure_datetime")
})

test_that("universe drift refuses resume (BDNB key or destination registry sha)", {
  base <- identity_fixture()
  found <- identity_fixture()
  found$universes$bdnb_residential <- strrep("9", 64)
  hit <- first_resume_mismatch(base, found)
  expect_match(hit$component, "universes.bdnb_residential")

  found <- identity_fixture()
  found$universes$bpe_destinations <- strrep("8", 64)
  hit <- first_resume_mismatch(base, found)
  expect_match(hit$component, "universes.bpe_destinations")
})

test_that("plan-census drift refuses resume — chunk_size re-cut is a new run", {
  base <- identity_fixture()
  found <- identity_fixture()
  found$plan_census$chunk_size <- 3L
  found$plan_census$n_chunks <- 2L
  hit <- first_resume_mismatch(base, found)
  expect_match(hit$component, "plan_census.chunk_size")

  found <- identity_fixture()
  found$plan_census$n_origin_coords <- 6L  # universe grew co-located duplicates? never silently
  hit <- first_resume_mismatch(base, found)
  expect_match(hit$component, "n_origin_coords|plan_census")
})

test_that("code drift is refused by default and overridable with allow_code_drift", {
  expected <- identity_fixture(git_sha = strrep("7", 40))
  found <- identity_fixture(git_sha = strrep("d", 40))

  hit <- first_resume_mismatch(expected, found)
  expect_identical(hit$component, "git_sha")
  expect_match(hit$reason, "code")

  expect_null(first_resume_mismatch(expected, found, allow_code_drift = TRUE))
})

test_that("the FIRST mismatched component wins the report order", {
  found <- identity_fixture()
  found$network_fingerprint <- strrep("f", 64)
  found$git_sha <- strrep("d", 40)
  found$routing_parameters$walk_speed <- 5
  hit <- first_resume_mismatch(identity_fixture(), found)
  expect_identical(hit$component, "network_fingerprint")

  found$network_fingerprint <- strrep("a", 64)  # network matches again
  hit <- first_resume_mismatch(identity_fixture(), found)
  expect_identical(hit$component, "routing_parameters.walk_speed")
})

test_that("the BDNB universe key hashes canonical coordinate lines", {
  o <- data.table::data.table(
    id = c("b2", "b1"),
    lon = c(-1.35, -1.35), lat = c(48.11, 48.11)
  )
  k1 <- bdnb_universe_key(o)
  k2 <- bdnb_universe_key(o[2:1])   # row order must not matter
  expect_identical(k1, k2)
  expect_match(k1, "^[0-9a-f]{64}$")
  o2 <- data.table::data.table(id = c("b2", "b1"),
                               lon = c(-1.3500001, -1.35),
                               lat = c(48.11, 48.11))
  expect_false(identical(k1, bdnb_universe_key(o2)))
})
