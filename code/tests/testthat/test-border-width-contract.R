# Contract test for the ADR-0002 border width at the cap-20 acceptance
# (#18, maintainer decision 2026-08-23): W = 15 000 m — the fastest atomic
# mode's reach (car, ~45 km/h effective on OSM way speeds) at the 20-minute
# cap, rounded up; identical to the legacy chain's own cap-20 width. The 30-
# minute attempt's 25 km was a derivation at the wrong cap and is retired.

test_that("border_width_m pins the accepted cap-20 derivation", {
  expect_identical(border_width_m(), 15000L)
})

test_that("every strip-width default consumes the named constant", {
  # The contract is that no call site hard-codes a metre value: each default
  # must literally be border_width_m(), so a future re-derivation lands in
  # exactly one place.
  default_w <- function(f) deparse(formals(f)$W)
  consumers <- list(
    crop_buffer, osm_crop_polygon, osm_crop_network, read_osm_network,
    read_bpe_universe, prepare_bpe_destinations, run_tracer
  )
  for (f in consumers) {
    expect_identical(default_w(f), "border_width_m()",
                     info = sprintf("%s must default W to border_width_m()", deparse(substitute(f))))
  }
})

test_that("the crop buffer composes the accepted width with the snap margin", {
  expect_identical(crop_buffer(), 16600)
})
