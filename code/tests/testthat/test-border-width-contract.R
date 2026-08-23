# Contract test for the ADR-0002 border width (#18).
#
# The cap-20 derivation yields 15 000 m, but the maintainer retained
# W = 25 000 m on 2026-08-23 as a deliberate safety margin (access-loss at
# the margins is the risk that matters; a wider strip only adds
# destinations). This test pins BOTH facts: the retained value and the fact
# that every strip-width default consumes the single named constant.

test_that("border_width_m pins the maintainer-retained value", {
  expect_identical(border_width_m(), 25000L)
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

test_that("the crop buffer composes the retained width with the snap margin", {
  expect_identical(crop_buffer(), 26600)
})
