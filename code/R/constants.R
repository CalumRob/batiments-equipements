# Shared constants of the once-run's matrix contract.
# The matrix contract lives in the tracer run-strategy ("Matrix contract") and
# ADR-0002: long parquet, one row per (batiment_id, TYPEQU, mode atomique),
# tt_nearest + count ladder, sparse (rows only where reachable within the cap),
# type axis = Bretagne ∪ zone frontalière (never filtered to Bretagne).

#' The four atomic modes of the matrix (CONTEXT.md: mode atomique).
atomic_modes <- function() c("walk", "transit", "bike", "car")

#' The count ladder (run-strategy D4): 5/10/15/20 minutes — exactly four rungs.
#' 20 minutes is the authoritative once-run cap (#17): no code path, validator,
#' fixture, or metadata field may claim or emit a rung beyond it.
ladder_rungs <- function() c(5L, 10L, 15L, 20L)

#' The ladder's count columns.
ladder_cols <- function() paste0("count_", ladder_rungs())

#' The cap: max_trip_duration (D4) — the hard ceiling on every reading.
#' Derived from the ladder's top rung so cap and ladder cannot drift apart.
cap_minutes <- function() max(ladder_rungs())

#' The maximum number of transit rides per itinerary (D5 named parameter).
#'
#' Legacy-faithful 2 (linking_logic.R): within the 20-minute cap a third
#' boarding is physically marginal — time, not ride count, binds — while
#' each allowed ride multiplies R5's search states. Routing with a
#' different ride bound than the legacy snapshot would inject unexplained
#' drift into the deltas comparison, so this is a named constant like W
#' and the cap: change it deliberately, never silently.
max_transit_rides <- function() 2L

#' Departure-time draws per minute of the transit window (D5 parameter).
#'
#' 1 = one sampled departure per minute of the 60-minute window. These feeds
#' are schedule-based: timetables exist at minute granularity, so denser
#' sampling interpolates variation the data cannot express while multiplying
#' search cost linearly (measured #22h: 0.662 -> 0.400 s/origin, 1.66x).
#' r5r >= 2.4.0 defaults to 5; r5r 2.3.0 — the legacy's engine — did not
#' expose the knob and sampled at about this density, so 1 is also the
#' legacy-comparable setting for the deltas signal.
transit_draws_per_minute <- function() 1L

#' Refuse a routing window beyond the authoritative cap (#17).
#'
#' One guard consumed by every routing entry point (run_tracer and the r5r
#' wrappers in link.R): a window above cap_minutes() would emit rows whose
#' tt_nearest exceeds the cap — a claim the matrix contract forbids. The
#' once-run cap is 20 minutes; nothing routes beyond it.
assert_within_cap <- function(max_trip_duration) {
  if (!is.numeric(max_trip_duration) || length(max_trip_duration) != 1L ||
      is.na(max_trip_duration) || max_trip_duration <= 0) {
    stop("max_trip_duration must be one positive number of minutes", call. = FALSE)
  }
  if (max_trip_duration > cap_minutes()) {
    stop(sprintf(
      "max_trip_duration (%g min) exceeds the authoritative cap (%d min, D4): no code path may route beyond the cap",
      max_trip_duration, cap_minutes()
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' The ADR-0002 border-strip width W, in metres.
#'
#' Rule (ADR-0002): W = the fastest atomic mode's reach at the cap, rounded
#' up — at the accepted 20-minute cap that derivation yields 15 000 m (car,
#' ~45 km/h effective on OSM way speeds). MAINTAINER DECISION 2026-08-23:
#' retain **25 000 m** anyway. The pure derivation was rejected because
#' shrinking the strip risks losing real access at the margins; the wider
#' band keeps the acquisition envelope identical to the well-measured 30-
#' minute attempt and costs nothing correctness-wise (a wider strip only
#' adds destinations). W is therefore a deliberate safety margin ABOVE the
#' ADR-0002 minimum, not the derivation itself. Re-derive — never silently
#' inherit — if the cap or the mode speeds change.
border_width_m <- function() 25000L

#' The five product clusters (PRD: alimentation, santé, administration, école,
#' banque) with the legacy flagship's TYPEQU membership. The cluster *definitions*
#' are a derivation input (like the kept-list): #198's review may edit them;
#' the fixture tests the mechanism with these defaults.
cluster_defs <- function() {
  list(
    alimentation  = c("B104", "B105", "B201", "B202", "B207"),
    sante         = c("D265", "D307"),
    administration = c("A129", "A128", "A206"),
    ecole         = c("C108", "C109"),
    banque        = c("A203", "A206")
  )
}

#' Cluster flag column names (has_<cluster>).
cluster_flag_cols <- function() paste0("has_", names(cluster_defs()))

#' The recorded full-universe routing-coordinate census (#20).
#'
#' Coordinate-level routing (#20) routes each EXACT coordinate once; these are
#' the counts recorded from the pinned Bretagne acquisitions when the seam was
#' introduced — the full-universe identity rows vs unique routing coordinates:
#'   * origins: 1,664,221 BDNB residential origins -> 1,424,208 unique
#'     coordinates (WGS84 after the EPSG:2154 transform);
#'   * destinations: 154,417 BPE listings -> 112,073 unique coordinates
#'     (Bretagne + zone frontalière as measured under the 30-minute attempt's
#'     W = 25 km; the destinations side is W-dependent — re-record actuals
#'     under the accepted cap-20 / W = 15 000 m acquisition at #19/#23).
#' The reduction is the expected saving of the once-run's pair pass; a re-run
#' records its ACTUAL routed-coordinate counts in run_metadata.json next to
#'   these expectations. Deduplication remains exact-coordinate-equality only —
#'   no snapping, rounding, or identity grouping produced these numbers.
#' @export
full_run_coordinate_counts <- function() {
  list(
    origins = list(rows = 1664221L, unique_coordinates = 1424208L),
    destinations = list(listings = 154417L, unique_coordinates = 112073L)
  )
}
