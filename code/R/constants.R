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
#' up. At the restored 20-minute cap (#17/#18), car's effective average speed
#' (~45 km/h on OSM way speeds; r5r CAR takes no speed parameter) reaches
#' 15.0 km in 20 minutes -> W = 15 000 m, exactly the legacy chain's own
#' cap-20 width. Accepted by the maintainer 2026-08-23. Re-derive — never
#' silently inherit — if the cap or the mode speeds change.
border_width_m <- function() 15000L

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
