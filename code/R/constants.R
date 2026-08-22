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
