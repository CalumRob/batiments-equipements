# Shared constants of the once-run's matrix contract.
# The matrix contract lives in the tracer run-strategy ("Matrix contract") and
# ADR-0002: long parquet, one row per (batiment_id, TYPEQU, mode atomique),
# tt_nearest + count ladder, sparse (rows only where reachable within the cap),
# type axis = Bretagne ∪ zone frontalière (never filtered to Bretagne).

#' The four atomic modes of the matrix (CONTEXT.md: mode atomique).
atomic_modes <- function() c("walk", "transit", "bike", "car")

#' The count ladder (run-strategy D4): 5/10/15/20/30 minutes.
ladder_rungs <- function() c(5L, 10L, 15L, 20L, 30L)

#' The ladder's count columns.
ladder_cols <- function() paste0("count_", ladder_rungs())

#' The cap: max_trip_duration (D4) — the hard ceiling on every reading.
cap_minutes <- function() max(ladder_rungs())

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
