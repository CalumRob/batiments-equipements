# S2 — the synthetic fixture generator (the tracer's test discipline).

#' Build the tiny deterministic fixture.
#'
#' 4 buildings (b1–b4) over 2 communes (35101 "Commune Centre", 56101
#' "Commune Rurale") x 2 EPCI x 2 déps (35, 56); 8 TYPEQU — 7 kept
#' (B104, B105, D265, A129, C108, A203, B204) + 1 non-kept (F999); modes
#' walk + car. Every row is a literal, hand-designed so the S3/S4 worked
#' examples are verifiable on paper:
#'   b1 urban walk-rich · b2 walk-poor (car-dependent) · b3 rural (walk-isolated)
#'   b4 deep rural (nothing by walk, two types by car).
#' The fixture lives on the authoritative cap-and-ladder contract (#17):
#' count rungs are exactly 5/10/15/20 minutes and the cap is 20 — the sparse
#' contract means a type unreachable within the cap simply has NO row (b3/b4
#' carry no walk readings; b2's only walk type sits exactly at the cap edge,
#' which the ladder's top rung exercises). Returns list(matrix, crosswalk,
#' kept) — the matrix passes validate_matrix().
make_fixture <- function() {
  r <- function(b, t, mo, tt, c5, c10, c15, c20) {
    list(
      batiment_id = b, TYPEQU = t, mode = mo, tt_nearest = tt,
      count_5 = c5, count_10 = c10, count_15 = c15, count_20 = c20
    )
  }

  rows <- list(
    # ---- b1 walk (urban, walk-rich; B204/F999 beyond the cap -> no rows) ----
    r("b1", "B104", "walk",  8, 0L, 1L, 1L, 2L),
    r("b1", "B105", "walk", 12, 0L, 0L, 1L, 1L),
    r("b1", "D265", "walk", 15, 0L, 0L, 1L, 1L),
    r("b1", "A129", "walk",  6, 0L, 1L, 1L, 1L),
    r("b1", "C108", "walk", 20, 0L, 0L, 0L, 1L),
    r("b1", "A203", "walk", 18, 0L, 0L, 0L, 1L),
    # ---- b1 car ----
    r("b1", "B104", "car",  3, 1L, 2L, 2L, 2L),
    r("b1", "B105", "car",  4, 1L, 1L, 1L, 1L),
    r("b1", "D265", "car",  5, 1L, 1L, 1L, 1L),
    r("b1", "A129", "car",  2, 1L, 1L, 1L, 1L),
    r("b1", "C108", "car",  8, 0L, 1L, 1L, 1L),
    r("b1", "A203", "car",  6, 0L, 1L, 1L, 1L),
    r("b1", "B204", "car", 10, 0L, 1L, 1L, 1L),
    r("b1", "F999", "car", 12, 0L, 0L, 1L, 1L),
    # ---- b2 walk (walk-poor: one type barely inside the cap) ----
    r("b2", "B104", "walk", 20, 0L, 0L, 0L, 1L),
    # ---- b2 car (same as b1) ----
    r("b2", "B104", "car",  3, 1L, 2L, 2L, 2L),
    r("b2", "B105", "car",  4, 1L, 1L, 1L, 1L),
    r("b2", "D265", "car",  5, 1L, 1L, 1L, 1L),
    r("b2", "A129", "car",  2, 1L, 1L, 1L, 1L),
    r("b2", "C108", "car",  8, 0L, 1L, 1L, 1L),
    r("b2", "A203", "car",  6, 0L, 1L, 1L, 1L),
    r("b2", "B204", "car", 10, 0L, 1L, 1L, 1L),
    r("b2", "F999", "car", 12, 0L, 0L, 1L, 1L),
    # ---- b3 walk: none (walk-isolated — nothing reachable within the cap) --
    # ---- b3 car ----
    r("b3", "B104", "car", 12, 0L, 0L, 1L, 1L),
    r("b3", "B105", "car", 14, 0L, 0L, 1L, 1L),
    r("b3", "D265", "car", 16, 0L, 0L, 0L, 1L),
    r("b3", "A129", "car", 13, 0L, 0L, 1L, 1L),
    r("b3", "C108", "car", 19, 0L, 0L, 0L, 1L),
    # ---- b4: no walk rows at all; two car rows within the 20-min rung ----
    r("b4", "B104", "car", 18, 0L, 0L, 0L, 1L),
    r("b4", "D265", "car", 19, 0L, 0L, 0L, 1L)
  )

  matrix_dt <- data.table::rbindlist(rows)

  crosswalk <- data.table::data.table(
    batiment_id      = c("b1", "b2", "b3", "b4"),
    code_insee       = c("35101", "35101", "56101", "56101"),
    nom_commune      = c("Commune Centre", "Commune Centre", "Commune Rurale", "Commune Rurale"),
    epci             = c("EPCI Centre", "EPCI Centre", "EPCI Rural", "EPCI Rural"),
    code_departement = c("35", "35", "56", "56"),
    region           = rep("Bretagne", 4L)
  )

  kept <- c("B104", "B105", "D265", "A129", "C108", "A203", "B204")

  list(matrix = matrix_dt, crosswalk = crosswalk, kept = kept)
}
