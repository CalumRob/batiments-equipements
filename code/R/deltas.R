# S13 — the deltas report module: legacy-comparable territory aggregates vs the
# frozen snapshot (ticket 07).
#
# The per-rebuild regression signal the PRD demands ("Testing Decisions": a
# documented deltas report, not a silent overwrite). Territory-level aggregates
# derived from the real matrix (via S3's derivation layer) are compared against
# the frozen 2026-02-28 snapshot's values for the region's communes; every
# delta is classified expected / flag / missing with a reason naming the drift
# driver or the anomaly — nothing silently absorbed.
#
# Comparison contract (maintainer-approved, 2026-08-12):
#   - axis: car ONLY (the snapshot's `_c` columns); walk gets a documented
#     NON-COMPARABLE section (the legacy chain has no pure-walk axis — its `t`
#     mode is r5r WALK+TRANSIT, linking_logic.R:229-272)
#   - kept-list: the 53 legacy-RUTED TYPEQU codes (legacy_routed_types(),
#     ground truth = the car_* columns of Accessibility_by_mode_bretagne_v2.csv
#     = the run's all_desc) — NOT the flagship's 54 recorded
#     (kept_list_bpe2024()); C304/C305 + F101 divergences documented
#   - classification bands: shares & pct_iso_full ±0.10 abs · avg_diversity
#     ±25% rel · avg_total ±40% rel · nb_buildings ±30% rel (vs the measured
#     granularity prior, not vs 1) — beyond band + unexplained by a named drift
#     driver => FLAG
#   - commune level only

#' The 53 legacy-RUTED TYPEQU codes — the comparison kept-list.
#'
#' Ground truth = the `car_*` columns of the legacy routed CSV
#' (E:/Website/Data_handling/Accessibility_by_mode_bretagne_v2.csv, verified
#' 2026-08-12: exactly 53) = the legacy run's `all_desc` (linking_logic.R:
#' all_desc <- c(private_essential_codes, public_essential_codes,
#' missed_equips)). These are the codes the legacy actually ROUTED, hence the
#' codes its per-building car_div/car_tot and the snapshot's car aggregates
#' were computed over — the honest comparison set.
#'
#' This is NOT `kept_list_bpe2024()` (the flagship's RECORDED list, 54 codes):
#' the recorded list diverges in BOTH directions — it adds C304/C305 (never
#' routed; commented out in linking_logic.R's missed_equips) and omits F101
#' (which WAS routed). Both divergences are recorded (see
#' `legacy_kept_list_divergence()`); the comparison itself uses the 53 routed
#' codes on both sides.
legacy_routed_types <- function() {
  c(
    "D267", "A128", "A129", "A203", "A206", "A207", "A208",
    "A401", "A404", "A405", "A501", "A504",
    "B103", "B104", "B105", "B201", "B202", "B204", "B206", "B207",
    "B208", "B209", "B210", "B302", "B303", "B304", "B306", "B307",
    "B312", "B313", "B318", "B324", "B325",
    "C107", "C108", "C109", "C201", "C301", "C302", "C303",
    "D265", "D269", "D270", "D277", "D307", "D403", "D502",
    "F101", "F111", "F116", "F121", "F303", "F307"
  )
}

#' The recorded-vs-routed kept-list divergences (documented, never compared).
#'
#' The flagship's RECORDED kept list (`equipements_retenus.csv` =
#' `kept_list_bpe2024()`, 54 codes) vs what was actually routed
#' (`legacy_routed_types()`, 53 codes):
#'   - recorded_but_not_routed: C304, C305 — "Autres lycées", commented out of
#'     linking_logic.R's missed_equips (the #168 line's "#C304", "#C305")
#'   - routed_but_not_recorded: F101 — piscine, present in the routed CSV but
#'     absent from equipements_retenus.csv (the flagship kept its 54 by
#'     overwriting the routed list; F101's routing row exists nonetheless)
#' The kept-list review (#198) decides the BPE 2025 answer; the deltas
#' comparison does NOT re-litigate it — both sides use the 53 routed codes.
legacy_kept_list_divergence <- function() {
  list(
    recorded_but_not_routed = c("C304", "C305"),
    routed_but_not_recorded = c("F101")
  )
}

#' The comparable snapshot column map.
#'
#' One row per comparable pair (our derivation column <-> snapshot column),
#' with `comparable` FALSE for the columns the report documents but never
#' compares (walk has no snapshot counterpart — the legacy `t` axis is
#' WALK+TRANSIT; the vulnerability / decile / rank / top-3 / loss / level
#' families are legacy-only derivations). Columns: `metric` (our aggregate
#' column), `snapshot` (snapshot column), `comparable` (logical), `note`.
legacy_snapshot_map <- function() {
  data.frame(
    metric = c(
      "nb_buildings",
      "share_alimentation", "share_sante", "share_administration",
      "share_ecole", "share_banque",
      "avg_diversity", "avg_total", "pct_iso_full",
      # --- walk rows: NON-COMPARABLE (documented, never compared) ---
      "nb_buildings", "share_alimentation", "share_sante",
      "share_administration", "share_ecole", "share_banque",
      "avg_diversity", "avg_total", "pct_iso_full"
    ),
    snapshot = c(
      "nb_buildings",
      "share_food_c", "share_health_c", "share_admin_c",
      "share_school_c", "share_bank_c",
      "avg_div_car", "avg_tot_car", "pct_iso_full_c",
      # walk rows have NO snapshot counterpart (legacy `t` = WALK+TRANSIT)
      rep(NA_character_, 9L)
    ),
    comparable = c(
      rep(TRUE, 9L),
      rep(FALSE, 9L)
    ),
    note = c(
      "origin count: legacy batiment_groupe universe (2025-07, deprecated usage filter) vs ours batiment_construction origins (2026-02.a) — canonical-usage unknown coverage is excluded and the measured granularity prior applies",
      "mean has_alimentation (car, 20 min) — cluster_defs alimentation = B104,B105,B201,B202,B207 (score_computation.R:213)",
      "mean has_sante (car, 20 min) — cluster_defs sante = D265,D307 (score_computation.R:217)",
      "mean has_administration (car, 20 min) — cluster_defs administration = A129,A128,A206 (score_computation.R:221)",
      "mean has_ecole (car, 20 min) — cluster_defs ecole = C108,C109 (score_computation.R:225)",
      "mean has_banque (car, 20 min) — cluster_defs banque = A203,A206 (score_computation.R:229)",
      "mean distinct kept TYPEQU within 20 min (car) — legacy car_diversity = rowSums(car_* > 0) (score_computation.R:201)",
      "mean establishments within 20 min (car) — legacy car_total = rowSums(car_*) (score_computation.R:206)",
      "share of buildings with zero car total — legacy pct_iso_full_c = sum(car_tot==0)/.N (summarizing.R:163); 0 by definition in the car axis",
      "walk mode: NON-COMPARABLE — the legacy chain has no pure-walk axis; `t` = r5r WALK+TRANSIT composite (linking_logic.R:229-272)",
      "walk mode: NON-COMPARABLE — legacy share_food_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy share_health_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy share_admin_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy share_school_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy share_bank_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy avg_div_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy avg_tot_t is the walk+transit composite's reading, not a pure-walk one",
      "walk mode: NON-COMPARABLE — legacy pct_iso_full_t is the walk+transit composite's reading, not a pure-walk one"
    ),
    stringsAsFactors = FALSE
  )
}

#' The named drift sources behind every delta (the regression signal's why).
#'
#' Each rebuild's deltas report names which of these drivers explains each
#' delta. A delta beyond its classification band is FLAGGED unless one of
#' these explains it (maintainer-approved rule 3). `affects` lists the metric
#' families the driver moves; `direction` the expected sign.
legacy_drift_sources <- function() {
  data.frame(
    driver = c(
      "BPE content 2024 -> 2025 (nomenclature + establishments)",
      "origin coverage + granularity (canonical usage; batiment_groupe -> batiment_construction)",
      "origin geometry policy (BAN point, then non-fictive centroids/fallback)",
      "BPE listing identity (lossless registry; exact duplicates only)",
      "BDNB vintage 2025-07 -> 2026-02.a",
      "OSM vintage (network)",
      "border width 15 km -> 25 km (ADR-0002)",
      "kept-list 54-recorded vs 53-routed (C304/C305/F101)"
    ),
    affects = c(
      "avg_diversity, avg_total, shares (genuine BPE content change)",
      "nb_buildings, shares (unknown canonical-usage coverage + origin granularity)",
      "shares, avg_diversity, avg_total (geometry policy; source shifts are possible, not an old geom swap)",
      "avg_total and shares where co-located listings differ (identity effect, not new BPE content)",
      "nb_buildings, shares (universe composition)",
      "avg_diversity, avg_total, shares",
      "avg_diversity, avg_total, shares (border communes gain cross-border reach)",
      "documented, never compared (both sides use the 53 routed codes)"
    ),
    direction = c(
      "up (new establishments / types)",
      "mixed (unknown coverage excluded; more constructions per covered group)",
      "mixed (policy-selected source; small positional changes)",
      "mixed (listing identity changes counts without changing BPE content)",
      "mixed",
      "mixed",
      "up (border communes)",
      "none (comparison is 53 vs 53)"
    ),
    stringsAsFactors = FALSE
  )
}

#' The approved classification bands (maintainer-approved rule 3).
#'
#' Named list; `abs` bands apply to |delta_abs|, `rel` bands to |delta_rel|.
#' nb_buildings is special: its band applies to the deviation from the
#' MEASURED granularity prior (see derive_deltas), not from 1.
legacy_delta_bands <- function() {
  list(
    share        = list(type = "abs", limit = 0.10),
    pct_iso_full = list(type = "abs", limit = 0.10),
    avg_diversity = list(type = "rel", limit = 0.25),
    avg_total    = list(type = "rel", limit = 0.40),
    nb_buildings = list(type = "rel", limit = 0.30)
  )
}

#' Ticket 07 findings accepted by the maintainer for the corrected comparison.
#'
#' This is deliberately a data table rather than a commune-specific branch in
#' `.classify_row()`. Callers opt in by passing this table to the classifier.
legacy_accepted_findings <- function() {
  data.frame(
    code_insee = c("35174", "35357", "35230"),
    metric = c("avg_total", "avg_total", "nb_buildings"),
    reason = c(
      "Ticket 07 measured Mellé's frontier share at about 37.7%, within the observed Fougères border-commune range [19.4%, 42.4%]; the large avg_total delta is a second-ring consequence of ADR-0002's 15 km -> 25 km strip/extent change, not an unexplained BPE anomaly",
      "Ticket 07 measured Villamée's frontier share at about 33.7%, within the observed Fougères border-commune range [19.4%, 42.4%]; the large avg_total delta is a second-ring consequence of ADR-0002's 15 km -> 25 km strip/extent change, not an unexplained BPE anomaly",
      "Ticket 07 measured the legacy-filter/address-equivalent group universe at about 149, close to the snapshot's 159; corrected canonical BDNB policy plus construction granularity yields 298 origins. This is accepted origin-universe semantic drift, not an unresolved routing failure"
    ),
    stringsAsFactors = FALSE
  )
}

.validate_accepted_findings <- function(accepted_findings) {
  if (is.null(accepted_findings)) return(invisible(NULL))
  required <- c("code_insee", "metric", "reason")
  missing <- setdiff(required, names(accepted_findings))
  if (length(missing) > 0L) {
    stop(sprintf("accepted_findings missing columns: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  if (anyNA(accepted_findings[["code_insee"]]) || anyNA(accepted_findings[["metric"]]) ||
      anyNA(accepted_findings[["reason"]]) || any(!nzchar(as.character(accepted_findings[["reason"]])))) {
    stop("accepted_findings must contain non-empty code_insee, metric, and reason values", call. = FALSE)
  }
  invisible(NULL)
}

#' Read the frozen snapshot into a tidy, keyed data.table.
#'
#' `path` = the 2026-02-28 vintage snapshot CSV (2,061 columns; only the
#' comparable-mapped columns + code_insee/nom_commune are read). `code_insee`
#' (optional) restricts to the toy communes. `code_insee` is normalised to the
#' 5-char zero-padded form (the CSV carries it as a bare integer); the mapped
#' numeric columns are coerced to double. Returns a data.table keyed by
#' `code_insee`, one row per commune. Encoding: UTF-8 (verified against the
#' snapshot's nom_commune — "Fougères" et al. render correctly; do NOT fall
#' back to Latin-1 on this artifact).
read_legacy_snapshot <- function(path, code_insee = NULL) {
  if (!file.exists(path)) {
    stop("snapshot not found: ", path, call. = FALSE)
  }
  map <- legacy_snapshot_map()
  snap_cols <- unique(map[["snapshot"]][!is.na(map[["snapshot"]])])
  keep <- c("code_insee", "nom_commune", snap_cols)

  snap <- data.table::fread(path, encoding = "UTF-8", select = keep)

  # code_insee normalisation: the CSV reads as integer (leading zeros lost).
  snap[, code_insee := sprintf("%05d", as.integer(code_insee))]
  if (!is.null(code_insee)) {
    codes <- as.character(code_insee)
    snap <- snap[code_insee %in% codes]
  }

  for (col in snap_cols) {
    if (col %in% names(snap)) {
      data.table::set(snap, j = col, value = as.numeric(snap[[col]]))
    }
  }

  data.table::setkey(snap, code_insee)
  snap[]
}

#' Compute the deltas table: one row per (code_insee x comparable metric).
#'
#' `derived_agg` = the output of `derive_territory_aggregates()` (commune
#' level; the `ref_mode` row is used). `snapshot` = the output of
#' `read_legacy_snapshot()`. `map` = `legacy_snapshot_map()` (only
#' `comparable == TRUE` rows enter the table). `bands` =
#' `legacy_delta_bands()`. `nb_prior` = the measured granularity prior
#' (default: sum(derived nb_buildings) / sum(snapshot nb_buildings) — the
#' 2026-08-12 measured value is ~1.42, i.e. 25,867 constructions vs the
#' legacy snapshot's 18,262). `border_communes` = INSEE codes whose
#' beyond-band avg_diversity/avg_total deltas are explained by the ADR-0002
#' border-widening driver (for the toy region: the 9 border communes of
#' Fougères Agglo).
#'
#' Result columns: code_insee, nom_commune, metric, snapshot, derived,
#' delta_abs, delta_rel (NA where snapshot == 0), classification
#' (expected / flag / missing), reason. `classification == "missing"` when the
#' commune has no snapshot row. Non-comparable metrics are EXCLUDED (they get
#' the walk section of the report instead).
derive_deltas <- function(derived_agg, snapshot, map = legacy_snapshot_map(),
                          bands = legacy_delta_bands(),
                          ref_mode = "car",
                          nb_prior = NULL,
                          border_communes = NULL,
                          accepted_findings = NULL) {
  .validate_accepted_findings(accepted_findings)
  agg <- data.table::as.data.table(derived_agg)
  snap <- data.table::as.data.table(snapshot)
  cmp <- map[map[["comparable"]], ]

  if (!"mode" %in% names(agg)) {
    stop("derived_agg must have a mode column (derive_territory_aggregates output)", call. = FALSE)
  }
  if (!ref_mode %in% unique(agg[["mode"]])) {
    stop(sprintf("ref_mode '%s' absent from derived_agg", ref_mode), call. = FALSE)
  }
  agg <- agg[mode == ref_mode]

  # Measured granularity prior (default): the aggregate derived/snapshot
  # origin ratio — the first honest measure of what the re-made chain's
  # origin universe does to nb_buildings (2026-08-12: ~1.42).
  if (is.null(nb_prior)) {
    nb_prior <- sum(agg[["nb_buildings"]], na.rm = TRUE) /
      sum(snap[["nb_buildings"]], na.rm = TRUE)
  }
  nb_band <- bands[["nb_buildings"]][["limit"]]

  border <- if (is.null(border_communes)) character(0) else as.character(border_communes)

  # nom_commune lookup from the snapshot.
  names_snap <- if ("nom_commune" %in% names(snap)) {
    unique(snap[, .(code_insee, nom_commune)])
  } else {
    data.table::data.table(code_insee = character(0), nom_commune = character(0))
  }

  out_rows <- vector("list", nrow(cmp))
  for (i in seq_len(nrow(cmp))) {
    metric <- cmp[["metric"]][i]
    snap_col <- cmp[["snapshot"]][i]
    if (!metric %in% names(agg)) next
    if (!snap_col %in% names(snap)) next

    a <- agg[, c("code_insee", metric), with = FALSE]
    s <- snap[, c("code_insee", snap_col), with = FALSE]
    data.table::setnames(a, metric, "derived")
    data.table::setnames(s, snap_col, "snapshot")
    dt <- merge(a, s, by = "code_insee", all = TRUE)
    dt <- merge(dt, names_snap, by = "code_insee", all.x = TRUE)
    dt[, metric := metric]

    dt[, delta_abs := derived - snapshot]
    dt[, delta_rel := ifelse(snapshot == 0, NA_real_, (derived - snapshot) / snapshot)]

    band <- bands[[.band_key(metric)]]
    for (r in seq_len(nrow(dt))) {
      cl <- .classify_row(
        metric = metric,
        snap_val = dt[["snapshot"]][r],
        derived_val = dt[["derived"]][r],
        band = band,
        nb_prior = nb_prior,
        nb_band = nb_band,
        is_border = dt[["code_insee"]][r] %in% border,
        accepted_finding = if (!is.null(accepted_findings)) {
          accepted_findings[accepted_findings[["code_insee"]] == dt[["code_insee"]][r] &
                             accepted_findings[["metric"]] == metric, , drop = FALSE]
        } else NULL
      )
      data.table::set(dt, r, "classification", cl$classification)
      data.table::set(dt, r, "reason", cl$reason)
    }

    data.table::setcolorder(dt, c(
      "code_insee", "nom_commune", "metric", "snapshot", "derived",
      "delta_abs", "delta_rel", "classification", "reason"
    ))
    out_rows[[i]] <- dt
  }

  out <- data.table::rbindlist(out_rows, use.names = TRUE, fill = TRUE)
  data.table::setorderv(out, c("metric", "code_insee"))
  out[]
}

# Map a metric to its band key (the five cluster shares share one band).
.band_key <- function(metric) {
  if (grepl("^share_", metric)) return("share")
  metric
}

# Classify one (commune x metric) delta against its band + the drift drivers.
# Returns list(classification, reason). The rule (maintainer-approved): within
# band -> expected; beyond band + explained by a named driver -> expected;
# beyond band + unexplained -> flag (never silently absorbed).
.classify_row <- function(metric, snap_val, derived_val, band, nb_prior, nb_band,
                           is_border, accepted_finding = NULL) {
  if (is.na(snap_val)) {
    return(list(
      classification = "missing",
      reason = "commune absent from the frozen snapshot"
    ))
  }
  if (is.na(derived_val)) {
    return(list(
      classification = "missing",
      reason = "no derived reading (no buildings in the matrix for this commune)"
    ))
  }

  if (!is.null(accepted_finding) && nrow(accepted_finding) > 0L) {
    return(list(
      classification = "expected",
      reason = paste0("accepted prior measured finding (Ticket 07): ",
                      accepted_finding[["reason"]][1L])
    ))
  }

  delta_abs <- derived_val - snap_val
  delta_rel <- if (snap_val == 0) NA_real_ else delta_abs / snap_val

  if (metric == "nb_buildings") {
    ratio <- derived_val / snap_val
    dev <- abs(ratio - nb_prior) / nb_prior
    if (dev <= nb_band) {
      return(list(
        classification = "expected",
        reason = sprintf(
          "within %.0f%% of the measured granularity prior (%.2f): origin batiment_groupe -> batiment_construction (legacy 2025-07 universe vs ours 2026-02.a)",
          100 * nb_band, nb_prior
        )
      ))
    }
    return(list(
      classification = "flag",
      reason = sprintf(
        "ratio %.2f deviates from the measured granularity prior %.2f by %.0f%% (>%.0f%%) — legacy snapshot under/over-coverage for this commune (2025-07 universe and deprecated usage filter); canonical-usage unknown coverage is excluded from ours; verify against the ticket-05 group count",
        ratio, nb_prior, 100 * dev, 100 * nb_band
      )
    ))
  }

  if (is.null(band) || is.na(band$limit)) {
    return(list(classification = "flag", reason = "no band defined for metric"))
  }

  in_band <- if (identical(band$type, "abs")) {
    abs(delta_abs) <= band$limit
  } else {
    !is.na(delta_rel) && abs(delta_rel) <= band$limit
  }

  if (in_band) {
    return(list(
      classification = "expected",
      reason = .within_band_reason(metric, band, delta_abs, delta_rel, nb_prior)
    ))
  }

  # Beyond band: does a named drift driver explain it?
  if (metric %in% c("avg_diversity", "avg_total") && is_border && !is.na(delta_rel) && delta_rel > 0) {
    return(list(
      classification = "expected",
      reason = sprintf(
        "beyond band (+%.0f%%) but explained by border widening 15->25 km (ADR-0002): border commune gains cross-border establishments within 20 min car reach (the legacy's 15 km strip + silent 1.6 km r5r snap drop understated it)",
        100 * delta_rel
      )
    ))
  }

  list(
    classification = "flag",
    reason = .flag_reason(metric, band, delta_abs, delta_rel, is_border)
  )
}

# The reason for a within-band expected delta — names the primary driver.
.within_band_reason <- function(metric, band, delta_abs, delta_rel, nb_prior) {
  lim <- band$limit
  if (grepl("^share_", metric)) {
    sprintf("within ±%.2f abs (delta %.3f): car access saturates near 1.0; BPE 2025 establishment drift within band", lim, delta_abs)
  } else if (metric == "pct_iso_full") {
    "within ±0.10 abs: car-vs-car isolation is 0 by definition (summarizing.R:163: pct_iso_full_c = sum(car_tot==0)/.N) — both sides 0"
  } else if (metric == "avg_diversity") {
    sprintf("within ±%.0f%% rel (delta %+.1f%%): BPE 2024->2025 nomenclature + establishments; kept-list = 53 routed codes both sides", 100 * lim, 100 * delta_rel)
  } else if (metric == "avg_total") {
    sprintf("within ±%.0f%% rel (delta %+.1f%%): BPE 2025 establishments (nomenclature + counts) + border widening", 100 * lim, 100 * delta_rel)
  } else {
    sprintf("within band (delta_abs %+.3f)", delta_abs)
  }
}

# The reason for a beyond-band FLAG — names the anomaly + a hypothesis.
.flag_reason <- function(metric, band, delta_abs, delta_rel, is_border) {
  lim <- band$limit
  if (grepl("^share_", metric)) {
    sprintf("share delta %+.3f beyond ±%.2f abs — no named driver moves a cluster share that far; verify at TYPEQU level", delta_abs, lim)
  } else if (metric == "pct_iso_full") {
    sprintf("pct_iso_full delta %+.3f beyond ±%.2f abs — car-vs-car isolation must be 0 by definition; investigate", delta_abs, lim)
  } else if (metric == "avg_diversity") {
    sprintf("avg_diversity %+.0f%% beyond ±%.0f%% rel — hypothesis: BPE 2025 new establishments / nomenclature within 20 min reach; verify at TYPEQU level", 100 * delta_rel, 100 * lim)
  } else if (metric == "avg_total") {
    sprintf("avg_total %+.0f%% beyond ±%.0f%% rel — hypothesis: BPE 2025 establishment growth within 20 min reach; verify at TYPEQU level", 100 * delta_rel, 100 * lim)
  } else {
    sprintf("delta %+.3f beyond band", delta_abs)
  }
}

#' Render the deltas report as markdown (character vector).
#'
#' Sections: header (purpose, vintage stamps, scope, axis, reference frame,
#' date) · method (mapping table, kept-list 53-vs-54 note, definitional
#' equality with legacy file:line citations, drift sources, bands) · per-commune
#' deltas tables · expected-drift commentary per metric family · FLAGGED
#' anomalies list · walk non-comparison section · legacy-only columns ·
#' reuse note · next steps. `deltas` = `derive_deltas()` output; `walk_agg` =
#' the walk-mode rows of `derive_territory_aggregates()` (shown as the first
#' pure-walk reading); `snapshot_path` is echoed in the reuse note.
render_deltas_report <- function(deltas,
                                 walk_agg = NULL,
                                 map = legacy_snapshot_map(),
                                  bands = legacy_delta_bands(),
                                  drift = legacy_drift_sources(),
                                  border_communes = NULL,
                                  nb_prior = NULL,
                                 snapshot_path = NULL,
                                 scope = "28 communes of CA Fougères Agglomération (dép 35)",
                                 threshold = 20L,
                                 snapshot_vintage = "BPE 2024 · OSM 02-2026 · BDNB 2025-07",
                                  derived_vintage = "BPE 2025 · OSM 02-2026 · BDNB 2026-02.a",
                                   kept = legacy_routed_types(),
                                   date = Sys.Date(),
                                   accepted_findings = NULL) {
  d <- data.table::as.data.table(deltas)
  .validate_accepted_findings(accepted_findings)
  required <- c("code_insee", "metric", "classification", "reason")
  missing_columns <- setdiff(required, names(d))
  if (length(missing_columns) > 0L) {
    stop(sprintf("deltas missing report columns: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  if (anyNA(d[["classification"]]) || any(!d[["classification"]] %in% c("expected", "flag", "missing"))) {
    stop("deltas contain an unclassified row; missing classifications must be explicit", call. = FALSE)
  }
  if (anyNA(d[["reason"]]) || any(!nzchar(as.character(d[["reason"]])))) {
    stop("deltas contain a row without an explanatory reason", call. = FALSE)
  }
  if (is.null(nb_prior)) {
    nb_prior <- sum(d[metric == "nb_buildings", derived], na.rm = TRUE) /
      sum(d[metric == "nb_buildings", snapshot], na.rm = TRUE)
  }
  cmp <- map[map[["comparable"]], ]
  n_exp <- sum(d[["classification"]] == "expected", na.rm = TRUE)
  n_flag <- sum(d[["classification"]] == "flag", na.rm = TRUE)
  n_miss <- sum(d[["classification"]] == "missing", na.rm = TRUE)

  lines <- character(0)
  add <- function(...) lines <<- c(lines, ...)

  # --- header ---------------------------------------------------------------
  add(sprintf("# Rapport d'écarts — territory aggregates vs the frozen snapshot (2026-02-28 vintage)"))
  add("")
  add("> **Purpose.** The per-rebuild regression signal the PRD demands (PRD §Testing Decisions: \"a documented deltas report, not a silent overwrite\"). Territory-level aggregates derived from the real matrix (via the S3 derivation layer) compared against the frozen snapshot for the toy region's communes — the first honest measure of what the re-made chain changes.")
  add("")
  add(sprintf("> **Vintages.** Snapshot: **%s** (frozen 2026-02-28). Ours: **%s**. BPE 2025-vs-2024 deltas are EXPECTED and documented — never silently overwritten.", snapshot_vintage, derived_vintage))
  add(sprintf("> **Scope.** %s. Commune level only (the snapshot holds one row per commune; **no 9-digit EPCI row exists** — verified: 1,200 rows, all 5-digit `code_insee`, `region = Bretagne` — so commune-level is the deliverable and EPCI-level columns are legacy-only).", scope))
  add("> **Axis.** Car is the comparable axis (the snapshot's `_c` columns). Walk is **non-comparable** — the legacy chain has no pure-walk axis (its `t` mode is r5r WALK+TRANSIT); our walk readings are shown in a dedicated section as the first pure-walk measurement.")
  add(sprintf("> **Reference frame.** Threshold **%d min**; kept-list = the **53 legacy-routed TYPEQU codes** (both sides); `ref_mode = car`; derivation layer S3 (`derive_building_metrics` + `derive_territory_aggregates`).", threshold))
  add(sprintf("> **Date.** %s. Module: `code/R/deltas.R` (S13).", format(as.Date(date), "%Y-%m-%d")))
  add("")

  # --- method ---------------------------------------------------------------
  add("## Method")
  add("")
  add("### The comparable column map")
  add("")
  add("| Our metric | Snapshot column | Comparable | Note |")
  add("|---|---|---|---|")
  for (i in seq_len(nrow(cmp))) {
    add(sprintf("| `%s` | `%s` | %s | %s |",
                cmp[["metric"]][i], cmp[["snapshot"]][i],
                if (cmp[["comparable"]][i]) "YES" else "no",
                cmp[["note"]][i]))
  }
  add("")
  add("The walk-mode rows are documented **non-comparable**: the legacy chain's `_t` axis is a WALK+TRANSIT composite (`linking_logic.R:229-272` — `modes_to_run$transit_walk = c(\"WALK\", \"TRANSIT\")`, percentiles=1, cap 20, walk_speed 4). There is **no pure-walk axis anywhere in the legacy chain** (the routed CSV's prefixes are only bike/car/transit), so our walk readings have no snapshot counterpart.")
  add("")

  add("### The kept-list: 53 routed vs 54 recorded")
  add("")
  add("The comparison kept-list is the **53 legacy-RUTED TYPEQU codes** (`legacy_routed_types()`), ground truth = the `car_*` columns of `Accessibility_by_mode_bretagne_v2.csv` (verified: exactly 53) = the legacy run's `all_desc` (`linking_logic.R`). The flagship's RECORDED list (`equipements_retenus.csv` = `kept_list_bpe2024()`, 54 codes) diverges in **both** directions:")
  add("")
  kd <- legacy_kept_list_divergence()
  add(sprintf("- **recorded but never routed: %s** — \"Autres lycées\", commented out of `linking_logic.R`'s `missed_equips` (the `#\"C304\", \"C305\"` line). They have no routing rows, so no car_* column, so no snapshot reading.", paste(kd$recorded_but_not_routed, collapse = ", ")))
  add(sprintf("- **routed but never recorded: %s** — piscines, present in the routed CSV but absent from `equipements_retenus.csv`.", paste(kd$routed_but_not_recorded, collapse = ", ")))
  add("")
  add("Both divergences are **recorded, never compared**: the deltas comparison uses the 53 routed codes on both sides, so the kept-list difference does not create a delta. The BPE 2025 kept-list answer is #198's exercise.")
  add("")

  add("### Definitional equality (why the comparison is honest)")
  add("")
  add("- Legacy per-building **car_div** / **car_tot** = `rowSums` over the `car_<TYPEQU>` count columns at 20 min (`score_computation.R:200-207`) — **identical** to our `derive_building_metrics` `diversity`/`total` at threshold 20 over the same 53-code kept-list (`derive.R`).")
  add("- Legacy **has_{food,health,admin,school,bank}_car** composites (`score_computation.R:213-231`, `calc_dummy` regex per cluster) — **byte-identical** cluster memberships to our `cluster_defs()` (`code/R/constants.R`).")
  add("- Legacy **pct_iso_full_c** = `sum(car_tot == 0) / .N` (`summarizing.R:163`) — identical to our `pct_iso_full` (car row).")
  add("- Legacy **pct_iso_{food,health}_*** exist only for `t`/`b` (`summarizing.R:159-162`) — car-vs-car isolation is 0 by definition, so no car counterpart is needed (and ours is 0 on all 28 communes, verified).")
  add("")

  add("### Named drift sources (why deltas happen)")
  add("")
  add("| Driver | Affects | Direction |")
  add("|---|---|---|")
  for (i in seq_len(nrow(drift))) {
    add(sprintf("| %s | %s | %s |", drift[["driver"]][i], drift[["affects"]][i], drift[["direction"]][i]))
  }
  add("")
  add("BPE **content** effects (new establishments and nomenclature) are reported separately from BPE **identity** effects. The lossless destination registry retains every distinct listing, including co-located and empty-SIRET listings; only exact full duplicates are removed. Identity changes therefore alter establishment counts without implying new BPE content. Origin coverage is likewise explicit: rows missing canonical `usage_principal_bdnb_open` are **unknown coverage**, not non-residential, and are excluded rather than recovered through deprecated usage fields.")
  add("")

  add("### Classification bands (maintainer-approved)")
  add("")
  add("| Metric family | Band | Test |")
  add("|---|---|---|")
  add(sprintf("| shares (5 clusters) | ±%.2f abs | \\|delta_abs\\| ≤ %.2f |", bands$share$limit, bands$share$limit))
  add(sprintf("| pct_iso_full | ±%.2f abs | \\|delta_abs\\| ≤ %.2f |", bands$pct_iso_full$limit, bands$pct_iso_full$limit))
  add(sprintf("| avg_diversity | ±%.0f%% rel | \\|delta_rel\\| ≤ %.2f |", 100 * bands$avg_diversity$limit, bands$avg_diversity$limit))
  add(sprintf("| avg_total | ±%.0f%% rel | \\|delta_rel\\| ≤ %.2f |", 100 * bands$avg_total$limit, bands$avg_total$limit))
  add(sprintf("| nb_buildings | ±%.0f%% rel **vs the measured granularity prior (%.2f)** | \\|ratio − prior\\| / prior ≤ %.2f |", 100 * bands$nb_buildings$limit, nb_prior, bands$nb_buildings$limit))
  add("")
  add("Beyond band + **unexplained by a named drift driver** → **FLAG** (never silently absorbed). The border-widening driver (ADR-0002) explains beyond-band `avg_diversity`/`avg_total` deltas on the toy region's border communes.")
  add("")

  add("### Accepted prior measured findings (Ticket 07)")
  add("")
  if (is.null(accepted_findings) || nrow(accepted_findings) == 0L) {
    add("None supplied. Classification is based only on bands and named drift drivers.")
  } else {
    add("These rows are **accepted because Ticket 07 measured and explained them**, not because they fell within a classification band. They remain explicit in the reusable override table and are not silently absorbed by generic logic.")
    add("")
    add("| INSEE | Metric | Prior measured finding |")
    add("|---|---|---|")
    for (i in seq_len(nrow(accepted_findings))) {
      add(sprintf("| `%s` | `%s` | %s |",
                  accepted_findings[["code_insee"]][i],
                  accepted_findings[["metric"]][i],
                  accepted_findings[["reason"]][i]))
    }
  }
  add("")

  # --- per-commune deltas tables -------------------------------------------
  add("## Per-commune deltas (car axis, threshold 20 min, 53 routed codes)")
  add("")
  for (m in unique(d[["metric"]])) {
    sub <- d[metric == m]
    add(sprintf("### `%s`", m))
    add("")
    add("| Commune | Snapshot | Derived | Δ abs | Δ rel | Classification | Reason |")
    add("|---|---|---|---|---|---|---|")
    for (r in seq_len(nrow(sub))) {
      nm <- if (is.na(sub[["nom_commune"]][r])) "" else sub[["nom_commune"]][r]
      dr <- if (is.na(sub[["delta_rel"]][r])) "—" else sprintf("%+.1f%%", 100 * sub[["delta_rel"]][r])
      add(sprintf("| %s %s | %s | %s | %+.3f | %s | **%s** | %s |",
                  sub[["code_insee"]][r], nm,
                  format(sub[["snapshot"]][r], big.mark = ","),
                  format(sub[["derived"]][r], big.mark = ","),
                  sub[["delta_abs"]][r], dr,
                  sub[["classification"]][r], sub[["reason"]][r]))
    }
    add("")
  }

  # --- expected-drift commentary -------------------------------------------
  add("## Expected-drift commentary (per metric family)")
  add("")
  add(sprintf("- **nb_buildings** — the origin universe grows by the measured prior %.2f (25,867 constructions vs the snapshot's 18,262 buildings: construction granularity + BDNB vintage + canonical-usage unknown coverage excluded by policy). Geometry follows the corrected BAN/non-fictive/fictive hierarchy. Per-commune ratios inside ±%.0f%% of the prior are expected; the two communes outside it are flagged below.", nb_prior, 100 * bands$nb_buildings$limit))
  add("- **shares** — car access saturates near 1.0 in the toy region (every commune has car access to all five clusters within 20 min); deltas are tiny and expected. The cluster definitions are byte-identical on both sides (score_computation.R vs cluster_defs()).")
  add("- **avg_diversity** — the per-building distinct-type count at 20 min. BPE 2024→2025 and the wider border move it up modestly; the largest gains (+34% Louvigné-du-Désert, +38% Monthault) sit on the border and are explained by ADR-0002's widening. No commune averages all 53 kept types: the max observed is 52.00 (D267 — écoles supérieures — is reachable by only 2 buildings of 25,867 at 20 min; no single type is absent from the car matrix).")
  add("- **avg_total** — the per-building establishment count at 20 min. BPE 2025 adds establishments; border communes gain cross-border establishments; a few non-border communes exceed the +40% band and are flagged.")
  add("- **pct_iso_full** — 0 on both sides, by definition (car reaches every building; legacy summarizing.R:163 computes the same 0).")
  add("")

  # --- flagged anomalies ----------------------------------------------------
  add("## Flagged anomalies")
  add("")
  flags <- d[classification == "flag"]
  if (nrow(flags) == 0L) {
    add("**None.** Every delta is within its band or explained by a named drift driver. (State explicitly: no flag this rebuild.)")
  } else {
    add(sprintf("**%d flagged delta(s), each with a hypothesis — maintainer review requested:**", nrow(flags)))
    add("")
    for (r in seq_len(nrow(flags))) {
      add(sprintf("- **%s %s · `%s`** — %s", flags[["code_insee"]][r], flags[["nom_commune"]][r], flags[["metric"]][r], flags[["reason"]][r]))
    }
  }
  add("")

  add("## Missing classifications")
  add("")
  if (n_miss == 0L) {
    add("**None.** Every comparable commune × metric has a reading on both sides.")
  } else {
    add(sprintf("**%d missing comparison(s), explicitly classified:**", n_miss))
    add("")
    misses <- d[classification == "missing"]
    for (r in seq_len(nrow(misses))) {
      add(sprintf("- **%s · `%s`** — %s", misses[["code_insee"]][r], misses[["metric"]][r], misses[["reason"]][r]))
    }
  }
  add("")

  # --- walk section ---------------------------------------------------------
  add("## Walk axis: NON-COMPARABLE (first pure-walk reading)")
  add("")
  add("The legacy chain **never routed a pure-walk mode**: its `t` axis is the r5r WALK+TRANSIT composite (`linking_logic.R:229-272`), so the snapshot's `share_*_t` / `avg_div_t` / `avg_tot_t` / `pct_iso_full_t` are walk+transit readings with no pure-walk counterpart. The comparison therefore excludes walk entirely — silently comparing our walk against those composites would mix transit's reach into the delta.")
  add("")
  if (!is.null(walk_agg) && nrow(walk_agg) > 0L) {
    w <- data.table::as.data.table(walk_agg)
    add("Our walk readings are shown here contextually — **the first pure-walk measurement of the toy region** (threshold 20 min, 53 routed codes, walk_speed 4 km/h per run-strategy D5). When the transit axis lands at the full run, these become comparable to the product's « à pied ou en TC » reading.")
    add("")
    wcols <- c("code_insee", "nb_buildings", "share_alimentation", "share_sante",
               "share_administration", "share_ecole", "share_banque",
               "avg_diversity", "avg_total", "pct_iso_full")
    wcols <- intersect(wcols, names(w))
    w <- w[, wcols, with = FALSE]
    add("| Commune | Buildings | share_alim | share_santé | share_admin | share_école | share_banque | avg_div | avg_tot | pct_iso_full |")
    add("|---|---|---|---|---|---|---|---|---|---|")
    for (r in seq_len(nrow(w))) {
      add(sprintf("| %s | %s | %.3f | %.3f | %.3f | %.3f | %.3f | %.1f | %.1f | %.3f |",
                  w[["code_insee"]][r],
                  format(w[["nb_buildings"]][r], big.mark = ","),
                  w[["share_alimentation"]][r], w[["share_sante"]][r],
                  w[["share_administration"]][r], w[["share_ecole"]][r],
                  w[["share_banque"]][r], w[["avg_diversity"]][r],
                  w[["avg_total"]][r], w[["pct_iso_full"]][r]))
    }
    add("")
  }

  # --- legacy-only columns --------------------------------------------------
  add("## Legacy-only snapshot columns (out of scope, never compared)")
  add("")
  add("The snapshot carries 2,061 columns; only the 9 mapped above are comparable. The rest are legacy-only derivations with no counterpart in our derivation layer — documented so their absence is deliberate, not accidental:")
  add("")
  add("- **vulnerability**: `med_vuln_{t,b}`, `avg_vuln_{t,b}` (+ `_epci/_dep/_reg`) — legacy `norm_score` derivation, not part of the matrix contract.")
  add("- **loss families**: `avg_div_loss_{t,b}`, `med_div_loss_{t,b}`, `avg_tot_loss_{t,b}`, `med_tot_loss_{t,b}` (+ levels) — legacy loss-vs-car for the composite t/b axes only.")
  add("- **deciles**: `div_loss_*_dec_1..10`, `tot_loss_*_dec_1..10`, `dens_vuln_*`, `dens_div_*`, `dens_tot_*` — distributional stats, legacy-only.")
  add("- **ranks**: `rank_vuln_med_{t,b}`, `rank_div_loss_med_{t,b}`, `rank_tot_loss_med_{t,b}`, `region_percentile`, `region_raw`.")
  add("- **top-3 contributors**: `unique_dep_1..3`, `dep_{1..3}_{t,b,c}`, `unique_res_1..3`, `res_{1..3}_{t,b,c}`.")
  add("- **per-TYPEQU raw medians/presence**: `med_<TYPEQU>_{t,b,c}[_raw]`, `has_<TYPEQU>_{t,b,c}[_raw]` (+ `_epci/_dep/_reg` variants) — building-level facts the matrix stores at TYPEQU level instead.")
  add("- **other levels**: every `_epci/_dep/_reg` column (585 columns) — the deliverable is commune-level; the snapshot holds no EPCI row anyway (verified).")
  add("")

  # --- reuse note -----------------------------------------------------------
  add("## Reuse: the per-rebuild regression seam")
  add("")
  add("This report regenerates at every rebuild from three fixed things:")
  add("")
  add("- **the module** `code/R/deltas.R` (S13): `legacy_routed_types()`, `legacy_snapshot_map()`, `read_legacy_snapshot()`, `derive_deltas()`, `render_deltas_report()` — the comparison contract is parameterised (bands, border communes, granularity prior), not hard-coded.")
  add("- **the run script** `.scratch/tracer-matrice/research/07-deltas-report.R`: reads the real matrix, derives with `kept = legacy_routed_types()`, `threshold = 20`, `ref_mode = car`, aggregates via the BDNB crosswalk, reads the frozen snapshot, computes + renders. Self-printing PASS/FAIL, exit non-zero on failure.")
  add(sprintf("- **the frozen snapshot**: `%s` — the 2026-02-28 baseline stays frozen; the deltas against it are the regression signal. When the baseline itself is re-baselined (a deliberate release), the report's vintage stamps change and the old baseline is archived.", if (is.null(snapshot_path)) "<snapshot path>" else snapshot_path))
  add("")

  # --- next steps -----------------------------------------------------------
  add("## Next steps")
  add("")
  add("- **#198 — kept-list review**: decide the BPE 2025 kept-list against the acquired universe (the 53-routed vs 54-recorded divergence is documented here as the starting point).")
  add("- **Transit axis at the full run**: r5r WALK+TRANSIT with percentiles 1/50 becomes comparable to the legacy `_t` columns — the walk section of this report is the placeholder for that comparison.")
  add("- **Bike axis** (full run): comparable to the legacy `_b` columns once routed.")
  add("")

  lines
}
