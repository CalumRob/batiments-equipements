# S3 — the building-level derivation layer: kept-list selection, the div_loss
# family, and per-cluster access flags.

#' Derive one reading per (building, mode atomique).
#'
#' At a ladder-rung `threshold` (default 20 — the « Vingt minutes » reading),
#' and comparing each mode against `ref_mode` (default car):
#'   - diversity: number of distinct kept TYPEQU with count_T >= 1
#'   - total: sum of count_T over kept TYPEQU (establishments within T)
#'   - div_loss = ref_mode.diversity - mode.diversity
#'   - tot_loss = ref_mode.total - mode.total
#'   - has_<cluster>: TRUE when any type of the cluster is within T
#'
#' `kept` is REQUIRED — the kept-list is a derivation input decided by #198
#' against the BPE 2025 universe; nothing is baked in. Buildings absent from a
#' mode (sparse matrix) get an all-zero reading. The ref_mode row itself has
#' div_loss/tot_loss = 0 by construction.
derive_building_metrics <- function(matrix, kept, threshold = 20, ref_mode = "car") {
  if (missing(kept)) {
    stop("`kept` is required: the kept-list is a derivation input (#198 decides it against BPE 2025); no default is baked in", call. = FALSE)
  }
  validate_matrix(matrix)
  if (!threshold %in% ladder_rungs()) {
    stop(sprintf("threshold must be a ladder rung: %s", paste(ladder_rungs(), collapse = ", ")), call. = FALSE)
  }
  if (!ref_mode %in% atomic_modes()) {
    stop(sprintf("ref_mode must be an atomic mode: %s", paste(atomic_modes(), collapse = ", ")), call. = FALSE)
  }

  m <- data.table::as.data.table(matrix)
  kept <- as.character(kept)
  count_col <- paste0("count_", threshold)
  cl <- cluster_defs()

  k <- m[TYPEQU %in% kept]
  reached <- k[k[[count_col]] >= 1]

  metrics <- reached[, .(
    diversity = data.table::uniqueN(TYPEQU),
    total = sum(.SD[[1]]),
    has_alimentation   = any(TYPEQU %in% cl[["alimentation"]]),
    has_sante          = any(TYPEQU %in% cl[["sante"]]),
    has_administration = any(TYPEQU %in% cl[["administration"]]),
    has_ecole          = any(TYPEQU %in% cl[["ecole"]]),
    has_banque         = any(TYPEQU %in% cl[["banque"]])
  ), by = .(batiment_id, mode), .SDcols = count_col]

  # Sparse contract: enumerate every building × mode, zero-fill missing combos.
  grid <- data.table::CJ(batiment_id = unique(m$batiment_id), mode = unique(m$mode))
  out <- merge(grid, metrics, by = c("batiment_id", "mode"), all.x = TRUE)
  for (col in c("diversity", "total")) {
    data.table::set(out, which(is.na(out[[col]])), col, 0L)
  }
  for (col in cluster_flag_cols()) {
    data.table::set(out, which(is.na(out[[col]])), col, FALSE)
  }

  # Loss family vs the reference mode.
  ref <- out[mode == ref_mode, .(batiment_id, ref_diversity = diversity, ref_total = total)]
  out <- merge(out, ref, by = "batiment_id", all.x = TRUE)
  out[, div_loss := ref_diversity - diversity]
  out[, tot_loss := ref_total - total]
  out[, c("ref_diversity", "ref_total") := NULL]

  data.table::setcolorder(out, c("batiment_id", "mode", "diversity", "total",
                                 "div_loss", "tot_loss", cluster_flag_cols()))
  data.table::setorder(out, batiment_id, mode)
  out[]
}

#' Aggregate the building readings to a territory level.
#'
#' One row per (territory key, mode atomique): nb_buildings, the access shares
#' (share_<cluster> = mean of the has_<cluster> flags), the isolation family
#' (pct_iso_<cluster> = share of buildings with ref-mode access but no
#' mode access; pct_iso_full = share with zero total), and the div_loss /
#' tot_loss stats (mean + median). The crosswalk carries batiment_id → the
#' territory codes (code_insee / epci / code_departement / region). Buildings
#' without a territory code are excluded (legacy behavior).
derive_territory_aggregates <- function(metrics, crosswalk, level = "commune", ref_mode = "car") {
  level_keys <- c(
    commune = "code_insee", epci = "epci",
    departement = "code_departement", region = "region"
  )
  if (!level %in% names(level_keys)) {
    stop(sprintf("level must be one of: %s", paste(names(level_keys), collapse = ", ")), call. = FALSE)
  }
  if (!ref_mode %in% atomic_modes()) {
    stop(sprintf("ref_mode must be an atomic mode: %s", paste(atomic_modes(), collapse = ", ")), call. = FALSE)
  }
  key <- unname(level_keys[[level]])

  m <- data.table::as.data.table(metrics)
  xw <- data.table::as.data.table(crosswalk)
  if (!"batiment_id" %in% names(xw)) {
    stop("crosswalk must have a batiment_id column", call. = FALSE)
  }
  if (!key %in% names(xw)) {
    stop("crosswalk missing column: ", key, call. = FALSE)
  }

  clusters <- names(cluster_defs())
  flag_cols <- cluster_flag_cols()
  ref_flags <- paste0("ref_", clusters)

  dt <- merge(m, xw[, c("batiment_id", key), with = FALSE], by = "batiment_id", all.x = TRUE)
  dt <- dt[!is.na(get(key))]

  # Ref-mode access flags per building: the base of the pct_iso family.
  rf <- m[mode == ref_mode, c("batiment_id", flag_cols), with = FALSE]
  data.table::setnames(rf, flag_cols, ref_flags)
  dt <- merge(dt, rf, by = "batiment_id", all.x = TRUE)
  for (col in ref_flags) {
    data.table::set(dt, which(is.na(dt[[col]])), col, FALSE)
  }

  share_cols <- paste0("share_", clusters)
  iso_cols <- paste0("pct_iso_", clusters)

  agg <- dt[, c(
    list(
      nb_buildings = data.table::uniqueN(batiment_id),
      avg_diversity = mean(diversity, na.rm = TRUE),
      avg_total = mean(total, na.rm = TRUE),
      avg_div_loss = mean(div_loss, na.rm = TRUE),
      # median() preserves integer output for some group shapes and returns
      # double for others. Explicitly normalise the type so data.table can
      # combine groups with odd and even building counts.
      med_div_loss = as.numeric(median(div_loss, na.rm = TRUE)),
      avg_tot_loss = mean(tot_loss, na.rm = TRUE),
      med_tot_loss = as.numeric(median(tot_loss, na.rm = TRUE)),
      pct_iso_full = mean(total == 0, na.rm = TRUE)
    ),
    stats::setNames(
      lapply(flag_cols, function(f) mean(.SD[[f]], na.rm = TRUE)),
      share_cols
    ),
    stats::setNames(
      lapply(seq_along(clusters), function(i) {
        mean(.SD[[ref_flags[i]]] & !.SD[[flag_cols[i]]], na.rm = TRUE)
      }),
      iso_cols
    )
  ), by = c(key, "mode"), .SDcols = c(flag_cols, ref_flags)]

  data.table::setcolorder(agg, c(
    key, "mode", "nb_buildings", share_cols, iso_cols, "pct_iso_full",
    "avg_diversity", "avg_total", "avg_div_loss", "med_div_loss",
    "avg_tot_loss", "med_tot_loss"
  ))
  data.table::setorderv(agg, c(key, "mode"))
  agg[]
}
