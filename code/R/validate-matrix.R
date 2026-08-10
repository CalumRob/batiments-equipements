# S1 — the matrix schema validator + parquet reader.

#' Validate a matrice d'accessibilité artifact against the contract.
#'
#' Contract (run-strategy "Matrix contract" + ADR-0002): long table, one row per
#' (batiment_id, TYPEQU, mode atomique); `tt_nearest` in minutes + one count
#' column per ladder rung; rows exist only where the type is reachable within
#' the cap (sparse); the ladder is internally consistent — for every rung r,
#' count_r >= 1 iff tt_nearest <= r. Extensions are accepted by design (axis-
#' extension rule): extra columns (transit p1/p50), any TYPEQU (full universe),
#' any subset of the atomic modes. Throws with all violations on failure,
#' returns TRUE invisibly on success.
validate_matrix <- function(x) {
  if (!is.data.frame(x)) {
    stop("matrix must be a data.frame or data.table", call. = FALSE)
  }

  violations <- character(0)

  required <- c("batiment_id", "TYPEQU", "mode", "tt_nearest", ladder_cols())
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    violations <- c(
      violations,
      sprintf("missing required columns: %s", paste(missing, collapse = ", "))
    )
  }

  if (length(violations) == 0) {
    b <- x[["batiment_id"]]
    if (anyNA(b) || any(!nzchar(as.character(b)))) {
      violations <- c(violations, "batiment_id must be non-NA and non-empty")
    }
    tq <- x[["TYPEQU"]]
    if (anyNA(tq) || any(!nzchar(as.character(tq)))) {
      violations <- c(violations, "TYPEQU must be non-NA and non-empty")
    }
    m <- x[["mode"]]
    if (!all(m %in% atomic_modes())) {
      violations <- c(
        violations,
        sprintf("mode must be one of %s", paste(atomic_modes(), collapse = ", "))
      )
    }

    tt <- x[["tt_nearest"]]
    if (!is.numeric(tt)) {
      violations <- c(violations, "tt_nearest must be numeric")
    } else {
      if (anyNA(tt)) violations <- c(violations, "tt_nearest must not be NA")
      if (any(tt < 0)) violations <- c(violations, "tt_nearest must be >= 0")
      if (any(tt > cap_minutes())) {
        violations <- c(
          violations,
          sprintf("tt_nearest must be <= cap (%d minutes)", cap_minutes())
        )
      }
    }

    for (r in ladder_rungs()) {
      col <- paste0("count_", r)
      v <- x[[col]]
      if (!is.numeric(v)) {
        violations <- c(violations, sprintf("%s must be numeric", col))
      } else {
        if (anyNA(v)) violations <- c(violations, sprintf("%s must not be NA", col))
        if (any(v < 0)) violations <- c(violations, sprintf("%s must be >= 0", col))
        if (any(v != floor(v))) violations <- c(violations, sprintf("%s must be whole numbers", col))
      }
    }

    # Consistency: for each rung r, count_r >= 1 iff tt_nearest <= r.
    for (r in ladder_rungs()) {
      col <- paste0("count_", r)
      ok <- (x[[col]] >= 1) == (tt <= r)
      if (any(!ok, na.rm = TRUE)) {
        violations <- c(
          violations,
          sprintf("%s inconsistent with tt_nearest (count_r >= 1 iff tt_nearest <= %d)", col, r)
        )
      }
    }

    # Ladder monotonicity: counts can only grow as the cutoff widens.
    lads <- ladder_rungs()
    for (i in seq_along(lads)[-1]) {
      prev <- paste0("count_", lads[i - 1])
      cur <- paste0("count_", lads[i])
      if (any(x[[prev]] > x[[cur]], na.rm = TRUE)) {
        violations <- c(
          violations,
          sprintf("count ladder not monotone non-decreasing (%s > %s on some row)", prev, cur)
        )
        break
      }
    }

    # Uniqueness of the (batiment_id, TYPEQU, mode) key.
    keys <- as.data.frame(x)[c("batiment_id", "TYPEQU", "mode")]
    if (anyDuplicated(keys)) {
      violations <- c(violations, "duplicate (batiment_id, TYPEQU, mode) rows")
    }
  }

  if (length(violations) > 0) {
    stop(paste0("invalid matrix:\n- ", paste(violations, collapse = "\n- ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Read a matrix parquet artifact into a data.table (the once-run's format).
read_matrix <- function(path) {
  if (!file.exists(path)) {
    stop("parquet file not found: ", path, call. = FALSE)
  }
  data.table::as.data.table(arrow::read_parquet(path))
}
