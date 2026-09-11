# Address-level derivations for the public geography tables.
#
# The address accessibility monolith is intentionally sparse: it contains a
# row only when at least one TYPEQU was retained for an address.  Public
# geography tables have a different contract.  They are dense at
# (territory, TYPEQU) and their denominator is the complete address-origin
# spine.  This module performs that derivation without materialising the much
# larger address x full TYPEQU cartesian product.  The Arrow-backed publisher
# below keeps the same contract while reducing each sparse metric to a compact
# territory/TYPEQU/value histogram before collecting it into R.

address_aggregate_default_probs <- function() {
  seq(0.1, 0.9, by = 0.1)
}

address_aggregate_stat_suffixes <- function(probs) {
  c("mean", paste0("p", as.integer(round(100 * probs))),
    "max", "share")
}

# Type-7 quantiles over `n_total` observations where `values` are the observed
# sparse rows and every omitted observation is an implicit zero.  This avoids
# allocating one zero per absent address while retaining stats::quantile's
# interpolation rule.
address_aggregate_implicit_zero_quantile <- function(values, n_total, probs) {
  address_aggregate_implicit_zero_quantile_weighted(
    values, rep(1, length(values)), n_total, probs
  )
}

# Type-7 quantiles over a weighted sparse histogram.  `weights` are counts of
# addresses having each value; omitted address/type rows are implicit zeros.
address_aggregate_implicit_zero_quantile_weighted <- function(
    values, weights, n_total, probs) {
  if (n_total < length(values)) {
    stop("implicit-zero quantile has more histogram values than denominator",
         call. = FALSE)
  }
  if (length(values) != length(weights)) {
    stop("weighted implicit-zero quantile values and weights must have equal length",
         call. = FALSE)
  }
  if (!length(values)) return(rep(0, length(probs)))

  values <- as.numeric(values)
  weights <- as.numeric(weights)
  if (anyNA(values)) {
    stop("sparse metric values must be non-NA before aggregation", call. = FALSE)
  }
  if (anyNA(weights) || any(weights < 0) || any(weights != floor(weights))) {
    stop("sparse metric weights must be non-negative integers", call. = FALSE)
  }
  observed <- sum(weights)
  if (observed > n_total) {
    stop("implicit-zero quantile has more weighted rows than denominator",
         call. = FALSE)
  }

  ordered <- order(c(0, values), method = "radix")
  sorted_values <- c(0, values)[ordered]
  sorted_weights <- c(n_total - observed, weights)[ordered]
  cumulative <- cumsum(sorted_weights)

  order_stat <- function(rank) {
    if (rank <= 0 || rank > n_total) {
      stop("implicit-zero order statistic rank is outside the denominator",
           call. = FALSE)
    }
    sorted_values[which(cumulative >= rank)[[1L]]]
  }

  vapply(probs, function(prob) {
    h <- 1 + (n_total - 1) * prob
    lower_rank <- floor(h)
    fraction <- h - lower_rank
    lower <- order_stat(lower_rank)
    if (fraction == 0 || lower_rank == n_total) return(lower)
    lower + fraction * (order_stat(lower_rank + 1) - lower)
  }, numeric(1L))
}

address_aggregate_summary_values <- function(values, n_total, probs) {
  if (anyNA(values)) {
    stop("sparse metric values must be non-NA before aggregation", call. = FALSE)
  }
  quantiles <- address_aggregate_implicit_zero_quantile(
    values, n_total, probs
  )
  c(
    list(mean = sum(values) / n_total),
    stats::setNames(as.list(quantiles), paste0("p", as.integer(round(
      100 * probs
    )))),
    list(max = max(c(0, values)), share = sum(values > 0) / n_total)
  )
}

address_aggregate_summary_weighted_values <- function(
    values, weights, n_total, probs) {
  if (anyNA(values)) {
    stop("sparse metric values must be non-NA before aggregation", call. = FALSE)
  }
  quantiles <- address_aggregate_implicit_zero_quantile_weighted(
    values, weights, n_total, probs
  )
  stats::setNames(as.list(c(
    sum(as.numeric(values) * as.numeric(weights)) / n_total,
    quantiles,
    max(c(0, values)),
    sum(as.numeric(weights)[values > 0]) / n_total
  )),
    c("mean", paste0("p", as.integer(round(100 * probs))), "max", "share")
  )
}

address_aggregate_validate_probs <- function(probs) {
  if (!is.numeric(probs) || !length(probs) || anyNA(probs) ||
      any(probs <= 0) || any(probs >= 1) || anyDuplicated(probs) ||
      !identical(as.numeric(probs), sort(as.numeric(probs)))) {
    stop("probs must be strictly increasing numeric probabilities between 0 and 1",
         call. = FALSE)
  }
  as.numeric(probs)
}

address_aggregate_type_catalogue <- function(type_catalogue) {
  if (is.data.frame(type_catalogue)) {
    if (!"TYPEQU" %in% names(type_catalogue)) {
      stop("type_catalogue data.frame must have a TYPEQU column", call. = FALSE)
    }
    type_catalogue <- type_catalogue[["TYPEQU"]]
  }
  types <- unique(as.character(type_catalogue))
  types <- types[!is.na(types) & nzchar(types)]
  if (!length(types)) {
    stop("type_catalogue must contain at least one non-empty TYPEQU", call. = FALSE)
  }
  types
}

#' Aggregate sparse address/type metrics to a dense geography/type table.
#'
#' `metrics` is sparse and must be unique at `(address_id, TYPEQU)`.  Every
#' `value_columns` measure is treated as a zero-filled metric for omitted
#' address/type rows.  `address_spine` supplies the complete denominator and
#' the geography columns.  The result has one row for every assigned territory
#' crossed with every TYPEQU in `type_catalogue`.
#'
#' Quantiles use the type-7 rule and include implicit zero observations.  The
#' function is deliberately geography-agnostic: pass commune, EPCI, department,
#' or any other stable territory columns without changing the aggregation
#' semantics.
#'
#' @param metrics Sparse data.frame/data.table with address_id, TYPEQU, and
#'   numeric value columns.
#' @param address_spine One row per address_id, with geography columns.
#' @param type_catalogue Character vector or data.frame with TYPEQU.
#' @param territory_columns Geography columns used as the output key.
#' @param value_columns Numeric sparse metrics to aggregate.
#' @param probs Strictly increasing quantile probabilities between 0 and 1.
#' @return A data.table with one row per territory and TYPEQU.
#' @export
aggregate_address_type_metrics <- function(
    metrics, address_spine, type_catalogue,
    territory_columns = c("code_insee", "nom_commune"),
    value_columns, probs = address_aggregate_default_probs()) {
  stopifnot(is.data.frame(metrics), is.data.frame(address_spine))
  if (!is.character(territory_columns) || !length(territory_columns) ||
      anyNA(territory_columns) || anyDuplicated(territory_columns)) {
    stop("territory_columns must contain unique non-empty column names",
         call. = FALSE)
  }
  if (!is.character(value_columns) || !length(value_columns) ||
      anyNA(value_columns) || anyDuplicated(value_columns)) {
    stop("value_columns must contain unique non-empty column names",
         call. = FALSE)
  }
  probs <- address_aggregate_validate_probs(probs)
  types <- address_aggregate_type_catalogue(type_catalogue)

  metrics <- data.table::as.data.table(data.table::copy(metrics))
  spine <- data.table::as.data.table(data.table::copy(address_spine))
  required_metrics <- c("address_id", "TYPEQU", value_columns)
  missing <- setdiff(required_metrics, names(metrics))
  if (length(missing)) {
    stop("metrics missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  missing <- setdiff(c("address_id", territory_columns), names(spine))
  if (length(missing)) {
    stop("address_spine missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(metrics, by = c("address_id", "TYPEQU"))) {
    stop("metrics contain duplicate (address_id, TYPEQU) keys", call. = FALSE)
  }
  if (anyDuplicated(spine[["address_id"]])) {
    stop("address_spine must contain one row per address_id", call. = FALSE)
  }
  if (anyNA(metrics[["TYPEQU"]]) ||
      any(!as.character(metrics[["TYPEQU"]]) %in% types)) {
    stop("metrics contain TYPEQU values absent from type_catalogue", call. = FALSE)
  }
  for (column in value_columns) {
    if (!is.numeric(metrics[[column]])) {
      stop("metric column `", column, "` must be numeric", call. = FALSE)
    }
  }

  metrics[, address_id := as.character(address_id)]
  metrics[, TYPEQU := as.character(TYPEQU)]
  spine[, address_id := as.character(address_id)]
  unknown_addresses <- setdiff(metrics[["address_id"]], spine[["address_id"]])
  if (length(unknown_addresses)) {
    stop("metrics contain address_id values absent from address_spine",
         call. = FALSE)
  }

  # Geography with an incomplete key is not an assigned territory.  The
  # complete address denominator is preserved within every assigned territory;
  # unmatched addresses are excluded rather than silently assigned to a fake
  # geography.
  assigned <- spine[stats::complete.cases(
    spine[, territory_columns, with = FALSE]
  )]
  denominators <- assigned[, .(n_addresses = .N), by = territory_columns]
  territories <- unique(assigned[, territory_columns, with = FALSE])

  # Build the manageable public grid: territory x TYPEQU, not address x
  # TYPEQU.  The key columns are copied so inputs remain untouched.
  type_table <- data.table::data.table(TYPEQU = types)
  territories[, `.__aggregate_join_key` := 1L]
  type_table[, `.__aggregate_join_key` := 1L]
  grid <- merge(territories, type_table,
                by = ".__aggregate_join_key", allow.cartesian = TRUE,
                sort = FALSE)
  grid[, `.__aggregate_join_key` := NULL]

  observed <- metrics[assigned, on = "address_id", nomatch = 0L]
  if (nrow(observed)) {
    observed <- merge(observed, denominators, by = territory_columns,
                      all.x = TRUE, sort = FALSE)
  }

  suffixes <- address_aggregate_stat_suffixes(probs)
  if (nrow(observed)) {
    summaries <- observed[, {
      total <- as.integer(n_addresses[[1L]])
      values <- get(value_columns[[1L]])
      stats::setNames(
        c(
          list(n_observed = .N),
          address_aggregate_summary_values(values, total, probs)
        ),
        c("n_observed", paste0(value_columns[[1L]], "_", suffixes))
      )
    }, by = c(territory_columns, "TYPEQU", "n_addresses")]
    # The loop below adds the remaining metric families.  The first family was
    # computed above so the zero-fill and quantile logic has one obvious seam.
    if (length(value_columns) > 1L) {
      for (column in value_columns[-1L]) {
        extra <- observed[, {
          total <- as.integer(n_addresses[[1L]])
          values <- get(column)
          stats::setNames(
            address_aggregate_summary_values(values, total, probs),
            paste0(column, "_", suffixes)
          )
        }, by = c(territory_columns, "TYPEQU", "n_addresses")]
        summaries <- merge(summaries, extra,
                           by = c(territory_columns, "TYPEQU", "n_addresses"),
                           all = TRUE, sort = FALSE)
      }
    }
  } else {
    summaries <- data.table::copy(grid)
    summaries[, n_observed := 0L]
    for (column in value_columns) {
      for (suffix in suffixes) {
        summaries[, (paste0(column, "_", suffix)) := 0]
      }
    }
  }

  out <- merge(grid, summaries,
               by = c(territory_columns, "TYPEQU"), all.x = TRUE,
               sort = FALSE)
  # Attach the denominator after the grid/observed merge.  The observed path
  # already carries the same value for grouping; the denominator-side value is
  # authoritative and also fills TYPEQU combinations with no sparse row.
  out <- denominators[out, on = territory_columns]
  if ("i.n_addresses" %in% names(out)) out[, `i.n_addresses` := NULL]
  out[is.na(n_observed), n_observed := 0L]
  for (column in value_columns) {
    for (suffix in suffixes) {
      name <- paste0(column, "_", suffix)
      if (!name %in% names(out)) out[, (name) := 0]
      out[is.na(get(name)), (name) := 0]
    }
  }
  data.table::setcolorder(out, c(
    territory_columns, "TYPEQU", "n_addresses", "n_observed",
    unlist(lapply(value_columns, function(column) {
      paste0(column, "_", suffixes)
    }), use.names = FALSE)
  ))
  data.table::setorderv(out, c(territory_columns, "TYPEQU"))
  out[]
}

#' Aggregate a persisted Arrow address monolith without collecting its rows.
#'
#' The monolith is scanned once per value column.  Arrow groups sparse rows by
#' territory, TYPEQU, and metric value; only that compact histogram is
#' collected into R.  Quantiles and means then use the complete address spine
#' as their denominator, treating omitted address/type rows as zero.
#'
#' @param dataset An Arrow Dataset containing address_id, TYPEQU, and metrics.
#' @param address_spine One row per address_id with territory columns.
#' @param type_catalogue Character vector or data.frame with TYPEQU.
#' @param territory_columns Geography columns used as the output key.
#' @param value_columns Numeric sparse metrics to aggregate.
#' @param probs Strictly increasing quantile probabilities between 0 and 1.
#' @return A data.table with one row per territory and TYPEQU.
#' @export
aggregate_address_type_metrics_arrow <- function(
    dataset, address_spine, type_catalogue,
    territory_columns = c("code_insee", "nom_commune"),
    value_columns, probs = address_aggregate_default_probs()) {
  if (!inherits(dataset, c(
    "Dataset", "FileSystemDataset", "InMemoryDataset", "ArrowTabular"
  ))) {
    stop("dataset must be an Arrow Dataset or Table", call. = FALSE)
  }
  stopifnot(is.data.frame(address_spine))
  if (!is.character(value_columns) || !length(value_columns) ||
      anyNA(value_columns) || anyDuplicated(value_columns)) {
    stop("value_columns must contain unique non-empty column names",
         call. = FALSE)
  }
  probs <- address_aggregate_validate_probs(probs)
  types <- address_aggregate_type_catalogue(type_catalogue)
  spine <- data.table::as.data.table(data.table::copy(address_spine))
  missing <- setdiff(c("address_id", territory_columns), names(spine))
  if (length(missing)) {
    stop("address_spine missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(spine[["address_id"]])) {
    stop("address_spine must contain one row per address_id", call. = FALSE)
  }
  spine[, address_id := as.character(address_id)]
  assigned <- spine[stats::complete.cases(
    spine[, territory_columns, with = FALSE]
  )]
  if (!nrow(assigned)) {
    stop("address_spine has no complete territory assignments", call. = FALSE)
  }

  denominators <- assigned[, .(n_addresses = .N), by = territory_columns]
  territories <- unique(assigned[, territory_columns, with = FALSE])
  territories[, `.__aggregate_join_key` := 1L]
  type_table <- data.table::data.table(TYPEQU = types)
  type_table[, `.__aggregate_join_key` := 1L]
  grid <- merge(territories, type_table,
                by = ".__aggregate_join_key", allow.cartesian = TRUE,
                sort = FALSE)
  grid[, `.__aggregate_join_key` := NULL]
  grid <- merge(grid, denominators, by = territory_columns,
                all.x = TRUE, sort = FALSE)
  key_columns <- c(territory_columns, "TYPEQU")
  spine_arrow <- arrow::arrow_table(assigned[
    , c("address_id", territory_columns), with = FALSE
  ])
  output <- data.table::copy(grid)
  output[, n_observed := 0L]

  for (metric in value_columns) {
    query_columns <- c("address_id", "TYPEQU", metric)
    keys <- c(territory_columns, "TYPEQU", metric)
    histogram <- dataset |>
      dplyr::select(dplyr::all_of(query_columns)) |>
      dplyr::left_join(spine_arrow, by = "address_id") |>
      dplyr::filter(
        !is.na(.data[[territory_columns[[1L]]]]),
        !is.na(.data[[metric]])
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(n_rows = dplyr::n(), .groups = "drop") |>
      dplyr::collect()
    histogram <- data.table::as.data.table(histogram)

    if (nrow(histogram)) {
      histogram <- merge(
        histogram, denominators, by = territory_columns,
        all.x = TRUE, sort = FALSE
      )
      metric_summary <- histogram[, {
        total <- n_addresses[[1L]]
        stats <- address_aggregate_summary_weighted_values(
          get(metric), n_rows, total, probs
        )
        stats
      }, by = key_columns]
      if (metric == value_columns[[1L]]) {
        observed <- histogram[, .(n_observed = sum(n_rows)), by = key_columns]
        output[observed, on = key_columns, n_observed := i.n_observed]
      }
      output <- merge(output, metric_summary, by = key_columns,
                      all.x = TRUE, sort = FALSE,
                      suffixes = c("", ""))
      metric_names <- paste0(metric, "_",
                             address_aggregate_stat_suffixes(probs))
      data.table::setnames(
        output,
        old = setdiff(names(metric_summary), key_columns),
        new = metric_names
      )
    }

    metric_names <- paste0(metric, "_",
                           address_aggregate_stat_suffixes(probs))
    for (name in metric_names) {
      if (!name %in% names(output)) output[, (name) := 0]
      output[is.na(get(name)), (name) := 0]
    }
  }

  data.table::setcolorder(output, c(
    territory_columns, "TYPEQU", "n_addresses", "n_observed",
    unlist(lapply(value_columns, function(metric) {
      paste0(metric, "_", address_aggregate_stat_suffixes(probs))
    }), use.names = FALSE)
  ))
  data.table::setorderv(output, c(territory_columns, "TYPEQU"))
  output[]
}

#' The public geography levels emitted by the address publisher.
#'
#' Commune rows use the commune COG key; the publisher attaches the remaining
#' descriptive COG/EPCI attributes.  Coarser rows use stable codes only; their
#' statistics are recomputed from the address histograms, rather than averaging
#' commune summaries.  An absent EPCI assignment is valid at commune level but
#' is excluded from the EPCI level.
#' @export
address_aggregate_default_territory_levels <- function() {
  list(
    commune = c("code_insee", "code_departement", "nom_commune",
                "code_region"),
    epci = c("epci_code", "code_region"),
    departement = c("code_departement", "code_region"),
    region = "code_region"
  )
}

#' Aggregate a persisted address monolith at all public geography levels.
#'
#' A single Arrow scan per sparse metric produces a commune-level histogram.
#' Those weighted histograms are rolled up to EPCI, department, and region
#' before statistics are computed, preserving exact address weighting and
#' avoiding four separate scans of the 233-million-row monolith.
#'
#' @param dataset An Arrow Dataset containing address_id, TYPEQU, and metrics.
#' @param address_spine One row per address_id with all level columns.
#' @param type_catalogue Character vector or data.frame with TYPEQU.
#' @param territory_levels Named list of character geography-key vectors.
#' @param value_columns Numeric sparse metrics to aggregate.
#' @param probs Strictly increasing quantile probabilities between 0 and 1.
#' @return A named list of data.tables, one for each geography level.
#' @export
aggregate_address_type_metrics_arrow_levels <- function(
    dataset, address_spine, type_catalogue,
    territory_levels = address_aggregate_default_territory_levels(),
    value_columns, probs = address_aggregate_default_probs()) {
  if (!inherits(dataset, c(
    "Dataset", "FileSystemDataset", "InMemoryDataset", "ArrowTabular"
  ))) {
    stop("dataset must be an Arrow Dataset or Table", call. = FALSE)
  }
  stopifnot(is.data.frame(address_spine))
  if (!is.list(territory_levels) || !length(territory_levels) ||
      is.null(names(territory_levels)) || any(!nzchar(names(territory_levels)))) {
    stop("territory_levels must be a named non-empty list", call. = FALSE)
  }
  if (any(vapply(territory_levels, function(x) {
    !is.character(x) || !length(x) || anyNA(x) || anyDuplicated(x)
  }, logical(1L)))) {
    stop("each territory level must contain unique non-empty column names",
         call. = FALSE)
  }
  if (!is.character(value_columns) || !length(value_columns) ||
      anyNA(value_columns) || anyDuplicated(value_columns)) {
    stop("value_columns must contain unique non-empty column names",
         call. = FALSE)
  }
  probs <- address_aggregate_validate_probs(probs)
  types <- address_aggregate_type_catalogue(type_catalogue)
  spine <- data.table::as.data.table(data.table::copy(address_spine))
  source_columns <- unique(unlist(territory_levels, use.names = FALSE))
  missing <- setdiff(c("address_id", source_columns), names(spine))
  if (length(missing)) {
    stop("address_spine missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(spine[["address_id"]])) {
    stop("address_spine must contain one row per address_id", call. = FALSE)
  }
  spine[, address_id := as.character(address_id)]
  assigned_by_level <- lapply(territory_levels, function(columns) {
    spine[stats::complete.cases(spine[, columns, with = FALSE])]
  })
  if (!any(vapply(assigned_by_level, nrow, integer(1L)) > 0L)) {
    stop("address_spine has no complete territory assignments", call. = FALSE)
  }

  denominators <- lapply(territory_levels, function(columns) {
    assigned <- spine[stats::complete.cases(spine[, columns, with = FALSE])]
    assigned[, .(n_addresses = .N), by = columns]
  })
  grids <- lapply(seq_along(territory_levels), function(i) {
    columns <- territory_levels[[i]]
    territories <- unique(assigned_by_level[[i]][, columns, with = FALSE])
    territories[, `.__aggregate_join_key` := 1L]
    type_table <- data.table::data.table(TYPEQU = types)
    type_table[, `.__aggregate_join_key` := 1L]
    grid <- merge(territories, type_table,
                  by = ".__aggregate_join_key", allow.cartesian = TRUE,
                  sort = FALSE)
    grid[, `.__aggregate_join_key` := NULL]
    merge(grid, denominators[[i]], by = columns, all.x = TRUE, sort = FALSE)
  })
  output <- lapply(grids, data.table::copy)
  names(output) <- names(territory_levels)
  for (i in seq_along(output)) output[[i]][, n_observed := 0L]

  spine_arrow <- arrow::arrow_table(as.data.frame(spine[
    , c("address_id", source_columns), with = FALSE
  ]))
  for (metric in value_columns) {
    query_columns <- c("address_id", "TYPEQU", metric)
    keys <- c(source_columns, "TYPEQU", metric)
    histogram <- dataset |>
      dplyr::select(dplyr::all_of(query_columns)) |>
      dplyr::left_join(spine_arrow, by = "address_id") |>
      dplyr::filter(
        !is.na(.data[[source_columns[[1L]]]]),
        !is.na(.data[[metric]])
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(n_rows = dplyr::n(), .groups = "drop") |>
      dplyr::collect()
    histogram <- data.table::as.data.table(histogram)

    for (i in seq_along(territory_levels)) {
      columns <- territory_levels[[i]]
      level_keys <- c(columns, "TYPEQU", metric)
      metric_names <- paste0(metric, "_",
                             address_aggregate_stat_suffixes(probs))
      if (nrow(histogram)) {
        level_histogram <- histogram[
          stats::complete.cases(histogram[, columns, with = FALSE]),
          .(n_rows = sum(n_rows)), by = level_keys
        ]
        level_histogram <- merge(
          level_histogram, denominators[[i]], by = columns,
          all.x = TRUE, sort = FALSE
        )
        level_histogram <- level_histogram[!is.na(n_addresses)]
        metric_summary <- level_histogram[, {
          stats <- address_aggregate_summary_weighted_values(
            get(metric), n_rows, n_addresses[[1L]], probs
          )
          stats
        }, by = c(columns, "TYPEQU")]
        if (metric == value_columns[[1L]]) {
          observed <- level_histogram[, .(n_observed = sum(n_rows)),
                                      by = c(columns, "TYPEQU")]
          output[[i]][observed, on = c(columns, "TYPEQU"),
                      n_observed := i.n_observed]
        }
        output[[i]] <- merge(
          output[[i]], metric_summary,
          by = c(columns, "TYPEQU"), all.x = TRUE, sort = FALSE,
          suffixes = c("", "")
        )
        data.table::setnames(
          output[[i]],
          old = setdiff(names(metric_summary), c(columns, "TYPEQU")),
          new = metric_names
        )
      }
      for (name in metric_names) {
        if (!name %in% names(output[[i]])) output[[i]][, (name) := 0]
        output[[i]][is.na(get(name)), (name) := 0]
      }
    }
  }

  for (i in seq_along(output)) {
    columns <- territory_levels[[i]]
    output[[i]][, n_observed := as.integer(n_observed)]
    data.table::setcolorder(output[[i]], c(
      columns, "TYPEQU", "n_addresses", "n_observed",
      unlist(lapply(value_columns, function(metric) {
        paste0(metric, "_", address_aggregate_stat_suffixes(probs))
      }), use.names = FALSE)
    ))
    data.table::setorderv(output[[i]], c(columns, "TYPEQU"))
  }
  output
}
