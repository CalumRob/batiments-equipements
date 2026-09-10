# Address accessibility monolith.
#
# The monolith is a sparse wide table at (address_id, TYPEQU) grain.  The
# helpers in this file deliberately separate the merge contract from the
# filesystem builder: every source chunk is normalized to the same key and
# then upserted, with conflicts treated as data errors rather than silently
# resolved.

address_monolith_key_columns <- function() c("address_id", "TYPEQU")

address_monolith_null_like <- function(x, n) {
  if (length(x) == 0L) return(rep(NA, n))
  rep(x[NA_integer_], n)
}

address_monolith_unique_value <- function(x) {
  present <- x[!is.na(x)]
  if (length(present) == 0L) return(x[[1L]])
  if (length(unique(present)) > 1L) {
    stop("conflicting values for duplicate monolith key", call. = FALSE)
  }
  present[[1L]]
}

#' Collapse duplicate rows that represent the same monolith key.
#'
#' Duplicate rows are valid only when every supplied non-NA value agrees.
#' This is the seam that turns several construction rows sharing a routing
#' point into one address/TYPEQU row without summing destinations.
collapse_address_monolith_rows <- function(rows,
                                           key = address_monolith_key_columns()) {
  rows <- data.table::as.data.table(rows)
  missing <- setdiff(key, names(rows))
  if (length(missing)) {
    stop("monolith rows missing key column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(rows)) return(rows[])
  if (!any(duplicated(rows, by = key))) return(rows[])

  value_columns <- setdiff(names(rows), key)
  if (!length(value_columns)) {
    return(unique(rows, by = key))
  }

  # Comparing the number of unique full rows with the number of unique keys is
  # both native and strict: NA is a value in a source row, so NA versus a
  # non-NA duplicate is a conflict rather than an invitation to guess.
  key_unique <- unique(rows, by = key)
  full_unique <- unique(rows)
  if (nrow(full_unique) != nrow(key_unique)) {
    stop("conflicting values for duplicate monolith key", call. = FALSE)
  }
  key_unique
}

#' Upsert one normalized source chunk into the sparse monolith accumulator.
#'
#' A missing value means that this source chunk did not supply that column. If
#' both sides supply a value, it must match exactly. New keys are appended;
#' existing keys are never allowed to silently choose a winner.
merge_address_monolith_rows <- function(existing, incoming,
                                        key = address_monolith_key_columns()) {
  existing <- data.table::as.data.table(existing)
  incoming <- data.table::as.data.table(incoming)

  if (!nrow(existing)) {
    missing_incoming <- setdiff(key, names(incoming))
    if (length(missing_incoming)) {
      stop("both monolith tables must contain: ",
           paste(key, collapse = ", "), call. = FALSE)
    }
    return(collapse_address_monolith_rows(incoming, key))
  }
  if (!nrow(incoming)) {
    missing_existing <- setdiff(key, names(existing))
    if (length(missing_existing)) {
      stop("both monolith tables must contain: ",
           paste(key, collapse = ", "), call. = FALSE)
    }
    return(collapse_address_monolith_rows(existing, key))
  }

  missing_existing <- setdiff(key, names(existing))
  missing_incoming <- setdiff(key, names(incoming))
  if (length(missing_existing) || length(missing_incoming)) {
    stop("both monolith tables must contain: ", paste(key, collapse = ", "),
         call. = FALSE)
  }

  existing <- collapse_address_monolith_rows(existing, key)
  incoming <- collapse_address_monolith_rows(incoming, key)

  value_columns <- union(setdiff(names(existing), key),
                         setdiff(names(incoming), key))
  for (column in setdiff(value_columns, names(existing))) {
    existing[, (column) := address_monolith_null_like(
      incoming[[column]], nrow(existing)
    )]
  }
  for (column in setdiff(value_columns, names(incoming))) {
    incoming[, (column) := address_monolith_null_like(
      existing[[column]], nrow(incoming)
    )]
  }

  data.table::setkeyv(existing, key)
  data.table::setkeyv(incoming, key)
  existing[, `__monolith_existing_row` := seq_len(.N)]
  incoming[, `__monolith_incoming_row` := seq_len(.N)]
  matched <- existing[incoming, on = key, nomatch = 0L,
                     .(existing_row = x.__monolith_existing_row,
                        incoming_row = i.__monolith_incoming_row)]
  transit_columns <- intersect(
    address_monolith_transit_gain_columns(), value_columns
  )
  if (length(transit_columns) && nrow(matched) &&
      "transit_gain_walk_available" %in% transit_columns) {
    existing_walk_available <- if ("nearest_walk" %in% names(existing)) {
      !is.na(existing[["nearest_walk"]][matched[["existing_row"]]])
    } else {
      rep(FALSE, nrow(matched))
    }
    incoming_walk_available <- if ("nearest_walk" %in% names(incoming)) {
      !is.na(incoming[["nearest_walk"]][matched[["incoming_row"]]])
    } else {
      rep(FALSE, nrow(matched))
    }
    available_conflict <- !is.na(
      existing[["transit_gain_walk_available"]][matched[["existing_row"]]]
    ) & !is.na(
      incoming[["transit_gain_walk_available"]][matched[["incoming_row"]]]
    ) & existing[["transit_gain_walk_available"]][matched[["existing_row"]]] !=
      incoming[["transit_gain_walk_available"]][matched[["incoming_row"]]]
    choose_incoming <- available_conflict &
      !existing_walk_available & incoming_walk_available
    choose_existing <- available_conflict &
      existing_walk_available & !incoming_walk_available
    if (any(choose_incoming) || any(choose_existing)) {
      existing_rows <- matched[["existing_row"]]
      incoming_rows <- matched[["incoming_row"]]
      for (column in transit_columns) {
        old <- existing[[column]][existing_rows]
        new <- incoming[[column]][incoming_rows]
        if (any(choose_incoming)) {
          data.table::set(
            existing, existing_rows[choose_incoming], column,
            new[choose_incoming]
          )
        }
        if (any(choose_existing)) {
          data.table::set(
            incoming, incoming_rows[choose_existing], column,
            old[choose_existing]
          )
        }
      }
    }
  }
  for (column in value_columns) {
    old <- existing[[column]][matched[["existing_row"]]]
    new <- incoming[[column]][matched[["incoming_row"]]]
    both <- !is.na(old) & !is.na(new)
    different <- rep(FALSE, length(old))
    different[both] <- old[both] != new[both]
    if (any(different)) {
      bad <- which(different)[[1L]]
      existing_row <- matched[["existing_row"]][[bad]]
      key_text <- paste(
        sprintf("%s=%s", key,
                vapply(key, function(name) {
                  as.character(existing[[name]][[existing_row]])
                }, character(1))),
        collapse = ", "
      )
      stop("conflicting values for monolith key in column `", column,
           "` (", key_text, "; existing=", as.character(old[[bad]]),
           "; incoming=", as.character(new[[bad]]), ")", call. = FALSE)
    }
    fill <- is.na(old) & !is.na(new)
    if (any(fill)) {
      existing[matched[["existing_row"]][fill], (column) := new[fill]]
    }
  }

  data.table::set(existing, j = "__monolith_existing_row", value = NULL)
  new_rows <- incoming[
    !get("__monolith_incoming_row") %in% matched[["incoming_row"]]
  ]
  data.table::set(new_rows, j = "__monolith_incoming_row", value = NULL)
  data.table::set(incoming, j = "__monolith_incoming_row", value = NULL)
  if (nrow(new_rows)) {
    existing <- data.table::rbindlist(
      list(existing, new_rows), use.names = TRUE, fill = FALSE
    )
  }
  data.table::setkeyv(existing, key)
  existing[]
}

#' Project base matrix rows from construction origins to address origins.
#'
#' The once-run matrix is keyed by construction id, while the address output is
#' keyed by the canonical routing point reused by one or more addresses.  The
#' projection intentionally keeps the source metric columns unchanged; callers
#' rename them to their final wide family before the monolith upsert.
project_base_address_rows <- function(matrix_rows, origin_link, address_origins) {
  matrix_rows <- data.table::as.data.table(matrix_rows)
  origin_link <- data.table::as.data.table(origin_link)
  address_origins <- data.table::as.data.table(address_origins)

  required_matrix <- c("batiment_id", "TYPEQU")
  required_link <- c("id", "point_id")
  required_addresses <- c("address_id", "routing_point_id", "routing_source")
  missing_matrix <- setdiff(required_matrix, names(matrix_rows))
  missing_link <- setdiff(required_link, names(origin_link))
  missing_addresses <- setdiff(required_addresses, names(address_origins))
  if (length(missing_matrix)) {
    stop("base matrix rows missing column(s): ",
         paste(missing_matrix, collapse = ", "), call. = FALSE)
  }
  if (length(missing_link)) {
    stop("origin link missing column(s): ",
         paste(missing_link, collapse = ", "), call. = FALSE)
  }
  if (length(missing_addresses)) {
    stop("address origins missing column(s): ",
         paste(missing_addresses, collapse = ", "), call. = FALSE)
  }
  if (!nrow(matrix_rows)) return(matrix_rows[, .(address_id = character(),
                                                 TYPEQU = character())])

  matrix_ids <- unique(as.character(matrix_rows[["batiment_id"]]))
  linked_ids <- unique(as.character(origin_link[["id"]]))
  missing_ids <- setdiff(matrix_ids, linked_ids)
  if (length(missing_ids)) {
    stop("base matrix rows missing origin-point links: ",
         paste(utils::head(missing_ids, 5L), collapse = ", "),
         call. = FALSE)
  }

  links <- unique(origin_link[, .(
    batiment_id = as.character(id),
    routing_point_id = as.character(point_id)
  )])
  addresses <- unique(address_origins[
    routing_source == "existing",
    .(address_id = as.character(address_id),
      routing_point_id = as.character(routing_point_id))
  ])

  # Collapse at the routing point before expanding to addresses. This both
  # enforces the routing-point equivalence invariant and avoids repeating the
  # same construction rows once for every address sharing that point.
  projected <- matrix_rows[links, on = "batiment_id", nomatch = 0L]
  projected[, batiment_id := NULL]
  projected <- collapse_address_monolith_rows(
    projected, key = c("routing_point_id", "TYPEQU")
  )

  # The address crosswalk is one point per address, so this indexed join only
  # expands an already-deduplicated point/type row to its address identities.
  projected <- projected[addresses, on = "routing_point_id", nomatch = 0L]
  projected[, routing_point_id := NULL]
  data.table::setcolorder(projected, c(
    "address_id", "TYPEQU",
    setdiff(names(projected), c("address_id", "TYPEQU"))
  ))
  projected[]
}

#' Rename one atomic mode's long rows into the monolith's wide columns.
normalize_address_monolith_atomic_rows <- function(rows, mode) {
  rows <- data.table::as.data.table(rows)
  mode <- tolower(as.character(mode))
  if (!mode %in% c("walk", "bike", "car")) {
    stop("monolith atomic mode must be walk, bike, or car", call. = FALSE)
  }
  required <- c("address_id", "TYPEQU", "mode", "tt_nearest", ladder_cols())
  missing <- setdiff(required, names(rows))
  if (length(missing)) {
    stop("atomic monolith rows missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(as.character(rows[["mode"]]) != mode)) {
    stop("atomic monolith rows contain the wrong mode", call. = FALSE)
  }

  output <- rows[, c("address_id", "TYPEQU", "tt_nearest", ladder_cols()),
                 with = FALSE]
  value_columns <- c("tt_nearest", ladder_cols())
  output_names <- c(
    "address_id", "TYPEQU", paste0("nearest_", mode),
    paste0(ladder_cols(), "_", mode)
  )
  data.table::setnames(output, c("address_id", "TYPEQU", value_columns),
                       output_names)
  collapse_address_monolith_rows(output)
}

#' Rename a transit-gain chunk into the monolith's derived family.
normalize_address_monolith_transit_gain_rows <- function(rows) {
  rows <- data.table::as.data.table(rows)
  required <- c(
    "address_id", "TYPEQU", "mode", "walk_available", "time_gain_p1",
    "time_gain_p50", paste0("count_gain_", ladder_rungs(), "_p1"),
    paste0("count_gain_", ladder_rungs(), "_p50")
  )
  missing <- setdiff(required, names(rows))
  if (length(missing)) {
    stop("transit-gain monolith rows missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(as.character(rows[["mode"]]) != "transit_gain")) {
    stop("transit-gain monolith rows contain the wrong mode", call. = FALSE)
  }

  value_columns <- setdiff(required, c("address_id", "TYPEQU", "mode"))
  output <- rows[, c("address_id", "TYPEQU", value_columns), with = FALSE]
  data.table::setnames(output, value_columns,
                       paste0(
                         "transit_gain_",
                         sub("^(time|count)_gain_", "\\1_", value_columns)
                       ))
  collapse_address_monolith_rows(output)
}

address_monolith_atomic_modes <- function() c("walk", "bike", "car")

address_monolith_nearest_columns <- function() {
  paste0("nearest_", address_monolith_atomic_modes())
}

address_monolith_atomic_count_columns <- function() {
  as.vector(outer(
    paste0("count_", ladder_rungs()), address_monolith_atomic_modes(),
    paste, sep = "_"
  ))
}

address_monolith_transit_gain_columns <- function() {
  c(
    "transit_gain_walk_available",
    "transit_gain_time_p1", "transit_gain_time_p50",
    paste0("transit_gain_count_", ladder_rungs(), "_p1"),
    paste0("transit_gain_count_", ladder_rungs(), "_p50")
  )
}

#' Attach the complete address spine to the sparse metric rows.
#'
#' This is deliberately a right/metric-sided join: addresses with no sparse
#' metric row are not manufactured into the monolith.  Downstream address
#' analysis must join this result onto the full address-origin spine when it
#' needs zero-access addresses.
finalize_address_monolith_rows <- function(metrics, address_origins) {
  metrics <- collapse_address_monolith_rows(metrics)
  address_origins <- data.table::as.data.table(address_origins)
  required_addresses <- c(
    "address_id", "lon", "lat", "n_non_dependance_constructions",
    "n_residential_groups", "routing_source"
  )
  missing <- setdiff(required_addresses, names(address_origins))
  if (length(missing)) {
    stop("address origins missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(address_origins[["address_id"]])) {
    stop("address origins contain duplicate address_id rows", call. = FALSE)
  }

  attrs <- address_origins[, .(
    address_id = as.character(address_id),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    n_linked_constructions = as.integer(n_non_dependance_constructions),
    n_linked_groups = as.integer(n_residential_groups),
    routing_source = as.character(routing_source)
  )]
  missing_attributes <- setdiff(
    unique(as.character(metrics[["address_id"]])),
    attrs[["address_id"]]
  )
  if (length(missing_attributes)) {
    stop("monolith metrics contain address_id values absent from address origins",
         call. = FALSE)
  }
  data.table::setkeyv(metrics, "address_id")
  data.table::setkeyv(attrs, "address_id")
  out <- metrics[attrs, on = "address_id", nomatch = 0L]
  if (nrow(out) && (anyNA(out[["lon"]]) || anyNA(out[["lat"]]))) {
    stop("address origins contain missing coordinates", call. = FALSE)
  }

  for (column in address_monolith_nearest_columns()) {
    if (!column %in% names(out)) out[, (column) := NA_real_]
  }
  for (column in address_monolith_atomic_count_columns()) {
    if (!column %in% names(out)) {
      out[, (column) := 0L]
    } else {
      value <- out[[column]]
      value[is.na(value)] <- 0L
      out[, (column) := as.integer(value)]
    }
  }
  if (!"transit_gain_walk_available" %in% names(out)) {
    out[, transit_gain_walk_available := NA]
  }
  if (!"transit_gain_time_p1" %in% names(out)) {
    out[, transit_gain_time_p1 := NA_real_]
  }
  if (!"transit_gain_time_p50" %in% names(out)) {
    out[, transit_gain_time_p50 := NA_real_]
  }
  for (column in setdiff(address_monolith_transit_gain_columns(),
                         c("transit_gain_walk_available",
                           "transit_gain_time_p1",
                           "transit_gain_time_p50"))) {
    if (!column %in% names(out)) out[, (column) := NA_integer_]
  }

  data.table::setcolorder(out, c(
    "address_id", "lon", "lat", "n_linked_constructions",
    "n_linked_groups", "routing_source", "TYPEQU",
    address_monolith_nearest_columns(),
    address_monolith_atomic_count_columns(),
    address_monolith_transit_gain_columns()
  ))
  data.table::setorderv(out, c("address_id", "TYPEQU"))
  out[]
}

project_reroute_address_rows <- function(rows, address_origins) {
  rows <- data.table::as.data.table(rows)
  address_origins <- data.table::as.data.table(address_origins)
  required <- c("batiment_id", "TYPEQU")
  missing <- setdiff(required, names(rows))
  if (length(missing)) {
    stop("reroute rows missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!"address_id" %in% names(address_origins) ||
      !"routing_source" %in% names(address_origins)) {
    stop("address origins must contain address_id and routing_source",
         call. = FALSE)
  }
  reroute_ids <- unique(as.character(address_origins[
    routing_source == "reroute", address_id
  ]))
  ids <- as.character(rows[["batiment_id"]])
  unknown <- setdiff(unique(ids), reroute_ids)
  if (length(unknown)) {
    stop("reroute rows contain non-reroute address_id values: ",
         paste(utils::head(unknown, 5L), collapse = ", "), call. = FALSE)
  }
  data.table::setnames(rows, "batiment_id", "address_id")
  rows[]
}

address_monolith_chunk_ids <- function(base_run_dir, reroute_root) {
  find_ids <- function(dirs, pattern) {
    paths <- unlist(lapply(dirs, function(dir) {
      if (!dir.exists(dir)) return(character())
      list.files(dir, pattern = pattern, full.names = FALSE)
    }), use.names = FALSE)
    as.integer(sub("^[^_]+_([0-9]+)[.]parquet$", "\\1", paths))
  }
  base_dirs <- file.path(base_run_dir, "chunks")
  reroute_dirs <- file.path(reroute_root, c("walk", "bike", "car", "transit"))
  base_ids <- find_ids(base_dirs, "^(walk|bike|car|transit)_[0-9]+[.]parquet$")
  reroute_ids <- find_ids(reroute_dirs, "^(walk|bike|car|transit)_[0-9]+[.]parquet$")
  list(
    base = sort(unique(base_ids[!is.na(base_ids)])),
    reroute = sort(unique(reroute_ids[!is.na(reroute_ids)]))
  )
}

address_monolith_read_parquet <- function(path) {
  if (!file.exists(path)) {
    stop("monolith input parquet not found: ", path, call. = FALSE)
  }
  data.table::as.data.table(arrow::read_parquet(path))
}

address_monolith_upsert_atomic <- function(accumulator, rows, mode,
                                           origin_link, address_origins,
                                           base = TRUE) {
  if (!nrow(rows)) return(accumulator)
  projected <- if (base) {
    project_base_address_rows(rows, origin_link, address_origins)
  } else {
    project_reroute_address_rows(rows, address_origins)
  }
  normalized <- if (mode == "transit_gain") {
    normalize_address_monolith_transit_gain_rows(projected)
  } else {
    normalize_address_monolith_atomic_rows(projected, mode)
  }
  merge_address_monolith_rows(accumulator, normalized)
}

address_monolith_base_point_ids <- function(rows, origin_link) {
  rows <- data.table::as.data.table(rows)
  origin_link <- data.table::as.data.table(origin_link)
  ids <- unique(as.character(rows[["batiment_id"]]))
  linked_ids <- unique(as.character(origin_link[["id"]]))
  missing_ids <- setdiff(ids, linked_ids)
  if (length(missing_ids)) {
    stop("base matrix rows missing origin-point links: ",
         paste(utils::head(missing_ids, 5L), collapse = ", "),
         call. = FALSE)
  }
  links <- unique(origin_link[, .(
    batiment_id = as.character(id),
    routing_point_id = as.character(point_id)
  )])
  ids_table <- data.table::data.table(batiment_id = ids)
  unique(ids_table[links, on = "batiment_id", nomatch = 0L,
                  .(routing_point_id)])[["routing_point_id"]]
}

address_monolith_checkpoint_path <- function(checkpoint_dir, kind, chunk_id) {
  file.path(checkpoint_dir, sprintf("%s_%04d.parquet", kind, chunk_id))
}

address_monolith_write_checkpoint <- function(output, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  arrow::write_parquet(output, tmp, compression = "zstd")
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) {
    if (file.exists(tmp)) unlink(tmp)
    stop("could not promote monolith checkpoint: ", path, call. = FALSE)
  }
  invisible(path)
}

address_monolith_read_checkpoint <- function(path) {
  output <- address_monolith_read_parquet(path)
  if (any(duplicated(output, by = c("address_id", "TYPEQU")))) {
    stop("monolith checkpoint contains duplicate (address_id, TYPEQU) rows: ",
         path, call. = FALSE)
  }
  output
}

address_monolith_process_chunk <- function(
    chunk_id, base, base_run_dir, reroute_root, origin_link, address_origins,
    verbose = FALSE, progress = function(...) invisible(NULL)) {
  accumulator <- data.table::data.table()
  point_ids <- character()
  address_ids <- character()
  modes <- address_monolith_atomic_modes()
  for (mode in modes) {
    path <- if (base) {
      file.path(base_run_dir, "chunks",
                paste0(mode, "_", chunk_id, ".parquet"))
    } else {
      file.path(reroute_root, mode,
                paste0(mode, "_", chunk_id, ".parquet"))
    }
    if (!file.exists(path)) next
    if (verbose) progress(sprintf("reading %s", basename(path)))
    raw <- address_monolith_read_parquet(path)
    if (verbose) progress(sprintf("%s source rows: %s", mode,
                                  format(nrow(raw), big.mark = ",")))
    if (base) {
      point_ids <- union(point_ids,
                         address_monolith_base_point_ids(raw, origin_link))
    } else {
      address_ids <- union(address_ids, as.character(raw[["batiment_id"]]))
    }
    before_rows <- nrow(accumulator)
    accumulator <- address_monolith_upsert_atomic(
      accumulator, raw, mode, origin_link, address_origins, base = base
    )
    if (verbose) progress(sprintf(
      "%s keys added: %s; accumulator total: %s", mode,
      format(nrow(accumulator) - before_rows, big.mark = ","),
      format(nrow(accumulator), big.mark = ",")
    ))
  }

  gain_path <- if (base) {
    file.path(base_run_dir, "derived", "transit-gain",
              paste0("transit_gain_", chunk_id, ".parquet"))
  } else {
    file.path(reroute_root, "derived", "transit-gain",
              paste0("transit_gain_", chunk_id, ".parquet"))
  }
  if (file.exists(gain_path)) {
    if (verbose) progress(sprintf("reading %s", basename(gain_path)))
    raw <- address_monolith_read_parquet(gain_path)
    if (verbose) progress(sprintf("transit_gain source rows: %s",
                                  format(nrow(raw), big.mark = ",")))
    if (base) {
      point_ids <- union(point_ids,
                         address_monolith_base_point_ids(raw, origin_link))
    } else {
      address_ids <- union(address_ids, as.character(raw[["batiment_id"]]))
    }
    before_rows <- nrow(accumulator)
    accumulator <- address_monolith_upsert_atomic(
      accumulator, raw, "transit_gain", origin_link, address_origins,
      base = base
    )
    if (verbose) progress(sprintf(
      "transit_gain keys added: %s; accumulator total: %s",
      format(nrow(accumulator) - before_rows, big.mark = ","),
      format(nrow(accumulator), big.mark = ",")
    ))
  }
  list(metrics = accumulator, point_ids = point_ids,
       address_ids = address_ids)
}

#' Build the one-file sparse address accessibility monolith.
#'
#' Base construction rows are projected through the canonical origin-point
#' crosswalk; supplemental reroute rows already carry address identities.  The
#' accumulator is keyed by (address_id, TYPEQU), so every sequential join uses
#' the same conflict-and-deduplication contract.
build_address_accessibility_monolith <- function(
    base_run_dir, output_path,
    reroute_root = file.path(base_run_dir, "chunks", "reroutes"),
    chunk_ids = NULL, reroute_chunk_ids = NULL, verbose = FALSE,
    checkpoint_dir = paste0(output_path, ".checkpoints"), resume = TRUE) {
  stopifnot(is.character(base_run_dir), length(base_run_dir) == 1L)
  stopifnot(is.character(output_path), length(output_path) == 1L)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  origin_link <- address_monolith_read_parquet(
    file.path(base_run_dir, "plan", "origin_link.parquet")
  )
  address_origins <- address_monolith_read_parquet(
    file.path(reroute_root, "plan", "address_origins.parquet")
  )
  ids <- address_monolith_chunk_ids(base_run_dir, reroute_root)
  if (is.null(chunk_ids)) chunk_ids <- ids$base
  if (is.null(reroute_chunk_ids)) reroute_chunk_ids <- ids$reroute
  started <- Sys.time()
  progress <- function(text) {
    if (!verbose) return(invisible(NULL))
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    cat(sprintf("[address-monolith +%8.1fs] %s\n", elapsed, text))
    flush.console()
  }
  progress(sprintf("starting: %d base chunks + %d reroute chunks; output=%s",
                   length(chunk_ids), length(reroute_chunk_ids), output_path))

  tmp <- sprintf("%s.tmp.pid%d", output_path, Sys.getpid())
  if (file.exists(tmp)) unlink(tmp)
  writer <- NULL
  sink <- NULL
  wrote_rows <- FALSE
  completed <- FALSE
  seen_base_points <- character()
  reroute_accumulator <- data.table::data.table()
  write_chunk <- function(output) {
    table <- arrow::Table$create(output)
    if (is.null(writer)) {
      sink <<- arrow::FileOutputStream$create(tmp)
      properties <- arrow::ParquetWriterProperties$create(
        names(output), compression = "zstd"
      )
      writer <<- arrow::ParquetFileWriter$create(
        table$schema, sink, properties
      )
    }
    writer$WriteTable(table, 65536L)
    wrote_rows <<- TRUE
  }
  on.exit({
    if (!is.null(writer)) try(writer$Close(), silent = TRUE)
    if (!is.null(sink)) try(sink$close(), silent = TRUE)
    if (file.exists(tmp) && completed) unlink(tmp)
  }, add = TRUE)

  for (chunk_id in chunk_ids) {
    progress(sprintf("BASE chunk %s/%s: starting", chunk_id, length(chunk_ids)))
    checkpoint <- address_monolith_checkpoint_path(
      checkpoint_dir, "base", chunk_id
    )
    if (resume && file.exists(checkpoint)) {
      output <- address_monolith_read_checkpoint(checkpoint)
      write_chunk(output)
      progress(sprintf("BASE chunk %s reused checkpoint: %s rows", chunk_id,
                       format(nrow(output), big.mark = ",")))
      next
    }
    chunk <- address_monolith_process_chunk(
      chunk_id = chunk_id, base = TRUE, base_run_dir = base_run_dir,
      reroute_root = reroute_root, origin_link = origin_link,
      address_origins = address_origins, verbose = verbose,
      progress = progress
    )
    overlap <- intersect(seen_base_points, chunk$point_ids)
    if (length(overlap)) {
      stop("base routing points appear in multiple chunks: ",
           paste(utils::head(overlap, 5L), collapse = ", "), call. = FALSE)
    }
    seen_base_points <- union(seen_base_points, chunk$point_ids)
    if (nrow(chunk$metrics)) {
      output <- finalize_address_monolith_rows(
        chunk$metrics, address_origins
      )
      address_monolith_write_checkpoint(output, checkpoint)
      write_chunk(output)
      progress(sprintf("BASE chunk %s complete: %s rows, %s routing points",
                       chunk_id, format(nrow(chunk$metrics), big.mark = ","),
                       format(length(chunk$point_ids), big.mark = ",")))
    }
  }

  for (chunk_id in reroute_chunk_ids) {
    progress(sprintf("REROUTE chunk %s/%s: starting", chunk_id,
                     length(reroute_chunk_ids)))
    checkpoint <- address_monolith_checkpoint_path(
      checkpoint_dir, "reroute-metrics", chunk_id
    )
    if (resume && file.exists(checkpoint)) {
      metrics <- address_monolith_read_checkpoint(checkpoint)
      before_rows <- nrow(reroute_accumulator)
      reroute_accumulator <- merge_address_monolith_rows(
        reroute_accumulator, metrics
      )
      progress(sprintf(
        paste0(
          "REROUTE chunk %s reused checkpoint: %s rows; keys added: %s; ",
          "reroute accumulator: %s"
        ), chunk_id,
        format(nrow(metrics), big.mark = ","),
        format(nrow(reroute_accumulator) - before_rows, big.mark = ","),
        format(nrow(reroute_accumulator), big.mark = ",")
      ))
      next
    }
    chunk <- address_monolith_process_chunk(
      chunk_id = chunk_id, base = FALSE, base_run_dir = base_run_dir,
      reroute_root = reroute_root, origin_link = origin_link,
      address_origins = address_origins, verbose = verbose,
      progress = progress
    )
    if (nrow(chunk$metrics)) {
      address_monolith_write_checkpoint(chunk$metrics, checkpoint)
      before_rows <- nrow(reroute_accumulator)
      reroute_accumulator <- merge_address_monolith_rows(
        reroute_accumulator, chunk$metrics
      )
      progress(sprintf("REROUTE chunk %s complete: %s rows, %s addresses",
                       chunk_id, format(nrow(chunk$metrics), big.mark = ","),
                       format(length(chunk$address_ids), big.mark = ",")))
      progress(sprintf("REROUTE accumulator keys added: %s; total: %s",
                       format(nrow(reroute_accumulator) - before_rows,
                              big.mark = ","),
                       format(nrow(reroute_accumulator), big.mark = ",")))
    }
  }

  if (nrow(reroute_accumulator)) {
    output <- finalize_address_monolith_rows(
      reroute_accumulator, address_origins
    )
    write_chunk(output)
    progress(sprintf("REROUTE aggregate complete: %s rows",
                     format(nrow(output), big.mark = ",")))
  }

  if (!wrote_rows) {
    stop("no accessibility rows were found for the address monolith",
         call. = FALSE)
  }
  writer$Close()
  sink$close()
  writer <- NULL
  sink <- NULL
  if (file.exists(output_path)) unlink(output_path)
  if (!file.rename(tmp, output_path)) {
    stop("could not promote address monolith output: ", output_path,
         call. = FALSE)
  }
  completed <- TRUE
  progress(sprintf("complete: %s", output_path))
  invisible(output_path)
}
