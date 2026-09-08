# S23 — the derived transit-gain view: transit compared with walk without
# another routing pass.
#
# This is deliberately NOT a fourth atomic matrix mode.  It is a presentation
# derivation over the transit and walk matrix axes.  Count gains are signed
# `transit - walk` deltas at each ladder rung for p1 and p50; time gains are
# `walk - transit` so a positive value means transit is faster.  The result is
# a net aggregate difference, not an identity-level set difference: the matrix
# has already grouped destinations by (batiment_id, TYPEQU).

transit_gain_p1_count_columns <- function() {
  paste0("count_gain_", ladder_rungs(), "_p1")
}

transit_gain_p50_count_columns <- function() {
  paste0("count_gain_", ladder_rungs(), "_p50")
}

transit_gain_required_columns <- function() {
  c(
    "batiment_id", "TYPEQU", "mode", "walk_available",
    "time_gain_p1", "time_gain_p50",
    transit_gain_p1_count_columns(),
    transit_gain_p50_count_columns()
  )
}

#' Validate one derived transit-gain chunk.
#'
#' This validator is intentionally separate from `validate_matrix()`: a gain
#' view is a derived comparison artifact, not an atomic matrix mode.
#'
#' @param x A transit-gain data.frame/data.table.
#' @return TRUE invisibly when the derived-artifact contract holds.
#' @export
validate_transit_gain_rows <- function(x) {
  if (!is.data.frame(x)) {
    stop("transit-gain rows must be a data.frame or data.table", call. = FALSE)
  }
  missing <- setdiff(transit_gain_required_columns(), names(x))
  if (length(missing)) {
    stop("transit-gain rows missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  key <- c("batiment_id", "TYPEQU")
  if (anyDuplicated(x[, key, with = FALSE])) {
    stop("transit-gain rows have duplicate (batiment_id, TYPEQU) keys",
         call. = FALSE)
  }
  if (!all(x[["mode"]] == "transit_gain")) {
    stop("transit-gain rows must contain only mode = 'transit_gain'",
         call. = FALSE)
  }
  if (!is.logical(x[["walk_available"]])) {
    stop("walk_available must be logical", call. = FALSE)
  }

  for (col in c("time_gain_p1", "time_gain_p50")) {
    if (!is.numeric(x[[col]])) {
      stop(col, " must be numeric", call. = FALSE)
    }
    missing_time <- is.na(x[[col]])
    if (any(missing_time != !x[["walk_available"]])) {
      stop(col, " must be NA exactly when walk_available is FALSE",
           call. = FALSE)
    }
  }
  for (col in c(transit_gain_p1_count_columns(),
               transit_gain_p50_count_columns())) {
    value <- x[[col]]
    if (!is.numeric(value) || anyNA(value) || any(value != floor(value))) {
      stop(col, " must contain non-NA whole-number deltas", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Derive the net transit gain view for one transit/walk chunk.
#'
#' The transit input is the already-derived transit matrix chunk, with p1 and
#' p50 values. The walk input is the corresponding walk matrix chunk. Both are
#' joined at the matrix contract's `(batiment_id, TYPEQU)` grain, not at a
#' discarded coordinate-level route-pair grain.
#'
#' Count gains are signed `transit - walk` deltas. A missing walk row is treated
#' as zero for counts and is marked in `walk_available`; its time gains are NA
#' because there is no walking travel time within the sparse cap to subtract.
#' Time gains are `walk - transit`, so positive means transit is faster.
#'
#' @param transit A validated `transit` matrix chunk.
#' @param walk A validated `walk` matrix chunk.
#' @return A data.table with one row per transit matrix row and the derived
#'   signed gain columns.
#' @export
derive_transit_gain_rows <- function(transit, walk) {
  stopifnot(is.data.frame(transit), is.data.frame(walk))
  validate_matrix(transit)
  validate_matrix(walk)

  transit <- data.table::as.data.table(data.table::copy(transit))
  walk <- data.table::as.data.table(data.table::copy(walk))

  if (!all(transit[["mode"]] == "transit")) {
    stop("transit input must contain only mode = 'transit'", call. = FALSE)
  }
  if (!all(walk[["mode"]] == "walk")) {
    stop("walk input must contain only mode = 'walk'", call. = FALSE)
  }

  key <- c("batiment_id", "TYPEQU")
  if (anyDuplicated(transit[, key, with = FALSE])) {
    stop("transit input has duplicate (batiment_id, TYPEQU) rows",
         call. = FALSE)
  }
  if (anyDuplicated(walk[, key, with = FALSE])) {
    stop("walk input has duplicate (batiment_id, TYPEQU) rows",
         call. = FALSE)
  }

  walk_cols <- c("tt_nearest", ladder_cols())
  walk_view <- walk[, c(key, walk_cols), with = FALSE]
  data.table::setnames(walk_view, walk_cols, paste0("walk_", walk_cols))

  transit[, .transit_order := seq_len(.N)]
  joined <- merge(transit, walk_view, by = key, all.x = TRUE, sort = FALSE)
  data.table::setorder(joined, .transit_order)

  walk_available <- !is.na(joined[["walk_tt_nearest"]])
  walk_counts <- lapply(ladder_cols(), function(col) {
    value <- joined[[paste0("walk_", col)]]
    value[is.na(value)] <- 0L
    as.integer(value)
  })
  names(walk_counts) <- ladder_cols()

  out <- joined[, c(
    list(
      batiment_id = batiment_id,
      TYPEQU = TYPEQU,
      mode = rep("transit_gain", .N),
      walk_available = walk_available,
      time_gain_p1 = ifelse(
        walk_available, walk_tt_nearest - travel_time_p1, NA_real_),
      time_gain_p50 = ifelse(
        walk_available, walk_tt_nearest - travel_time_p50, NA_real_)
    ),
    stats::setNames(
      lapply(seq_along(ladder_cols()), function(i) {
        as.integer(get(ladder_cols()[i]) - walk_counts[[i]])
      }),
      transit_gain_p1_count_columns()
    ),
    stats::setNames(
      lapply(seq_along(ladder_cols()), function(i) {
        as.integer(get(paste0(ladder_cols()[i], "_p50")) - walk_counts[[i]])
      }),
      transit_gain_p50_count_columns()
    )
  )]

  data.table::setorder(out, batiment_id, TYPEQU)
  validate_transit_gain_rows(out)
  out[]
}

#' Derive and atomically write one transit-gain chunk.
#'
#' @param transit_path Path to one validated transit matrix parquet.
#' @param walk_path Path to the corresponding validated walk matrix parquet.
#' @param chunk_id Chunk number used in the derived filename.
#' @param out_dir Directory for `transit_gain_<chunk_id>.parquet`.
#' @param overwrite Whether an existing derived artifact may be replaced.
#' @return The derived parquet path invisibly.
#' @export
derive_transit_gain_chunk <- function(transit_path, walk_path, chunk_id,
                                      out_dir, overwrite = FALSE) {
  stopifnot(is.character(transit_path), length(transit_path) == 1L,
            !is.na(transit_path), nzchar(transit_path))
  stopifnot(is.character(walk_path), length(walk_path) == 1L,
            !is.na(walk_path), nzchar(walk_path))
  stopifnot(length(chunk_id) == 1L, !is.na(chunk_id), chunk_id >= 1L)
  stopifnot(is.character(out_dir), length(out_dir) == 1L,
            !is.na(out_dir), nzchar(out_dir))

  path <- file.path(out_dir,
                    sprintf("transit_gain_%d.parquet", as.integer(chunk_id)))
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("transit-gain artifact already exists: ", path,
         " (pass overwrite = TRUE to replace it)", call. = FALSE)
  }

  rows <- derive_transit_gain_rows(read_matrix(transit_path),
                                   read_matrix(walk_path))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- sprintf("%s.tmp.pid%d", path, Sys.getpid())
  arrow::write_parquet(rows, tmp)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  promote_temp_file(tmp, path)
  validate_transit_gain_rows(read_matrix(path))
  invisible(path)
}

#' Derive the transit-gain view for an address reroute run.
#'
#' This is a pure parquet derivation: it never opens the routing network and
#' never routes again. Each reroute transit chunk is paired with the walk chunk
#' carrying the same chunk id, then written under the run's derived namespace.
#'
#' @param reroute_root The `chunks/reroutes` directory.
#' @param chunk_ids Optional chunk ids. When omitted, the completed reroute
#'   manifest freezes the chunk count; without a manifest, matching walk and
#'   transit filenames are discovered.
#' @param overwrite Whether existing gain artifacts may be replaced.
#' @param verbose Whether to report each derived/skipped chunk.
#' @return A list containing the derived paths and chunk ids.
#' @export
derive_reroute_transit_gains <- function(reroute_root,
                                         chunk_ids = NULL,
                                         overwrite = FALSE,
                                         verbose = TRUE) {
  stopifnot(is.character(reroute_root), length(reroute_root) == 1L,
            !is.na(reroute_root), nzchar(reroute_root))
  if (!dir.exists(reroute_root)) {
    stop("reroute root does not exist: ", reroute_root, call. = FALSE)
  }

  manifest_path <- file.path(reroute_root, "manifest.json")
  manifest <- if (file.exists(manifest_path)) {
    load_run_manifest(manifest_path)
  } else NULL

  if (is.null(chunk_ids)) {
    if (!is.null(manifest)) {
      n_chunks <- as.integer(manifest$plan_census$n_chunks)
      if (is.na(n_chunks) || n_chunks < 1L) {
        stop("reroute manifest has no chunks", call. = FALSE)
      }
      chunk_ids <- seq_len(n_chunks)
      required <- unlist(lapply(chunk_ids, function(id) {
        c(chunk_entry_id("walk", id), chunk_entry_id("transit", id))
      }))
      entries <- manifest$entries[required]
      missing <- required[vapply(entries, is.null, logical(1L))]
      if (length(missing)) {
        stop("reroute manifest is missing walk/transit entries: ",
             paste(missing, collapse = ", "), call. = FALSE)
      }
      incomplete <- required[vapply(entries, function(entry) {
        !identical(entry$status, "complete")
      }, logical(1L))]
      if (length(incomplete)) {
        stop("reroute walk/transit entries are not complete: ",
             paste(incomplete, collapse = ", "), call. = FALSE)
      }
    } else {
      transit_files <- list.files(
        file.path(reroute_root, "transit"),
        pattern = "^transit_[0-9]+[.]parquet$"
      )
      walk_files <- list.files(
        file.path(reroute_root, "walk"),
        pattern = "^walk_[0-9]+[.]parquet$"
      )
      parse_ids <- function(files, prefix) {
        stem <- sub("[.]parquet$", "", files)
        as.integer(sub(sprintf("^%s_", prefix), "", stem))
      }
      transit_ids <- parse_ids(transit_files, "transit")
      walk_ids <- parse_ids(walk_files, "walk")
      if (!identical(sort(transit_ids), sort(walk_ids))) {
        stop("reroute walk/transit chunk sets do not match", call. = FALSE)
      }
      chunk_ids <- sort(transit_ids)
    }
  }
  chunk_ids <- sort(unique(as.integer(chunk_ids)))
  if (!length(chunk_ids) || anyNA(chunk_ids) || any(chunk_ids < 1L)) {
    stop("chunk_ids must contain positive integer chunk ids", call. = FALSE)
  }

  out_dir <- file.path(reroute_root, "derived", "transit-gain")
  paths <- character(length(chunk_ids))
  skipped <- logical(length(chunk_ids))
  for (i in seq_along(chunk_ids)) {
    chunk_id <- chunk_ids[[i]]
    transit_path <- file.path(reroute_root, "transit",
                              sprintf("transit_%d.parquet", chunk_id))
    walk_path <- file.path(reroute_root, "walk",
                           sprintf("walk_%d.parquet", chunk_id))
    if (!file.exists(transit_path) || !file.exists(walk_path)) {
      stop("missing walk/transit reroute artifact for chunk ", chunk_id,
           call. = FALSE)
    }
    path <- file.path(out_dir, sprintf("transit_gain_%d.parquet", chunk_id))
    if (file.exists(path) && !isTRUE(overwrite)) {
      validate_transit_gain_rows(arrow::read_parquet(path))
      skipped[[i]] <- TRUE
      if (isTRUE(verbose)) message("transit gain: skipped valid chunk ", chunk_id)
    } else {
      derive_transit_gain_chunk(
        transit_path, walk_path, chunk_id, out_dir, overwrite = overwrite
      )
      if (isTRUE(verbose)) message("transit gain: derived chunk ", chunk_id)
    }
    paths[[i]] <- path
  }

  invisible(list(
    reroute_root = reroute_root,
    output_dir = out_dir,
    chunk_ids = chunk_ids,
    n_chunks = length(chunk_ids),
    paths = paths,
    skipped = skipped
  ))
}
