# 22-repair-gtfsx-stops.R — surgical repair of DEFECT A (the #22 gate finding):
# #25's derived gtfsx_ feeds kept only stops referenced by surviving
# stop_times, dropping STATION rows — orphaning every child's parent_station
# (r5r refuses the build on HIGH-priority ReferentialIntegrityErrors).
#
# Repair rule per affected gtfsx_ zip:
#   1. close the referential chain from the feed's RAW source artifact
#      (ancestor stops, namespaced identically), when resolvable;
#   2. BLANK any still-unresolvable parent_station (the field is optional;
#      stations are organizational and never affect routing); every blank is
#      counted and reported — nothing silent.
# Then rewrite the zip (all other entries preserved), verify ZERO dangling
# refs in the rewritten bytes, commit atomically, and emit a manifest patch
# proposal (same pin ids, new sha256/size/acquired_at, provenance appended).
#
# ORDERING NOTE: extract the archive BEFORE writing the patched stops.txt —
# the extraction must not clobber the patch (v1's bug).

suppressMessages(library(data.table))
options(width = 200)

MAIN  <- "E:/batiments-equipements"
DATA  <- file.path(MAIN, "data")
WT    <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
OUT   <- file.path(WT, ".scratch", "tracer-matrice", "research", "outputs")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

sha256 <- function(path) digest::digest(file = path, algo = "sha256")
utc_stamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

gtfs_read <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return(NULL)
  out <- suppressWarnings(try(fread(path, colClasses = "character",
                                    encoding = "UTF-8", na.strings = "",
                                    showProgress = FALSE), silent = TRUE))
  if (inherits(out, "try-error")) return(NULL)
  clean <- trimws(gsub("^\ufeff", "", names(out)), which = "both")
  setnames(out, names(out), clean)
  out
}

uz <- function(zip, base, exdir) {
  nm <- utils::unzip(zip, list = TRUE)[["Name"]]
  hit <- grep(paste0("(^|[.]/)", base, "$"), nm, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) return(invisible(FALSE))
  utils::unzip(zip, files = hit[1], exdir = exdir, overwrite = TRUE)
  invisible(TRUE)
}

m <- jsonlite::fromJSON(file.path(DATA, "manifest.json"), simplifyVector = FALSE)
gtfsx_ids <- grep("^gtfsx_", names(m[["sources"]]), value = TRUE)
cat(sprintf("manifest carries %d gtfsx_ pins\n", length(gtfsx_ids)))

report <- list(); patched_manifest <- list()

for (id in gtfsx_ids) {
  e <- m[["sources"]][[id]]
  pfx <- e[["prefix"]]
  zpath <- file.path(MAIN, e[["cached_path"]])
  if (!file.exists(zpath)) {
    report[[length(report) + 1L]] <- data.frame(
      id = id, n_stops = NA_integer_, n_dangling = NA_integer_,
      n_added = 0L, n_blanked = 0L, note = "derived zip missing")
    next
  }
  tmp <- file.path(tempdir(), paste0("fix-", id))
  unlink(tmp, recursive = TRUE); dir.create(tmp)

  ## 1. EXTRACT EVERYTHING FIRST (before any patched write lands here)
  utils::unzip(zpath, exdir = tmp, overwrite = TRUE)
  dstops <- gtfs_read(file.path(tmp, "stops.txt"))
  if (is.null(dstops)) {
    report[[length(report) + 1L]] <- data.frame(
      id = id, n_stops = NA_integer_, n_dangling = NA_integer_,
      n_added = 0L, n_blanked = 0L, note = "no readable stops.txt")
    unlink(tmp, recursive = TRUE); next
  }
  ps <- dstops[["parent_station"]]
  dang_mask <- !is.na(ps) & nzchar(ps) & !(ps %in% dstops[["stop_id"]])
  n_dang0 <- sum(dang_mask)
  if (n_dang0 == 0L) {
    report[[length(report) + 1L]] <- data.frame(
      id = id, n_stops = nrow(dstops), n_dangling = 0L,
      n_added = 0L, n_blanked = 0L, note = "clean")
    unlink(tmp, recursive = TRUE); next
  }

  ## 2. ancestor closure from the raw source, when locatable
  n_added <- 0L
  src_name <- sub(".*feed '([^']+)'.*", "\\1", e[["provenance"]])
  art_name <- sub(".*\\(artifact ([^)]+)\\).*", "\\1", e[["provenance"]])
  if (identical(art_name, e[["provenance"]])) art_name <- ""
  cand <- unique(c(
    if (nzchar(art_name)) file.path(DATA, "downloads", art_name) else NULL,
    file.path(DATA, "downloads", paste0(src_name, ".zip"))))
  cand <- cand[file.exists(cand)]
  if (length(cand)) {
    rtmp <- file.path(tempdir(), paste0("raw-", id))
    unlink(rtmp, recursive = TRUE); dir.create(rtmp)
    uz(cand[1], "stops.txt", rtmp)
    rstops <- gtfs_read(file.path(rtmp, "stops.txt"))
    unlink(rtmp, recursive = TRUE)
    if (!is.null(rstops)) {
      ridx <- setNames(seq_len(nrow(rstops)), rstops[["stop_id"]])
      strip1 <- function(x) sub(paste0("^", pfx, ":"), "", x)
      need <- unique(strip1(ps[dang_mask]))
      have <- as.character(dstops[["stop_id"]])
      frontier <- need[need %in% names(ridx)]
      seen <- character(0); keep_rows <- integer(0)
      while (length(frontier)) {
        seen <- c(seen, frontier)
        rows <- ridx[frontier]; keep_rows <- union(keep_rows, rows)
        nxt <- rstops$parent_station[rows]
        nxt <- nxt[!is.na(nxt) & nzchar(nxt) &
                     !(nxt %in% c(seen, have))]
        frontier <- unique(nxt[nxt %in% names(ridx)])
      }
      if (length(keep_rows)) {
        add <- rstops[keep_rows]
        pf <- function(x) ifelse(is.na(x) | x == "", x, paste0(pfx, ":", x))
        add[, stop_id := pf(stop_id)]
        if ("parent_station" %in% names(add))
          add[, parent_station := pf(parent_station)]
        if ("location_id" %in% names(add)) add[, location_id := pf(location_id)]
        miss_cols <- setdiff(names(dstops), names(add))
        for (mc in miss_cols) add[, (mc) := NA_character_]
        new_rows <- add[, names(dstops), with = FALSE]
        n_added <- nrow(new_rows)
        dstops <- rbindlist(list(dstops, new_rows), use.names = TRUE)
      }
    }
  }

  ## 3. blank whatever STILL dangles (counted, never silent)
  ps2 <- dstops[["parent_station"]]
  still <- !is.na(ps2) & nzchar(ps2) & !(ps2 %in% dstops[["stop_id"]])
  n_blanked <- sum(still)
  if (n_blanked) dstops[still, parent_station := ""]

  ## 4. write patched table INTO the extracted tree, rebuild, verify, commit
  fwrite(dstops, file.path(tmp, "stops.txt"), na = "")
  znew <- file.path(tmp, "rebuilt.zip")
  members <- unique(basename(list.files(tmp, recursive = TRUE)))
  members <- members[nzchar(members) & members != "rebuilt.zip"]
  zip::zip(zipfile = znew, files = members, root = tmp, recurse = TRUE)
  stopifnot(file.exists(znew), file.size(znew) > 0)

  vtmp <- file.path(tempdir(), paste0("ver-", id))
  unlink(vtmp, recursive = TRUE); dir.create(vtmp)
  uz(znew, "stops.txt", vtmp)
  vst <- gtfs_read(file.path(vtmp, "stops.txt"))
  unlink(vtmp, recursive = TRUE)
  vd <- vst[["parent_station"]]; vd <- vd[!is.na(vd) & nzchar(vd)]
  final_dang <- sum(!(vd %in% vst[["stop_id"]]))
  if (final_dang > 0L || nrow(vst) != nrow(dstops)) {
    report[[length(report) + 1L]] <- data.frame(
      id = id, n_stops = nrow(dstops), n_dangling = n_dang0,
      n_added = n_added, n_blanked = n_blanked,
      note = sprintf("VERIFY FAIL dang=%d rows=%d/%d", final_dang,
                     nrow(vst), nrow(dstops)))
    unlink(tmp, recursive = TRUE); next
  }

  ok <- file.copy(znew, zpath, overwrite = TRUE); stopifnot(ok)
  unlink(tmp, recursive = TRUE)
  report[[length(report) + 1L]] <- data.frame(
    id = id, n_stops = nrow(dstops), n_dangling = n_dang0,
    n_added = n_added, n_blanked = n_blanked, note = "PATCHED")
  patched_manifest[[id]] <- list(
    sha256 = sha256(zpath),
    size_bytes = as.integer(file.size(zpath)),
    acquired_at = utc_stamp(),
    added_stops = n_added,
    blanked_parent_refs = n_blanked
  )
}

rep <- rbindlist(report, use.names = TRUE, fill = TRUE)
fwrite(rep, file.path(OUT, "22-repair-report.csv"))
cat("\n=== REPAIR REPORT ===\n"); print(rep)
cat(sprintf("\npatched: %d / %d gtfsx_ pins\n",
            sum(rep$note == "PATCHED"), nrow(rep)))
jsonlite::write_json(patched_manifest,
                     file.path(OUT, "22-repair-manifest-patch.json"),
                     auto_unbox = TRUE, pretty = TRUE)
