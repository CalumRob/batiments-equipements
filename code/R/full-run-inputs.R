# Once-run input catalogue and validation seam.
#
# This file deliberately contains no downloaded artefacts.  The URLs describe
# the authoritative upstreams; acquire_source remains the only promotion and
# sha256-pinning mechanism.

#' The conservative envelope required for the Bretagne network and its border.
#'
#' SRTM is distributed in one-degree tiles.  The envelope is intentionally
#' rounded out to tile boundaries so that a later crop cannot lose the
#' ADR-0002 border margin at an administrative boundary.
full_run_dem_bbox <- function() {
  c(xmin = -6, ymin = 46, xmax = 3, ymax = 50)
}

#' Metadata for the authoritative KORRIGOBRET feed.
#'
#' The feed is a rolling publisher URL.  It is therefore not assigned a hash
#' in source code: the first acquisition self-pins it in manifest.json and all
#' subsequent runs enforce that pin.  This is reproducible without pretending
#' that "latest" is immutable.
korrigobret_source <- function() {
  list(
    id = "gtfs_korrigobret",
    source = "https://www.korrigo.bzh/ftp/OPENDATA/KORRIGOBRET.gtfs.zip",
    millesime = "publisher latest; sha256 pinned at acquisition",
    licence = "ODbL (data.gouv.fr dataset licence)",
    format = "GTFS ZIP",
    expected_sha256 = NULL,
    required_files = c("agency.txt", "routes.txt", "trips.txt",
                       "stops.txt", "stop_times.txt"),
    optional_files = c("calendar.txt", "calendar_dates.txt", "feed_info.txt")
  )
}

#' Metadata for the free NASA SRTMGL1 v3 elevation tiles.
#'
#' SRTM 1 arc-second (~30 m), EPSG:4326, covers 56S--60N and hence the whole
#' envelope above.  HGT tiles are the acquisition units; later code may merge
#' validated tiles to the GeoTIFF consumed by r5r.  NASA/USGS SRTM data are
#' public domain.  No tile is downloaded by this definition function.
srtm_source <- function() {
  list(
    id = "dem_srtm_gl1_v3",
    source = "https://elevation-tiles-prod.s3.amazonaws.com/skadi",
    tile_template = "https://elevation-tiles-prod.s3.amazonaws.com/skadi/{lat_band}/{tile}.hgt.gz",
    millesime = "SRTMGL1 v3 (void-filled), 1 arc-second",
    licence = "NASA/USGS public domain",
    format = "SRTM HGT (gzip)",
    crs = "EPSG:4326",
    resolution_arc_seconds = 1,
    no_data = -32768L,
    bbox = full_run_dem_bbox(),
    expected_sha256 = NULL
  )
}

#' Return the source catalogue used by tickets 13--16.
full_run_input_sources <- function() {
  list(gtfs = korrigobret_source(), dem = srtm_source())
}

#' The routing parameters recorded alongside a once-run.
#'
#' Bike is an atomic matrix mode, but r5r calls it BICYCLE and names its speed
#' argument bike_speed.  Keep those implementation details explicit in the
#' run metadata rather than relying on an r5r default.  Elevation is a network
#' input, not a matrix axis: NONE is the deliberate compatibility setting and
#' a native-elevation run records the validated DEM path.
full_run_routing_parameters <- function(bike_speed = 12, elevation = "NONE") {
  stopifnot(is.numeric(bike_speed), length(bike_speed) == 1L,
            !is.na(bike_speed), bike_speed > 0)
  stopifnot(is.character(elevation), length(elevation) == 1L,
            !is.na(elevation), nzchar(elevation))
  elevation_on <- !identical(toupper(elevation), "NONE")
  list(
    bike = list(
      matrix_mode = "bike",
      r5r_mode = "BICYCLE",
      speed_parameter = "bike_speed",
      speed_kmh = bike_speed
    ),
    elevation = list(
      enabled = elevation_on,
      setting = if (elevation_on) "native" else "NONE",
      # A symbolic setting (for example "native") is not a filesystem path;
      # the driver resolves it to the validated staged DEM later.
      dem_path = if (elevation_on && file.exists(elevation))
        normalizePath(elevation, mustWork = TRUE) else NULL
    )
  )
}

#' Acquire the pinned GTFS through the shared manifest/cache discipline.
acquire_korrigobret_gtfs <- function(data_dir = "data",
                                     manifest_path = file.path(data_dir, "manifest.json")) {
  s <- korrigobret_source()
  acquire_source(s[["id"]], s[["source"]], s[["millesime"]],
                 file.path(data_dir, "downloads"), manifest_path,
                 expected_sha256 = s[["expected_sha256"]], readers = "r5r-transit")
}

#' Acquire one SRTM tile through the shared manifest/cache discipline.
acquire_srtm_tile <- function(tile_id, data_dir = "data",
                              manifest_path = file.path(data_dir, "manifest.json")) {
  stopifnot(length(tile_id) == 1L, grepl("^[NS][0-9]{2}[EW][0-9]{3}$", tile_id))
  s <- srtm_source()
  # AWS groups HGT files by latitude band, rather than by complete tile id.
  lat_band <- substr(tile_id, 1L, 3L)
  url <- gsub("{lat_band}", lat_band, s[["tile_template"]], fixed = TRUE)
  url <- gsub("{tile}", tile_id, url, fixed = TRUE)
  if (grepl("{lat_band}", url, fixed = TRUE) || grepl("{tile}", url, fixed = TRUE)) {
    stop("unresolved SRTM tile placeholder in URL: ", url, call. = FALSE)
  }
  id <- paste0(s[["id"]], "_", tile_id)
  acquire_source(id, url, s[["millesime"]], file.path(data_dir, "downloads"),
                 manifest_path, expected_sha256 = s[["expected_sha256"]],
                 readers = "r5r-elevation")
}

#' SRTM tile names intersecting a longitude/latitude envelope.
srtm_tile_ids <- function(bbox = full_run_dem_bbox()) {
  stopifnot(is.numeric(bbox), all(c("xmin", "ymin", "xmax", "ymax") %in% names(bbox)))
  if (bbox[["xmin"]] >= bbox[["xmax"]] || bbox[["ymin"]] >= bbox[["ymax"]]) {
    stop("DEM bbox must have positive width and height", call. = FALSE)
  }
  # A boundary at an integer belongs to the tile on its left/below.
  xs <- seq(floor(bbox[["xmin"]]), ceiling(bbox[["xmax"]]) - 1L)
  ys <- seq(floor(bbox[["ymin"]]), ceiling(bbox[["ymax"]]) - 1L)
  lon <- function(x) ifelse(x < 0, sprintf("W%03d", abs(x)), sprintf("E%03d", x))
  lat <- function(y) ifelse(y < 0, sprintf("S%02d", abs(y)), sprintf("N%02d", y))
  as.vector(outer(ys, xs, function(y, x) paste0(lat(y), lon(x))))
}

#' Validate the structural contract of a KORRIGOBRET GTFS archive.
validate_korrigobret_gtfs <- function(path, source = korrigobret_source()) {
  if (!file.exists(path)) stop("GTFS input not found: ", path, call. = FALSE)
  # acquire_source deliberately caches by manifest id (without preserving the
  # upstream extension), so archive-ness must be established from the bytes,
  # not from the cache path.
  z <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) stop("GTFS input is not a readable ZIP archive: ",
                             conditionMessage(e), call. = FALSE)
  )
  if (!is.data.frame(z) || !"Name" %in% names(z)) {
    stop("GTFS input is not a readable ZIP archive", call. = FALSE)
  }
  files <- basename(z[["Name"]])
  missing <- setdiff(source[["required_files"]], files)
  if (length(missing)) stop("GTFS archive missing required files: ", paste(missing, collapse = ", "), call. = FALSE)
  if (any(z[["Length"]][match(source[["required_files"]], files)] <= 0)) stop("GTFS archive contains an empty required file", call. = FALSE)
  invisible(list(path = path, files = files, required_files = source[["required_files"]]))
}

# Read one GTFS member without trusting the archive's directory layout.  PAN
# archives have appeared both as flat entries and as entries prefixed with
# "./"; basename matching keeps the service-date gate independent of that
# packaging detail.
.gtfs_read_member <- function(path, member, required = FALSE) {
  members <- tryCatch(utils::unzip(path, list = TRUE)[["Name"]],
                      error = function(e) {
                        stop("GTFS input is not a readable ZIP archive: ",
                             conditionMessage(e), call. = FALSE)
                      })
  hit <- members[tolower(basename(members)) == tolower(member)]
  if (!length(hit)) {
    if (isTRUE(required)) {
      stop("GTFS archive missing required service-date file: ", member,
           call. = FALSE)
    }
    return(NULL)
  }
  if (length(hit) > 1L) {
    stop("GTFS archive contains multiple members named ", member,
         call. = FALSE)
  }
  out <- tryCatch(
    utils::read.csv(unz(path, hit[[1L]]), colClasses = "character",
                    check.names = FALSE, na.strings = c("", "NA")),
    error = function(e) stop("could not read GTFS member ", member, ": ",
                             conditionMessage(e), call. = FALSE)
  )
  names(out) <- sub("^\\ufeff", "", names(out))
  out
}

.gtfs_column_name <- function(x, requested) {
  hits <- names(x)[tolower(names(x)) == tolower(requested)]
  if (length(hits) > 1L) {
    stop("GTFS member contains multiple columns named ", requested,
         " (case-insensitive)", call. = FALSE)
  }
  if (!length(hits)) return(NULL)
  hits[[1L]]
}

.as_gtfs_service_date <- function(service_date) {
  if (inherits(service_date, "POSIXt")) {
    if (length(service_date) != 1L || is.na(service_date)) {
      stop("service_date must be one non-NA date", call. = FALSE)
    }
    service_date <- as.Date(format(service_date, "%Y-%m-%d", tz = "Europe/Paris"))
  } else if (inherits(service_date, "Date")) {
    if (length(service_date) != 1L || is.na(service_date)) {
      stop("service_date must be one non-NA date", call. = FALSE)
    }
  } else if (is.character(service_date) && length(service_date) == 1L &&
             !is.na(service_date)) {
    parsed <- if (grepl("^[0-9]{8}$", service_date)) {
      as.Date(service_date, format = "%Y%m%d")
    } else {
      as.Date(service_date, format = "%Y-%m-%d")
    }
    service_date <- parsed
  } else {
    stop("service_date must be one Date, ISO date string, or POSIXct", call. = FALSE)
  }
  if (is.na(service_date)) stop("service_date is not a valid date", call. = FALSE)
  service_date
}

.as_gtfs_activity_window <- function(activity_window) {
  if (is.null(activity_window)) return(NULL)
  if (is.list(activity_window)) {
    start <- activity_window$start
    end <- activity_window$end
    timezone <- activity_window$timezone
  } else {
    if (!is.character(activity_window) || length(activity_window) != 2L) {
      stop("activity_window must contain one start and one end time", call. = FALSE)
    }
    start <- activity_window[[1L]]
    end <- activity_window[[2L]]
    timezone <- "Europe/Paris"
  }
  if (!is.character(start) || length(start) != 1L || is.na(start) ||
      !is.character(end) || length(end) != 1L || is.na(end)) {
    stop("activity_window start and end must be one non-NA time string each",
         call. = FALSE)
  }
  if (is.null(timezone)) timezone <- "Europe/Paris"
  if (!is.character(timezone) || length(timezone) != 1L || is.na(timezone) ||
      !nzchar(timezone)) {
    stop("activity_window timezone must be one non-empty string", call. = FALSE)
  }
  parsed <- .gtfs_time_seconds(c(start, end))
  if (anyNA(parsed) || parsed[[1L]] >= parsed[[2L]]) {
    stop("activity_window must be an increasing pair of HH:MM:SS times",
         call. = FALSE)
  }
  list(start = start, end = end, timezone = timezone)
}

.gtfs_time_seconds <- function(values) {
  vapply(strsplit(as.character(values), ":", fixed = TRUE), function(parts) {
    if (length(parts) != 3L || any(!grepl("^[0-9]+$", parts))) return(NA_real_)
    hour <- suppressWarnings(as.numeric(parts[[1L]]))
    minute <- suppressWarnings(as.numeric(parts[[2L]]))
    second <- suppressWarnings(as.numeric(parts[[3L]]))
    if (anyNA(c(hour, minute, second)) || minute > 59 || second > 59) {
      return(NA_real_)
    }
    hour * 3600 + minute * 60 + second
  }, numeric(1L))
}

#' Count the GTFS services and trips active on one service date.
#'
#' The calculation mirrors the GTFS contract used by R5: weekday calendar
#' rows are selected by their date range, exception type 2 removes services,
#' and exception type 1 adds them.  It is intentionally ZIP-level and does not
#' start Java or build an r5r network.
gtfs_service_date_summary <- function(path, service_date) {
  if (!file.exists(path)) stop("GTFS input not found: ", path, call. = FALSE)
  d <- .as_gtfs_service_date(service_date)
  dstr <- format(d, "%Y%m%d")

  cal <- .gtfs_read_member(path, "calendar.txt")
  cald <- .gtfs_read_member(path, "calendar_dates.txt")
  trips <- .gtfs_read_member(path, "trips.txt", required = TRUE)

  if (is.null(cal) && is.null(cald)) {
    stop("GTFS archive contains neither calendar.txt nor calendar_dates.txt",
         call. = FALSE)
  }
  trip_service <- .gtfs_column_name(trips, "service_id")
  if (is.null(trip_service)) {
    stop("GTFS trips.txt is missing service_id", call. = FALSE)
  }

  active <- character(0)
  if (!is.null(cal) && nrow(cal)) {
    required <- c("service_id", "start_date", "end_date", "monday",
                  "tuesday", "wednesday", "thursday", "friday",
                  "saturday", "sunday")
    columns <- setNames(vapply(required, function(column) {
      hit <- .gtfs_column_name(cal, column)
      if (is.null(hit)) NA_character_ else hit
    }, character(1L), USE.NAMES = FALSE), required)
    missing <- required[is.na(columns)]
    if (length(missing)) {
      stop("GTFS calendar.txt is missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    dow <- c("monday", "tuesday", "wednesday", "thursday", "friday",
             "saturday", "sunday")[[as.integer(format(d, "%u"))]]
    active <- cal[[columns[["service_id"]]]][
      !is.na(suppressWarnings(as.integer(cal[[columns[["start_date"]]]]))) &
      !is.na(suppressWarnings(as.integer(cal[[columns[["end_date"]]]]))) &
      suppressWarnings(as.integer(cal[[columns[["start_date"]]]])) <= as.integer(dstr) &
      suppressWarnings(as.integer(cal[[columns[["end_date"]]]])) >= as.integer(dstr) &
      tolower(cal[[columns[[dow]]]]) == "1"]
  }

  if (!is.null(cald) && nrow(cald)) {
    service <- .gtfs_column_name(cald, "service_id")
    date <- .gtfs_column_name(cald, "date")
    exception <- .gtfs_column_name(cald, "exception_type")
    if (is.null(exception)) exception <- .gtfs_column_name(cald, "exception_date")
    missing <- c(
      if (is.null(service)) "service_id" else character(),
      if (is.null(date)) "date" else character(),
      if (is.null(exception)) "exception_type (or exception_date)" else character()
    )
    if (length(missing)) {
      stop("GTFS calendar_dates.txt is missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    on_date <- gsub("-", "", as.character(cald[[date]]), fixed = TRUE) == dstr
    removed <- cald[[service]][on_date & cald[[exception]] == "2"]
    added <- cald[[service]][on_date & cald[[exception]] == "1"]
    active <- union(setdiff(active, removed), added)
  }
  active <- sort(unique(as.character(active[!is.na(active)])), method = "radix")
  trip_services <- as.character(trips[[trip_service]])

  list(
    service_date = format(d, "%Y-%m-%d"),
    active_service_ids = active,
    n_active_services = length(active),
    n_active_trips = sum(trip_services %in% active),
    calendar_rows = if (is.null(cal)) 0L else nrow(cal),
    calendar_date_rows = if (is.null(cald)) 0L else nrow(cald)
  )
}

#' Count active trips with a stop departure in an execution-time window.
#'
#' GTFS stop times are interpreted in the feed's local service-day clock. The
#' selected once-run window is recorded with Europe/Paris semantics because the
#' candidate service date and the published Bretagne feeds are local-time
#' artefacts. The interval is half-open: the start is included and the end is
#' excluded.
gtfs_service_activity_summary <- function(path, service_date,
                                           activity_window) {
  summary <- gtfs_service_date_summary(path, service_date)
  window <- .as_gtfs_activity_window(activity_window)
  trips <- .gtfs_read_member(path, "trips.txt", required = TRUE)
  stop_times <- .gtfs_read_member(path, "stop_times.txt", required = TRUE)
  trip_id <- .gtfs_column_name(trips, "trip_id")
  service_id <- .gtfs_column_name(trips, "service_id")
  stop_trip_id <- .gtfs_column_name(stop_times, "trip_id")
  stop_id <- .gtfs_column_name(stop_times, "stop_id")
  departure <- .gtfs_column_name(stop_times, "departure_time")
  if (is.null(trip_id) || is.null(service_id)) {
    stop("GTFS trips.txt is missing trip_id or service_id", call. = FALSE)
  }
  if (is.null(stop_trip_id) || is.null(stop_id) || is.null(departure)) {
    stop("GTFS stop_times.txt is missing trip_id, stop_id, or departure_time",
         call. = FALSE)
  }
  active_trip_ids <- as.character(trips[[trip_id]])[
    as.character(trips[[service_id]]) %in% summary$active_service_ids
  ]
  stop_trip_ids <- as.character(stop_times[[stop_trip_id]])
  stop_ids <- as.character(stop_times[[stop_id]])
  departure_seconds <- .gtfs_time_seconds(stop_times[[departure]])
  active_stops <- stop_trip_ids %in% active_trip_ids &
    !is.na(stop_ids) & nzchar(stop_ids)
  in_window <- stop_trip_ids %in% active_trip_ids &
    !is.na(departure_seconds) &
    departure_seconds >= .gtfs_time_seconds(window$start) &
    departure_seconds < .gtfs_time_seconds(window$end)
  summary$n_window_trips <- length(unique(stop_trip_ids[in_window]))
  summary$n_active_stops <- length(unique(stop_ids[active_stops]))
  summary$activity_window <- window
  summary
}

#' Make a run-only GTFS copy whose target date carries a historical offer.
#'
#' This is an explicit proxy seam for feeds whose publisher has not yet
#' published the candidate service window.  The active service ids on
#' `source_service_date` are added as `calendar_dates.txt` exceptions on
#' `target_service_date`; the source archive is never modified.  Existing
#' exceptions on the target date are replaced so the target represents the
#' selected historical service date rather than an accidental union.
project_gtfs_service_date <- function(path, source_service_date,
                                       target_service_date, output_path) {
  if (!file.exists(path)) {
    stop("GTFS input not found: ", path, call. = FALSE)
  }
  source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  source_date <- .as_gtfs_service_date(source_service_date)
  target_date <- .as_gtfs_service_date(target_service_date)
  if (identical(source_date, target_date)) {
    stop("historical proxy source and target dates must differ", call. = FALSE)
  }
  stopifnot(is.character(output_path), length(output_path) == 1L,
            !is.na(output_path), nzchar(output_path))
  target_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  if (identical(source_path, target_path)) {
    stop("historical proxy refuses to modify the source archive in place",
         call. = FALSE)
  }

  source_summary <- gtfs_service_date_summary(source_path, source_date)
  active_ids <- source_summary$active_service_ids
  if (!length(active_ids)) {
    stop("historical proxy source date has no active services: ",
         source_summary$service_date, call. = FALSE)
  }

  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  work <- tempfile("gtfs-historical-proxy-")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  tryCatch(
    utils::unzip(source_path, exdir = work, overwrite = TRUE),
    error = function(e) stop("could not extract GTFS source for proxy: ",
                             conditionMessage(e), call. = FALSE)
  )

  members <- list.files(work, recursive = TRUE, full.names = TRUE,
                        include.dirs = FALSE)
  cald_path <- members[tolower(basename(members)) == "calendar_dates.txt"]
  if (length(cald_path) > 1L) {
    stop("GTFS source contains multiple calendar_dates.txt members",
         call. = FALSE)
  }
  if (!length(cald_path)) {
    cald_path <- file.path(work, "calendar_dates.txt")
    calendar_dates <- data.frame(
      service_id = character(), date = character(),
      exception_type = character(), stringsAsFactors = FALSE
    )
  } else {
    calendar_dates <- tryCatch(
      utils::read.csv(cald_path[[1L]], colClasses = "character",
                      check.names = FALSE, na.strings = c("", "NA")),
      error = function(e) stop("could not read calendar_dates.txt for proxy: ",
                               conditionMessage(e), call. = FALSE)
    )
    names(calendar_dates) <- sub("^\\ufeff", "", names(calendar_dates))
  }
  service <- .gtfs_column_name(calendar_dates, "service_id")
  date <- .gtfs_column_name(calendar_dates, "date")
  exception <- .gtfs_column_name(calendar_dates, "exception_type")
  if (is.null(exception)) exception <- .gtfs_column_name(calendar_dates, "exception_date")
  missing <- c(
    if (is.null(service)) "service_id" else character(),
    if (is.null(date)) "date" else character(),
    if (is.null(exception)) "exception_type (or exception_date)" else character()
  )
  if (length(missing)) {
    stop("GTFS calendar_dates.txt is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  target_string <- format(target_date, "%Y%m%d")
  existing_dates <- gsub("-", "", as.character(calendar_dates[[date]]),
                         fixed = TRUE)
  keep <- is.na(existing_dates) | existing_dates != target_string
  calendar_dates <- calendar_dates[keep, , drop = FALSE]
  projected <- data.frame(
    setNames(
      list(as.character(active_ids), rep(target_string, length(active_ids)),
           rep("1", length(active_ids))),
      c(service, date, exception)
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  extra <- setdiff(names(calendar_dates), names(projected))
  for (column in extra) projected[[column]] <- NA_character_
  projected <- projected[, names(calendar_dates), drop = FALSE]
  calendar_dates <- rbind(calendar_dates, projected)
  utils::write.csv(calendar_dates, cald_path[[1L]], row.names = FALSE,
                   quote = FALSE, na = "")

  files <- list.files(work, recursive = TRUE, full.names = FALSE,
                       include.dirs = FALSE)
  files <- sort(files, method = "radix")
  if (!length(files)) {
    stop("GTFS source extraction produced no files", call. = FALSE)
  }
  # ZIP headers carry member mtimes.  Extraction gives edited files the current
  # time, which would make an otherwise identical run-only proxy hash change
  # from one invocation to the next.  Normalize all member mtimes before
  # writing so the output hash depends on source bytes and policy, not the
  # wall clock.
  fixed_time <- as.POSIXct("1980-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(file.path(work, files), fixed_time)
  tmp <- tempfile("gtfs-historical-proxy-", tmpdir = dirname(target_path),
                  fileext = ".zip")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  tryCatch(
    zip::zip(zipfile = tmp, files = files, root = work,
             include_directories = FALSE),
    error = function(e) stop("could not write GTFS historical proxy: ",
                             conditionMessage(e), call. = FALSE)
  )
  promote_temp_file(tmp, target_path)
  target_path <- normalizePath(target_path, winslash = "/", mustWork = TRUE)
  target_summary <- gtfs_service_date_summary(target_path, target_date)

  list(
    policy = "historical-proxy",
    source_path = source_path,
    output_path = target_path,
    source_service_date = source_summary$service_date,
    target_service_date = target_summary$service_date,
    source_sha256 = sha256_file(source_path),
    sha256 = sha256_file(target_path),
    source_summary = source_summary,
    target_summary = target_summary
  )
}

#' Materialize the accepted Vit'obus historical proxy.
#'
#' The selected `gtfsx_vitre` artifact is already the repaired, namespaced copy
#' of PAN resource 83276.  Resolve that artifact from the manifest rather than
#' reaching for the distinct 03-HERVE resource 83280.  The generated archive is
#' a run-only override; the acquisition manifest and its source bytes remain
#' unchanged.
vitobus_historical_proxy <- function(
    data_dir = "data",
    manifest_path = file.path(data_dir, "manifest.json"),
    output_path = NULL,
    source_service_date = as.Date("2025-09-17"),
    target_service_date = as.Date("2026-09-16")) {
  manifest <- manifest_load(manifest_path)
  entry <- manifest$sources[["gtfsx_vitre"]]
  if (is.null(entry)) {
    stop("manifest has no selected gtfsx_vitre source", call. = FALSE)
  }
  if (is.null(entry$cached_path) || length(entry$cached_path) != 1L ||
      is.na(entry$cached_path) || !nzchar(entry$cached_path)) {
    stop("selected gtfsx_vitre source has no cached_path", call. = FALSE)
  }
  source_path <- resolve_cached_path(entry$cached_path, data_dir = data_dir)
  if (is.null(output_path)) {
    output_path <- file.path(data_dir, "downloads", "derived",
                             "vitre__urban-83276__historical-proxy.zip")
  }
  proxy <- project_gtfs_service_date(
    source_path,
    source_service_date = source_service_date,
    target_service_date = target_service_date,
    output_path = output_path
  )
  proxy$feed_id <- "gtfsx_vitre"
  proxy$source_resource_id <- "83276"
  proxy
}

#' Refuse a GTFS feed that has no trips on the requested service date.
validate_gtfs_service_date <- function(path, service_date,
                                       feed_id = basename(path)) {
  stopifnot(is.character(feed_id), length(feed_id) == 1L,
            !is.na(feed_id), nzchar(feed_id))
  summary <- gtfs_service_date_summary(path, service_date)
  if (summary$n_active_trips < 1L) {
    stop(sprintf(
      "GTFS service-date gate failed for %s: no active trips on %s",
      feed_id, summary$service_date
    ), call. = FALSE)
  }
  invisible(summary)
}

.validate_transit_selection_service_date <- function(selection, service_date,
                                                     resolved_paths,
                                                     required_ids = NULL,
                                                     activity_window = NULL) {
  ids <- vapply(selection, function(e) as.character(e$id), character(1L))
  if (is.null(required_ids)) required_ids <- ids
  if (!is.character(required_ids) || !length(required_ids) ||
      anyNA(required_ids) || any(!nzchar(required_ids))) {
    stop("required_ids must contain at least one non-empty feed id", call. = FALSE)
  }
  unknown <- setdiff(required_ids, ids)
  if (length(unknown)) {
    stop("required service-date feed id(s) not selected: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  summaries <- lapply(seq_along(selection), function(i) {
    if (is.null(activity_window)) {
      gtfs_service_date_summary(resolved_paths[[ids[[i]]]], service_date)
    } else {
      gtfs_service_activity_summary(resolved_paths[[ids[[i]]]], service_date,
                                    activity_window)
    }
  })
  names(summaries) <- ids
  date_failures <- required_ids[vapply(required_ids, function(id) {
    summaries[[id]]$n_active_trips < 1L
  }, logical(1L))]
  activity_failures <- if (is.null(activity_window)) character(0) else
    required_ids[vapply(required_ids, function(id) {
      summaries[[id]]$n_window_trips < 1L
    }, logical(1L))]
  stop_failures <- if (is.null(activity_window)) character(0) else
    required_ids[vapply(required_ids, function(id) {
      summaries[[id]]$n_active_stops < 1L
    }, logical(1L))]
  failures <- unique(c(date_failures, activity_failures, stop_failures))
  if (length(failures)) {
    detail <- vapply(failures, function(id) sprintf(
      "%s (%d active trips, %d touched stops%s)", id,
      summaries[[id]]$n_active_trips,
      if (is.null(activity_window)) NA_integer_ else
        summaries[[id]]$n_active_stops,
      if (id %in% activity_failures) ", 0 activity trips" else ""),
      character(1L))
    if (length(activity_failures) || length(stop_failures)) {
      stop(sprintf(
        "transit activity/coverage gate failed on %s for required feed(s): %s",
        summaries[[1L]]$service_date, paste(detail, collapse = ", ")
      ), call. = FALSE)
    }
    stop(sprintf(
      "transit service-date gate failed on %s for required feed(s): %s",
      summaries[[1L]]$service_date, paste(detail, collapse = ", ")
    ), call. = FALSE)
  }
  list(
    service_date = summaries[[1L]]$service_date,
    required_ids = required_ids,
    activity_window = if (is.null(activity_window)) NULL else
      .as_gtfs_activity_window(activity_window),
    feeds = summaries
  )
}

#' Check selected, pinned feeds before a release-grade transit run.
#'
#' `required_ids` is explicit because a combined set may contain seasonal feeds
#' that are legitimately inactive on the chosen date. If omitted, every
#' selected feed is required and the gate fails closed.
validate_transit_selection_service_date <- function(selection, service_date,
                                                     data_dir = "data",
                                                     required_ids = NULL,
                                                     activity_window = NULL) {
  stopifnot(is.list(selection), length(selection) > 0L)
  verified <- verify_transit_pins(selection, data_dir = data_dir)
  invisible(.validate_transit_selection_service_date(
    selection, service_date, verified$resolved_paths, required_ids,
    activity_window
  ))
}

.apply_transit_feed_overrides <- function(selection, feed_overrides) {
  if (is.null(feed_overrides)) return(selection)
  if (!is.list(feed_overrides) || is.null(names(feed_overrides)) ||
      any(!nzchar(names(feed_overrides)))) {
    stop("feed_overrides must be a named list keyed by selected feed id",
         call. = FALSE)
  }
  ids <- vapply(selection, function(e) as.character(e$id), character(1L))
  unknown <- setdiff(names(feed_overrides), ids)
  if (length(unknown)) {
    stop("feed override feed id(s) not selected: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }

  for (id in names(feed_overrides)) {
    override <- feed_overrides[[id]]
    if (!is.list(override)) {
      stop("feed override for ", id, " must be a list", call. = FALSE)
    }
    path <- override$output_path
    sha <- override$sha256
    if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
      stop("feed override for ", id, " has no output_path", call. = FALSE)
    }
    if (is.null(sha) || length(sha) != 1L || is.na(sha) || !nzchar(sha)) {
      stop("feed override for ", id, " has no sha256", call. = FALSE)
    }
    if (!file.exists(path)) {
      stop("feed override for ", id, " not found: ", path, call. = FALSE)
    }
    i <- match(id, ids)
    selection[[i]]$cached_path <- normalizePath(path, winslash = "/",
                                                 mustWork = TRUE)
    selection[[i]]$sha256 <- as.character(sha)
    selection[[i]]$staging_role <- if (is.null(override$policy))
      "override" else as.character(override$policy)
    lineage <- list(
      policy = selection[[i]]$staging_role,
      source_service_date = if (is.null(override$source_service_date)) NULL
        else as.character(override$source_service_date),
      target_service_date = if (is.null(override$target_service_date)) NULL
        else as.character(override$target_service_date),
      source_sha256 = if (is.null(override$source_sha256)) NULL
        else as.character(override$source_sha256),
      sha256 = as.character(sha)
    )
    if (!is.null(override$source_path)) {
      lineage$source_artifact <- basename(as.character(override$source_path))
    }
    if (!is.null(override$source_resource_id)) {
      lineage$source_resource_id <- as.character(override$source_resource_id)
    }
    selection[[i]]$staging_override <- lineage
  }
  selection
}

#' Validate an assembled DEM raster for r5r native elevation.
#'
#' terra is intentionally optional here; validation fails with an actionable
#' message rather than silently accepting an unchecked raster.
validate_dem_raster <- function(path, bbox = full_run_dem_bbox(), resolution_m = 30) {
  if (!file.exists(path)) stop("DEM raster not found: ", path, call. = FALSE)
  if (!requireNamespace("terra", quietly = TRUE)) stop("DEM validation requires the terra package (install outside this worktree)", call. = FALSE)
  r <- tryCatch(
    terra::rast(path),
    error = function(e) stop("DEM raster could not be read: ", path,
                             call. = FALSE)
  )
  crs <- tryCatch(sf::st_crs(terra::crs(r)), error = function(e) NA)
  # terra can preserve the WGS84 definition while dropping its authority ID
  # during merge/write. Compare the CRS semantically, rather than relying on
  # the optional authority-code field being present.
  has_crs <- !is.null(crs) && !is.na(crs[["wkt"]])
  if (!isTRUE(has_crs) || !isTRUE(crs == sf::st_crs(4326))) {
    stop("DEM CRS must be EPSG:4326", call. = FALSE)
  }
  e <- terra::ext(r)
  if (terra::xmin(e) > bbox[["xmin"]] || terra::ymin(e) > bbox[["ymin"]] ||
      terra::xmax(e) < bbox[["xmax"]] || terra::ymax(e) < bbox[["ymax"]]) {
    stop(sprintf("DEM raster does not cover the Bretagne + %g km border envelope",
                 border_width_m() / 1000), call. = FALSE)
  }
  resolution <- terra::res(r) * 111320
  if (any(abs(resolution - resolution_m) > 5)) stop("DEM resolution is not approximately 30 m", call. = FALSE)
  if (all(is.na(terra::values(r, mat = FALSE)))) stop("DEM raster contains only no-data values", call. = FALSE)
  invisible(list(path = path, crs = "EPSG:4326", resolution_m = resolution, extent = e, no_data_checked = TRUE))
}

# Locate the extensionless cache produced by acquire_source and make the
# inputs visible to r5r without putting them in the shared OSM directory.
stage_full_run_inputs <- function(network_dir, data_dir = "data",
                                  gtfs_path = file.path(data_dir, "downloads", "gtfs_korrigobret"),
                                  dem_path = NULL, bbox = full_run_dem_bbox(),
                                  require_dem = TRUE) {
  stopifnot(is.character(network_dir), length(network_dir) == 1L)
  dir.create(network_dir, recursive = TRUE, showWarnings = FALSE)
  gtfs_target <- NULL
  if (!is.null(gtfs_path)) {
    validate_korrigobret_gtfs(gtfs_path)
    gtfs_target <- file.path(network_dir, "korrigobret.gtfs.zip")
    if (!file.exists(gtfs_target) || !identical(sha256_file(gtfs_target), sha256_file(gtfs_path))) {
      if (!file.copy(gtfs_path, gtfs_target, overwrite = TRUE))
        stop("could not stage GTFS archive in network directory", call. = FALSE)
    }
  }
  stopifnot(is.logical(require_dem), length(require_dem) == 1L, !is.na(require_dem))
  if (isTRUE(require_dem) && is.null(dem_path)) {
    dem_path <- file.path(network_dir, "srtm_bretagne.tif")
    if (!file.exists(dem_path)) {
      tiles <- srtm_tile_ids(bbox)
      gz <- file.path(data_dir, "downloads", paste0("dem_srtm_gl1_v3_", tiles))
      if (any(!file.exists(gz))) {
        stop("missing cached SRTM tiles (refusing to redownload): ",
             paste(basename(gz[!file.exists(gz)]), collapse = ", "), call. = FALSE)
      }
      if (!requireNamespace("terra", quietly = TRUE))
        stop("assembling the DEM requires terra; install it outside this worktree", call. = FALSE)
      hgt <- file.path(network_dir, paste0(tiles, ".hgt"))
      for (j in seq_along(gz)) {
        if (!file.exists(hgt[[j]])) {
          if (requireNamespace("R.utils", quietly = TRUE)) {
            R.utils::gunzip(gz[[j]], destname = hgt[[j]], overwrite = TRUE, remove = FALSE)
          } else {
            stop("compressed SRTM tiles require R.utils to assemble the DEM", call. = FALSE)
          }
        }
      }
      rasters <- lapply(hgt, terra::rast)
      merged <- Reduce(terra::merge, rasters)
      # HGT coordinates are already longitude/latitude WGS84. Re-attach the
      # authority-bearing CRS after terra::merge so the written raster carries
      # EPSG:4326 without transforming any cells.
      terra::crs(merged) <- "EPSG:4326"
      terra::writeRaster(merged, dem_path, overwrite = TRUE)
    }
  }
  if (isTRUE(require_dem)) {
    dem <- validate_dem_raster(dem_path, bbox = bbox)
  } else {
    dem <- NULL
    dem_path <- NULL
  }
  list(gtfs_path = if (is.null(gtfs_target)) NULL else normalizePath(gtfs_target, mustWork = TRUE),
       gtfs_sha256 = if (is.null(gtfs_target)) NULL else sha256_file(gtfs_target),
       dem_path = if (is.null(dem_path)) NULL else normalizePath(dem_path, mustWork = TRUE),
       dem = dem)
}

#' The year-round transit feeds required by the once-run's service-date gate.
#'
#' The promoted current regime also carries seasonal and excursion feeds. They
#' may be skipped when they have no service on the requested Wednesday, but the
#' core urban networks below must fail closed rather than silently disappearing.
full_run_transit_required_ids <- function() {
  c("gtfsx_qub", "gtfsx_izilo", "gtfsx_tub", "gtfsx_coralie",
    "gtfsx_vitre")
}

#' The local-time gate used to keep school-only service outside the once-run.
#'
#' This is a feed-activity gate, not r5r's 60-minute routing departure window:
#' a selected feed must have at least one active stop departure in this
#' half-open interval before it is staged. GTFS times are local service-day
#' times; Europe/Paris is recorded explicitly for the run contract.
full_run_transit_activity_window <- function() {
  list(start = "10:00:00", end = "11:00:00", timezone = "Europe/Paris")
}

#' GTFS route types excluded from the routeable transit universe everywhere.
#'
#' Route type 715 is demand-responsive/on-demand service. It is not part of the
#' daily-reach public-transit offer, regardless of which provider publishes it.
full_run_transit_excluded_route_types <- function() c("715")

#' Derived transit feeds deliberately excluded from the current routing universe.
#'
#' The bytes remain in the acquisition manifest for provenance and possible
#' future re-audit, but these provider feeds must not be staged for the
#' once-run: Némus is outside the study envelope, Pontorson is a visitor
#' shuttle rather than daily-reach transit, Destineo contains a
#' non-regional airport feed, Brittany Ferries is not useful to this
#' reachability run, Nomad/Norm are non-Breton provider feeds, and the
#' BGC35 @v2 entry is a byte-identical auxiliary duplicate of the primary
#' BGC35 feed.
full_run_transit_excluded_ids <- function() {
  c("gtfsx_nemus", "gtfsx_des", "gtfsx_bferry", "gtfsx_nomad",
    "gtfsx_norm", "gtfsx_ponto", "gtfsx_bgc35dup")
}

#' Reasons recorded when an acquired provider feed is not routeable.
full_run_transit_exclusion_reasons <- function() {
  c(
    gtfsx_nemus = "provider feed outside Bretagne+25km boundary",
    gtfsx_des = "provider feed is a non-regional Destineo aggregate",
    gtfsx_bferry = "provider feed is not useful to this reachability run",
    gtfsx_nomad = "provider feed is a non-Breton Normandy network",
    gtfsx_norm = "provider feed is outside the Bretagne study network",
    gtfsx_ponto = "visitor shuttle is not daily-reach public transit",
    gtfsx_bgc35dup = "byte-identical auxiliary duplicate of gtfsx_bgc35"
  )
}

#' The evidence-backed school-only transit exclusions used by the rebuild.
#'
#' Route IDs and published short-name families are evaluated before the derived
#' feed namespacing step.  Prefix families here are operator-published school
#' timetable families, not a generic name-token rule: an ordinary public route
#' may serve an école, collège, or lycée without being school-only service.
#' Némus is a complete school-circuit feed in the selected manifest resource;
#' its ordinary urban lines are published by the separate urban-feed resource.
#' Trip patterns are likewise feed-specific and are used only where the audit
#' found a school-only trip mixed into an otherwise retained route.
school_only_transit_route_policy <- function() {
  list(
    whole_feeds = c("nemus", "des"),
    # Némus is a bus school-circuit feed and remains subject to the route-type
    # safety check. Destineo is deliberately rejected as a complete feed
    # because its payload includes a non-regional airport service encoded with
    # a non-bus route type; no partial route-type validation is appropriate.
    whole_feed_route_type_validation = c("nemus"),
    route_ids = list(
      star = c(
        "7-0401", "7-0403", "7-0404", "7-0405", "7-0406", "7-0407",
        "7-0408", "7-0409", "7-0410", "7-0411", "7-0420", "7-0422",
        "7-0430", "7-0432", "7-0433", "7-0434", "7-0435", "7-0436",
        "7-0437", "7-0438", "7-0441", "7-0443", "7-0444", "7-0445",
        "7-0446", "7-0451", "7-0452", "7-0453", "7-0454", "7-0455",
        "7-0457", "7-0461", "7-0466", "7-0467", "7-0471", "7-0475",
        "7-0482", "7-0491"
      ),
      tub = c("39", "40", "188", "189"),
      tudbus = c("A", "B"),
      kiceo = c("70", "74", "75", "77", "80", "82"),
      mat = c("S10A", "S509")
    ),
    route_short_name_patterns = list(
      tub = "^[PS]",
      mat = "^S",
      kiceo = "^S"
    ),
    # These feeds have been checked and contain school-period public services;
    # their school-oriented presentation is not a passenger restriction.
    reviewed_public_feeds = c("tbk"),
    trip_text_patterns = list(
      kor = "^MAURON - MAIRIE Restaurant scolaire$",
      bgc29 = "^Tr[eé]ouergat \\(Strasbourg scol\\)$",
      norm = "renfort scol$"
    ),
    # Standard bus (3), intercity coach (200), and the extended bus/school
    # classes (712/713). Global route-type exclusions, including demand-
    # responsive 715, are applied by staging before this school-only policy.
    bus_route_types = c("3", "200", "712", "713")
  )
}

#' Remove only evidence-backed school-only routes from a GTFS table pair.
#'
#' The filter is applied before identifiers are namespaced.  It checks that
#' every route selected by the policy is a bus route, removes its trips, and
#' returns the removed identifiers for rebuild provenance.  Unknown feed
#' prefixes and non-bus matches fail closed rather than silently changing the
#' transit offer.
#'
#' @param routes A GTFS routes table containing route_id and route_type.
#' @param trips A GTFS trips table containing route_id.
#' @param feed_prefix The derived-feed prefix used by the policy.
#' @return A list containing filtered routes and trips, removed route/trip ids,
#'   and whether the policy removed a complete feed.
#' @export
filter_school_only_transit_routes <- function(routes, trips, feed_prefix) {
  stopifnot(is.data.frame(routes), is.data.frame(trips),
            is.character(feed_prefix), length(feed_prefix) == 1L,
            !is.na(feed_prefix), nzchar(feed_prefix))
  if (!all(c("route_id", "route_type") %in% names(routes))) {
    stop("routes must contain route_id and route_type", call. = FALSE)
  }
  if (!"route_id" %in% names(trips)) {
    stop("trips must contain route_id", call. = FALSE)
  }

  policy <- school_only_transit_route_policy()
  known <- c(policy$whole_feeds, names(policy$route_ids),
             policy$reviewed_public_feeds,
             names(policy$trip_text_patterns))
  if (!feed_prefix %in% known) {
    stop("unknown transit school-service feed: ", feed_prefix, call. = FALSE)
  }

  route_ids <- as.character(routes[["route_id"]])
  policy_route_ids <- route_ids
  namespaced_prefix <- paste0(feed_prefix, ":")
  namespaced <- startsWith(policy_route_ids, namespaced_prefix)
  policy_route_ids[namespaced] <- substring(
    policy_route_ids[namespaced], nchar(namespaced_prefix) + 1L
  )
  if (feed_prefix %in% policy$whole_feeds) {
    matched <- !is.na(route_ids) & nzchar(route_ids)
  } else if (feed_prefix %in% names(policy$route_ids)) {
    matched <- policy_route_ids %in% policy$route_ids[[feed_prefix]]
  } else {
    matched <- rep(FALSE, length(route_ids))
  }

  short_name_pattern <- policy$route_short_name_patterns[[feed_prefix]]
  if (!is.null(short_name_pattern)) {
    if (!"route_short_name" %in% names(routes)) {
      stop("school-service route policy requires route_short_name in feed ",
           feed_prefix, call. = FALSE)
    }
    short_names <- as.character(routes[["route_short_name"]])
    short_names[is.na(short_names)] <- ""
    matched <- matched | grepl(short_name_pattern, short_names,
                               ignore.case = TRUE, perl = TRUE)
  }

  validate_route_types <- !feed_prefix %in% policy$whole_feeds ||
    feed_prefix %in% policy$whole_feed_route_type_validation
  if (any(matched) && validate_route_types) {
    route_types <- as.character(routes[["route_type"]][matched])
    if (anyNA(route_types) || any(!route_types %in% policy$bus_route_types)) {
      stop("school-service policy matched non-bus route(s) in feed ",
           feed_prefix, call. = FALSE)
    }
  }

  keep_routes <- !matched
  kept_route_ids <- route_ids[keep_routes]
  trip_route_ids <- as.character(trips[["route_id"]])
  trip_text_matched <- rep(FALSE, nrow(trips))
  patterns <- policy$trip_text_patterns[[feed_prefix]]
  if (!is.null(patterns) && length(patterns) && nrow(trips)) {
    text_columns <- intersect(c("trip_headsign", "trip_short_name"),
                              names(trips))
    if (!length(text_columns)) {
      stop("school-service trip policy requires a trip text column in feed ",
           feed_prefix, call. = FALSE)
    }
    for (column in text_columns) {
      values <- as.character(trips[[column]])
      values[is.na(values)] <- ""
      trip_text_matched <- trip_text_matched | vapply(values, function(text)
        any(vapply(patterns, function(pattern)
          grepl(pattern, text, ignore.case = TRUE, perl = TRUE), logical(1L))),
        logical(1L))
    }
    trip_route_matches <- unique(trip_route_ids[trip_text_matched])
    route_types <- as.character(routes[["route_type"]][match(
      trip_route_matches, route_ids
    )])
    if (anyNA(route_types) || any(!route_types %in% policy$bus_route_types)) {
      stop("school-service trip policy matched non-bus route(s) in feed ",
           feed_prefix, call. = FALSE)
    }
  }
  keep_trips <- trip_route_ids %in% kept_route_ids & !trip_text_matched
  subset_rows <- function(x, keep) {
    if (data.table::is.data.table(x)) x[keep] else x[keep, , drop = FALSE]
  }

  list(
    routes = subset_rows(routes, keep_routes),
    trips = subset_rows(trips, keep_trips),
    removed_route_ids = unique(route_ids[matched]),
    removed_trip_ids = if ("trip_id" %in% names(trips))
      unique(as.character(trips[["trip_id"]][!keep_trips])) else character(0),
    feed_removed = feed_prefix %in% policy$whole_feeds
  )
}

.filter_gtfs_feed_for_staging <- function(
    path, output_path, feed_prefix = NULL,
    excluded_route_types = full_run_transit_excluded_route_types()) {
  members <- tryCatch(utils::unzip(path, list = TRUE)[["Name"]],
                      error = function(e) return(NULL))
  if (is.null(members)) {
    return(list(path = path, changed = FALSE,
                removed_route_ids = character(0),
                removed_trip_ids = character(0),
                route_type_removed_route_ids = character(0),
                route_type_removed_trip_ids = character(0),
                school_removed_route_ids = character(0),
                school_removed_trip_ids = character(0)))
  }
  route_member <- members[tolower(basename(members)) == "routes.txt"]
  trips_member <- members[tolower(basename(members)) == "trips.txt"]
  if (length(route_member) != 1L || length(trips_member) != 1L) {
    return(list(path = path, changed = FALSE,
                removed_route_ids = character(0),
                removed_trip_ids = character(0),
                route_type_removed_route_ids = character(0),
                route_type_removed_trip_ids = character(0),
                school_removed_route_ids = character(0),
                school_removed_trip_ids = character(0)))
  }

  work <- tempfile("gtfs-route-type-filter-")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = work, overwrite = TRUE)
  read_extracted <- function(member) {
    out <- utils::read.csv(file.path(work, member), colClasses = "character",
                           check.names = FALSE, na.strings = c("", "NA"))
    names(out) <- sub("^\\ufeff", "", names(out))
    out
  }
  routes <- read_extracted(route_member[[1L]])
  trips <- read_extracted(trips_member[[1L]])
  if (!all(c("route_id", "route_type") %in% names(routes)) ||
      !all(c("trip_id", "route_id") %in% names(trips))) {
    stop("GTFS route-type filter requires route_id/route_type in routes.txt and ",
         "trip_id/route_id in trips.txt", call. = FALSE)
  }
  route_types <- as.character(routes[["route_type"]])
  route_ids <- as.character(routes[["route_id"]])
  type_removed_route_ids <- unique(as.character(route_ids[
    !is.na(route_types) & route_types %in% excluded_route_types
  ]))
  type_removed_trip_ids <- unique(as.character(trips[["trip_id"]][
    as.character(trips[["route_id"]]) %in% type_removed_route_ids
  ]))
  routes_after_type <- routes[!route_ids %in% type_removed_route_ids,
                              , drop = FALSE]
  trips_after_type <- trips[
    !as.character(trips[["trip_id"]]) %in% type_removed_trip_ids,
    , drop = FALSE
  ]

  school_policy <- school_only_transit_route_policy()
  school_known <- c(
    school_policy$whole_feeds, names(school_policy$route_ids),
    school_policy$reviewed_public_feeds,
    names(school_policy$trip_text_patterns)
  )
  school <- list(
    routes = routes_after_type, trips = trips_after_type,
    removed_route_ids = character(0), removed_trip_ids = character(0)
  )
  if (!is.null(feed_prefix) && feed_prefix %in% school_known) {
    school <- filter_school_only_transit_routes(
      routes_after_type, trips_after_type, feed_prefix
    )
  }
  removed_route_ids <- unique(c(type_removed_route_ids,
                                school$removed_route_ids))
  removed_trip_ids <- unique(c(type_removed_trip_ids,
                               school$removed_trip_ids))
  if (!length(removed_route_ids) && !length(removed_trip_ids)) {
    return(list(path = path, changed = FALSE,
                removed_route_ids = character(0),
                removed_trip_ids = character(0),
                route_type_removed_route_ids = character(0),
                route_type_removed_trip_ids = character(0),
                school_removed_route_ids = character(0),
                school_removed_trip_ids = character(0)))
  }
  write_extracted <- function(table, member) {
    utils::write.csv(table, file.path(work, member), row.names = FALSE,
                     quote = FALSE, na = "")
  }
  write_extracted(school$routes, route_member[[1L]])
  write_extracted(school$trips, trips_member[[1L]])

  for (member in members[tolower(basename(members)) %in%
                         c("stop_times.txt", "frequencies.txt")]) {
    table <- read_extracted(member)
    trip_column <- if ("trip_id" %in% names(table)) "trip_id" else NULL
    if (!is.null(trip_column)) {
      table <- table[!as.character(table[[trip_column]]) %in% removed_trip_ids,
                     , drop = FALSE]
      write_extracted(table, member)
    }
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  files <- list.files(work, recursive = TRUE, full.names = FALSE,
                      include.dirs = FALSE)
  files <- sort(files, method = "radix")
  fixed_time <- as.POSIXct("1980-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(file.path(work, files), fixed_time)
  zip::zip(zipfile = output_path, files = files, root = work,
           include_directories = FALSE)
  list(path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
       changed = TRUE, removed_route_ids = removed_route_ids,
       removed_trip_ids = removed_trip_ids,
       route_type_removed_route_ids = type_removed_route_ids,
       route_type_removed_trip_ids = type_removed_trip_ids,
       school_removed_route_ids = school$removed_route_ids,
       school_removed_trip_ids = school$removed_trip_ids)
}

#' Resolve the GTFS service date used by a transit once-run.
#'
#' An explicit service date wins. Otherwise the departure instant is converted
#' to Europe/Paris before extracting the date, so a UTC departure around
#' midnight cannot select the previous local service day by accident.
resolve_transit_service_date <- function(departure_datetime,
                                          service_date = NULL) {
  if (!is.null(service_date)) {
    return(format(.as_gtfs_service_date(service_date), "%Y-%m-%d"))
  }
  if (is.null(departure_datetime) ||
      !inherits(departure_datetime, "POSIXt") ||
      length(departure_datetime) != 1L || is.na(departure_datetime)) {
    stop("transit service date requires an explicit service_date or one non-NA POSIXt departure_datetime",
         call. = FALSE)
  }
  format(as.Date(format(departure_datetime, "%Y-%m-%d", tz = "Europe/Paris")),
         "%Y-%m-%d")
}

# A cheap executable contract check used by operators before a full run.
probe_run_modes <- function(modes = atomic_modes(), departure_datetime = NULL) {
  stopifnot(is.character(modes), length(modes) > 0L)
  bad <- setdiff(modes, atomic_modes())
  if (length(bad)) stop("unknown routing mode(s): ", paste(bad, collapse = ", "), call. = FALSE)
  if ("transit" %in% modes && (is.null(departure_datetime) ||
      !inherits(departure_datetime, "POSIXct") || length(departure_datetime) != 1L ||
      is.na(departure_datetime)))
    stop("transit probe requires an explicit non-NA POSIXct departure_datetime", call. = FALSE)
  invisible(list(modes = modes, r5r_modes = c(walk = "WALK", car = "CAR",
                                               bike = "BICYCLE", transit = "TRANSIT")[modes],
                 transit_requires_departure = "transit" %in% modes))
}

# --- The multi-feed staging seam (#19, ADR-0004 revision) -------------------
#
# The single-feed stage_full_run_inputs above predates ticket 25: the
# promoted manifest now carries the verified, deduplicated UNION of PAN GTFS
# — pin_key_role groups (primary-current / primary-D1 / reference-current /
# auxiliary*) plus the derived namespaced gtfsx_* set. The functions below
# stage N feeds from that manifest into an r5r network directory.

#' The coherent transit window regimes of the promoted manifest.
#'
#' No single departure date satisfies every operator yet (#25 escalation):
#' the two regimes are complementary windows, decided at execution time
#' (ADR-0004). "current" = current-vintage primaries + their derived,
#' namespaced gtfsx_* set; "D1-archive" = the late-2025 archive primaries.
transit_window_regimes <- function() c("current", "D1-archive")

transit_regime_role <- function(regime) {
  switch(match.arg(regime, transit_window_regimes()),
         "current" = "primary-current",
         "D1-archive" = "primary-D1")
}

#' Select the transit pins of one window regime from a loaded manifest.
#'
#' Pure selection over manifest_load()'s object — no filesystem access.
#'
#' Regime rules (ADR-0004 revision; corrected under the #22 gate, 2026-08-24):
#' "current" stages the derived namespaced gtfs_* set minus the complete feeds
#' named by full_run_transit_excluded_ids(). #25's ownership map makes the
#' remainder the complete routing universe (one owner feed per network, ids
#' prefixed per feed, no cross-feed collisions), while the primary-current raw
#' pins are provenance and the D1-archive regime's inputs. Co-staging a raw pin
#' with its gtfsx_ twin double-routes whole networks (STAR ×2, an unfiltered
#' Korrigo + its RIV remainder — measured on the real manifest).
#' "D1-archive" keeps exactly the primary-D1 group.
#' Rival vintages (reference-current), auxiliary archives and non-transit
#' readers never stage: every network routes exactly once.
#'
#' @return Named list of the selected manifest entries; errors on an empty
#'   selection (a regime with zero feeds means the manifest is not promoted).
select_transit_pins <- function(manifest, regime = c("current", "D1-archive")) {
  stopifnot(is.list(manifest), !is.null(manifest[["sources"]]))
  if (!is.character(regime) || length(regime) != 1L ||
      !regime %in% transit_window_regimes()) {
    stop(sprintf(
      "unknown transit window regime '%s' — must be one of: %s",
      paste(regime, collapse = ", "),
      paste(transit_window_regimes(), collapse = ", ")
    ), call. = FALSE)
  }
  role <- transit_regime_role(regime)
  ids <- names(manifest$sources)
  if (is.null(ids)) stop("no transit pins selected: the manifest carries no sources", call. = FALSE)

  keep <- vapply(ids, function(id) {
    e <- manifest$sources[[id]]
    readers <- e$readers
    if (is.null(readers) || !any(readers == "r5r-transit")) return(FALSE)
    if (regime == "current") {
      # The derived namespaced set is the current routing universe (#22 gate
      # correction), minus complete feeds rejected by the once-run source
      # policy. One owner feed remains per network, with disjoint ids.
      startsWith(id, "gtfsx_") &&
        !id %in% full_run_transit_excluded_ids()
    } else {
      r <- e$pin_key_role
      !is.null(r) && length(r) == 1L && !is.na(r) && nzchar(r) &&
        identical(r, role)
    }
  }, logical(1L))

  sel <- manifest$sources[keep]
  if (!length(sel)) {
    stop(sprintf(
      "no transit pins selected for regime '%s' (role %s) — the manifest is not the promoted set",
      regime, role
    ), call. = FALSE)
  }
  sel
}

#' Resolve a manifest cached_path to a real file.
#'
#' Promoted manifests record cached_path relative to the durable root
#' ("data/downloads/derived/x.zip"), while callers pass data_dir ("data").
#' Resolution order: as given (absolute or cwd-relative), then
#' durable-root-relative (dirname(data_dir) + path). Errors when neither
#' exists — never silently rewrites history.
resolve_cached_path <- function(cached_path, data_dir = "data") {
  stopifnot(is.character(cached_path), length(cached_path) == 1L,
            !is.na(cached_path), nzchar(cached_path))
  if (file.exists(cached_path)) return(normalizePath(cached_path, winslash = "/"))
  root_rel <- file.path(dirname(data_dir), cached_path)
  if (file.exists(root_rel)) return(normalizePath(root_rel, winslash = "/"))
  stop(sprintf(
    "cached_path '%s' not found (tried cwd-relative and '%s')",
    cached_path, root_rel
  ), call. = FALSE)
}

#' Integrity gate: verify every selected pin before anything touches r5r.
#'
#' For each entry: a pinned sha256 must exist and the cached bytes at
#' cached_path must hash back to it. ALL failures are collected and reported
#' in one error (an operator fixes one pass over the cache, not N runs).
verify_transit_pins <- function(selection, data_dir = "data") {
  stopifnot(is.list(selection), length(selection) > 0L)
  problems <- character(0)
  resolved <- list()
  for (i in seq_along(selection)) {
    e <- selection[[i]]
    id <- if (!is.null(e$id)) e$id else names(selection)[[i]]
    pin <- e$sha256
    if (is.null(pin) || length(pin) != 1L || is.na(pin) || !nzchar(pin)) {
      problems <- c(problems, sprintf("%s: no sha256 pin recorded", id))
      next
    }
    cp <- e$cached_path
    if (is.null(cp) || length(cp) != 1L || is.na(cp) || !nzchar(cp)) {
      problems <- c(problems, sprintf("%s: no cached_path recorded", id))
      next
    }
    p <- tryCatch(resolve_cached_path(cp, data_dir = data_dir),
                  error = function(err) {
                    problems <<- c(problems, sprintf("%s: %s", id,
                                                     conditionMessage(err)))
                    NULL
                  })
    if (is.null(p)) next
    observed <- tryCatch(sha256_file(p), error = function(err) {
      problems <<- c(problems, sprintf("%s: unreadable cache (%s)", id,
                                       conditionMessage(err)))
      NULL
    })
    if (is.null(observed)) next
    if (!identical(tolower(observed), tolower(as.character(pin)))) {
      problems <- c(problems, sprintf(
        "%s: sha256 mismatch — pin %s but cache hashes %s", id, pin, observed))
      next
    }
    resolved[[id]] <- p
  }
  if (length(problems)) {
    stop(sprintf(
      "transit integrity gate failed for %d feed(s) — refusing to touch r5r:\n%s",
      length(problems), paste(problems, collapse = "\n")
    ), call. = FALSE)
  }
  invisible(list(n_verified = length(resolved), resolved_paths = resolved))
}

#' Stage every transit feed of one window regime into an r5r network directory.
#'
#' The N-feed seam (#19): select -> integrity-gate -> copy. Feeds are already
#' namespaced upstream (#25 prefixes every identifier per feed), so staged
#' filenames are kept verbatim. Copies are skipped when the target already
#' holds byte-identical content (idempotent re-invocation).
#'
#' Default regime is "current" — the derived namespaced gtfsx_* set minus the
#' complete feeds rejected by full_run_transit_excluded_ids() (the #22-gate
#' correction: raw primaries are provenance/D1 inputs, never co-staged with
#' their twins); pass regime = "D1-archive" for the late-2025 archive group.
#' See transit_window_regimes(). Zero-service feeds (no readable trips.txt or 0
#' trip rows) are skipped and recorded, never staged.
#'
#' @return The COMPLETE transit identity block for cache identity and run
#'   metadata: regime, n_feeds (staged count), one record per feed carrying
#'   id, sha256, role ("derived-namespaced" for the gtfsx_* set;
#'   "primary-D1" pins under the archive regime), prefix when namespaced,
#'   staged_file (basename within network_dir), skipped (zero-service or
#'   service-date-inactive feeds: id + reason), and excluded (complete source
#'   feeds rejected before staging: id + reason + sha256).
#' @param service_date Optional GTFS service date. When supplied, the
#'   service-date gate runs after integrity verification and before any feed is
#'   copied. This is a Date, ISO date string, or POSIXct value.
#' @param required_ids Feed ids that must have at least one active trip on
#'   `service_date`. If omitted, every selected feed is required; pass the
#'   explicit year-round subset when seasonal feeds are staged.
#' @param activity_window Optional half-open local-time activity window. When
#'   supplied, feeds with no active stop departure in the interval are skipped
#'   after the service-date gate. The once-run supplies
#'   `full_run_transit_activity_window()`.
#' @param feed_overrides Optional named list keyed by selected feed id. Each
#'   value is a materialized run-only feed result with `output_path` and
#'   `sha256` (for example, `project_gtfs_service_date()`); the override is
#'   integrity-gated and recorded in the returned lineage without modifying the
#'   acquisition manifest.
stage_transit_feeds <- function(network_dir, data_dir = "data",
                                manifest_path = file.path(data_dir, "manifest.json"),
                                regime = c("current", "D1-archive"),
                                service_date = NULL, required_ids = NULL,
                                feed_overrides = NULL,
                                activity_window = NULL) {
  regime <- match.arg(regime)
  manifest <- manifest_load(manifest_path)
  selection <- select_transit_pins(manifest, regime = regime)
  required_for_staging <- if (is.null(required_ids)) character(0) else required_ids
  requested_service_date <- if (is.null(service_date)) NULL else
    .as_gtfs_service_date(service_date)
  excluded_ids <- if (identical(regime, "current")) {
    intersect(full_run_transit_excluded_ids(), names(manifest$sources))
  } else character(0)
  excluded <- lapply(excluded_ids, function(id) {
    e <- manifest$sources[[id]]
    list(
      id = id,
      reason = unname(full_run_transit_exclusion_reasons()[[id]]),
      sha256 = if (is.null(e$sha256)) NULL else as.character(e$sha256)
    )
  })
  if (identical(regime, "current") &&
      identical(format(requested_service_date, "%Y-%m-%d"), "2026-09-16") &&
      "gtfsx_vitre" %in% names(selection) &&
      !"gtfsx_vitre" %in% names(feed_overrides)) {
    feed_overrides <- c(
      list(gtfsx_vitre = vitobus_historical_proxy(
        data_dir = data_dir,
        manifest_path = manifest_path,
        target_service_date = requested_service_date
      )),
      feed_overrides
    )
  }
  selection <- .apply_transit_feed_overrides(selection, feed_overrides)
  gate <- verify_transit_pins(selection, data_dir = data_dir)
  service_gate <- if (is.null(requested_service_date)) NULL else
    .validate_transit_selection_service_date(
      selection, requested_service_date, gate$resolved_paths, required_ids,
      activity_window
    )
  if (!is.null(service_gate)) required_for_staging <- service_gate$required_ids

  dir.create(network_dir, recursive = TRUE, showWarnings = FALSE)
  feeds <- vector("list", length(selection))
  skipped <- list()
  for (i in seq_along(selection)) {
    e <- selection[[i]]
    source_path <- gate$resolved_paths[[e$id]]
    feed_prefix <- if (!is.null(e$prefix)) as.character(e$prefix) else
      sub("^gtfsx_", "", as.character(e$id))
    filtered_path <- tempfile(paste0("stage-", e$id, "-"), fileext = ".zip")
    feed_filter <- .filter_gtfs_feed_for_staging(
      source_path, filtered_path, feed_prefix = feed_prefix
    )
    src <- feed_filter$path
    if (isTRUE(feed_filter$changed)) {
      on.exit(unlink(src, force = TRUE), add = TRUE)
    }
    post_filter_coverage <- NULL
    if (!is.null(requested_service_date)) {
      post_filter_coverage <- if (is.null(activity_window)) {
        gtfs_service_date_summary(src, requested_service_date)
      } else {
        gtfs_service_activity_summary(
          src, requested_service_date, activity_window
        )
      }
      service_gate$feeds[[e$id]] <- post_filter_coverage
      if (e$id %in% required_for_staging &&
          post_filter_coverage$n_active_trips < 1L) {
        stop(sprintf(
          "required transit feed %s has no routeable trips after staging filters",
          e$id
        ), call. = FALSE)
      }
      if (e$id %in% required_for_staging && !is.null(activity_window) &&
          (post_filter_coverage$n_window_trips < 1L ||
           post_filter_coverage$n_active_stops < 1L)) {
        stop(sprintf(
          paste0("required transit feed %s fails activity/coverage after ",
                 "staging filters (%d activity trips, %d touched stops)"),
          e$id, post_filter_coverage$n_window_trips,
          post_filter_coverage$n_active_stops
        ), call. = FALSE)
      }
    }
    if (!is.null(service_gate) &&
        post_filter_coverage$n_active_trips < 1L) {
      skipped[[length(skipped) + 1L]] <- list(
        id = e$id,
        reason = paste0("inactive after staging filters on service date ",
                        service_gate$service_date, " (0 active trips)"))
      next
    }
    if (!is.null(service_gate) && !is.null(activity_window) &&
        post_filter_coverage$n_window_trips < 1L) {
      window <- service_gate$activity_window
      skipped[[length(skipped) + 1L]] <- list(
        id = e$id,
        reason = sprintf(
          "no stop departures in activity window %s-%s (%s)",
          window$start, window$end, window$timezone
        )
      )
      next
    }
    # A feed with no service cannot route and only pollutes r5r's GTFS
    # validation (#22 gate: an emptied remainder tripped HIGH-priority
    # checks). Skip it, record it, never silently.
    n_trips <- tryCatch({
      nm <- utils::unzip(src, list = TRUE)[["Name"]]
      hit <- grep("(^|[.]/)trips\\.txt$", nm, ignore.case = TRUE, value = TRUE)
      if (!length(hit)) 0L else {
        t2 <- file.path(tempdir(), paste0("stage-", e$id))
        unlink(t2, recursive = TRUE); dir.create(t2)
        on.exit(unlink(t2, recursive = TRUE), add = TRUE)
        utils::unzip(src, files = hit[1], exdir = t2, overwrite = TRUE)
        tp <- file.path(t2, hit[1])
        nrow(data.table::fread(tp, colClasses = "character",
                               showProgress = FALSE))
      }
    }, error = function(e) -1L)
    if (n_trips == 0L) {
      if (e$id %in% required_for_staging) {
        stop(sprintf(
          "required transit feed %s has no routeable trips after staging filters",
          e$id
        ), call. = FALSE)
      }
      skipped[[length(skipped) + 1L]] <- list(
        id = e$id,
        reason = "zero-service feed (no readable trips.txt or 0 trip rows)",
        route_type_filter = if (length(feed_filter$route_type_removed_route_ids) ||
                                length(feed_filter$route_type_removed_trip_ids)) list(
          excluded_route_types = full_run_transit_excluded_route_types(),
          removed_route_ids = feed_filter$route_type_removed_route_ids,
          removed_trip_ids = feed_filter$route_type_removed_trip_ids
        ) else NULL,
        school_service_filter = if (length(feed_filter$school_removed_route_ids) ||
                                    length(feed_filter$school_removed_trip_ids)) list(
          removed_route_ids = feed_filter$school_removed_route_ids,
          removed_trip_ids = feed_filter$school_removed_trip_ids
        ) else NULL
      )
      next
    }
    target <- file.path(network_dir, basename(source_path))
    if (!file.exists(target) ||
        !identical(sha256_file(target), sha256_file(src))) {
      if (!file.copy(src, target, overwrite = TRUE)) {
        stop("could not stage feed ", e$id, " into ", network_dir,
             call. = FALSE)
      }
    }
    record <- list(
      id = e$id,
      sha256 = sha256_file(src),
      role = if (!is.null(e$staging_role)) as.character(e$staging_role)
             else if (!is.null(e$pin_key_role)) as.character(e$pin_key_role)
             else "derived-namespaced",
      prefix = if (!is.null(e$prefix)) as.character(e$prefix) else NULL,
      staged_file = basename(source_path)
    )
    if (isTRUE(feed_filter$changed)) {
      record$source_sha256 <- as.character(e$sha256)
      if (length(feed_filter$route_type_removed_route_ids) ||
          length(feed_filter$route_type_removed_trip_ids)) {
        record$route_type_filter <- list(
          excluded_route_types = full_run_transit_excluded_route_types(),
          removed_route_ids = feed_filter$route_type_removed_route_ids,
          removed_trip_ids = feed_filter$route_type_removed_trip_ids
        )
      }
      if (length(feed_filter$school_removed_route_ids) ||
          length(feed_filter$school_removed_trip_ids)) {
        record$school_service_filter <- list(
          removed_route_ids = feed_filter$school_removed_route_ids,
          removed_trip_ids = feed_filter$school_removed_trip_ids
        )
      }
    }
    if (!is.null(e$staging_override)) record$override <- e$staging_override
    feeds[[i]] <- record
  }
  # skips leave holes in the preallocated list - compact before reporting
  feeds <- Filter(Negate(is.null), feeds)
  # A network directory is reusable, but its GTFS ZIP set must describe this
  # staging result exactly. In particular, a feed skipped by the service-date
  # gate must not survive from an earlier date and get rediscovered by r5r.
  staged_files <- if (length(feeds)) {
    vapply(feeds, function(feed) as.character(feed$staged_file), character(1L))
  } else character(0)
  existing_gtfs <- list.files(network_dir, pattern = "\\.zip$",
                              full.names = FALSE, ignore.case = TRUE)
  stale <- setdiff(existing_gtfs, staged_files)
  if (length(stale)) unlink(file.path(network_dir, stale), force = TRUE)
  list(
    regime = regime,
    n_feeds = length(feeds),
    feeds = feeds,
    skipped = skipped,
    excluded = excluded,
    service_coverage = service_gate
  )
}
