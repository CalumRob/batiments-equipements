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
  if (!"service_id" %in% names(trips)) {
    stop("GTFS trips.txt is missing service_id", call. = FALSE)
  }

  active <- character(0)
  if (!is.null(cal) && nrow(cal)) {
    required <- c("service_id", "start_date", "end_date", "monday",
                  "tuesday", "wednesday", "thursday", "friday",
                  "saturday", "sunday")
    missing <- setdiff(required, names(cal))
    if (length(missing)) {
      stop("GTFS calendar.txt is missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    dow <- c("monday", "tuesday", "wednesday", "thursday", "friday",
             "saturday", "sunday")[[as.integer(format(d, "%u"))]]
    start <- suppressWarnings(as.integer(cal$start_date))
    end <- suppressWarnings(as.integer(cal$end_date))
    active <- cal$service_id[!is.na(start) & !is.na(end) &
                               start <= as.integer(dstr) &
                               end >= as.integer(dstr) &
                               tolower(cal[[dow]]) == "1"]
  }

  if (!is.null(cald) && nrow(cald)) {
    required <- c("service_id", "date", "exception_type")
    missing <- setdiff(required, names(cald))
    if (length(missing)) {
      stop("GTFS calendar_dates.txt is missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    on_date <- gsub("-", "", as.character(cald$date), fixed = TRUE) == dstr
    removed <- cald$service_id[on_date & cald$exception_type == "2"]
    added <- cald$service_id[on_date & cald$exception_type == "1"]
    active <- union(setdiff(active, removed), added)
  }
  active <- sort(unique(as.character(active[!is.na(active)])), method = "radix")
  trip_services <- as.character(trips$service_id)

  list(
    service_date = format(d, "%Y-%m-%d"),
    active_service_ids = active,
    n_active_services = length(active),
    n_active_trips = sum(trip_services %in% active),
    calendar_rows = if (is.null(cal)) 0L else nrow(cal),
    calendar_date_rows = if (is.null(cald)) 0L else nrow(cald)
  )
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
                                                     required_ids = NULL) {
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
    gtfs_service_date_summary(resolved_paths[[ids[[i]]]], service_date)
  })
  names(summaries) <- ids
  failures <- required_ids[vapply(required_ids, function(id) {
    summaries[[id]]$n_active_trips < 1L
  }, logical(1L))]
  if (length(failures)) {
    detail <- vapply(failures, function(id) sprintf(
      "%s (%d active trips)", id, summaries[[id]]$n_active_trips),
      character(1L))
    stop(sprintf(
      "transit service-date gate failed on %s for required feed(s): %s",
      summaries[[1L]]$service_date, paste(detail, collapse = ", ")
    ), call. = FALSE)
  }
  list(
    service_date = summaries[[1L]]$service_date,
    required_ids = required_ids,
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
                                                     required_ids = NULL) {
  stopifnot(is.list(selection), length(selection) > 0L)
  verified <- verify_transit_pins(selection, data_dir = data_dir)
  invisible(.validate_transit_selection_service_date(
    selection, service_date, verified$resolved_paths, required_ids
  ))
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
#' "current" stages EXACTLY the derived namespaced gtfsx_* set — #25's
#' ownership map makes it the complete routing universe (one owner feed per
#' network, ids prefixed per feed, no cross-feed collisions), while the
#' primary-current raw pins are provenance and the D1-archive regime's
#' inputs. Co-staging a raw pin with its gtfsx_ twin double-routes whole
#' networks (STAR ×2, an unfiltered Korrigo + its RIV remainder — measured on
#' the real manifest). "D1-archive" keeps exactly the primary-D1 group.
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
      # The derived namespaced set IS the current routing universe (#22
      # gate correction): one owner feed per network, ids disjoint.
      startsWith(id, "gtfsx_")
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
#' Default regime is "current" — EXACTLY the derived namespaced gtfsx_* set
#' (the #22-gate correction: raw primaries are provenance/D1 inputs, never
#' co-staged with their twins); pass regime = "D1-archive" for the late-2025
#' archive group. See transit_window_regimes(). Zero-service feeds (no
#' readable trips.txt or 0 trip rows) are skipped and recorded, never staged.
#'
#' @return The COMPLETE transit identity block for cache identity and run
#'   metadata: regime, n_feeds (staged count), one record per feed carrying
#'   id, sha256, role ("derived-namespaced" for the gtfsx_* set;
#'   "primary-D1" pins under the archive regime), prefix when namespaced,
#'   staged_file (basename within network_dir), and skipped (zero-service
#'   feeds: id + reason).
#' @param service_date Optional GTFS service date. When supplied, the
#'   service-date gate runs after integrity verification and before any feed is
#'   copied. This is a Date, ISO date string, or POSIXct value.
#' @param required_ids Feed ids that must have at least one active trip on
#'   `service_date`. If omitted, every selected feed is required; pass the
#'   explicit year-round subset when seasonal feeds are staged.
stage_transit_feeds <- function(network_dir, data_dir = "data",
                                manifest_path = file.path(data_dir, "manifest.json"),
                                regime = c("current", "D1-archive"),
                                service_date = NULL, required_ids = NULL) {
  regime <- match.arg(regime)
  manifest <- manifest_load(manifest_path)
  selection <- select_transit_pins(manifest, regime = regime)
  gate <- verify_transit_pins(selection, data_dir = data_dir)
  service_gate <- if (is.null(service_date)) NULL else
    .validate_transit_selection_service_date(
      selection, service_date, gate$resolved_paths, required_ids
    )

  dir.create(network_dir, recursive = TRUE, showWarnings = FALSE)
  feeds <- vector("list", length(selection))
  skipped <- list()
  for (i in seq_along(selection)) {
    e <- selection[[i]]
    src <- gate$resolved_paths[[e$id]]
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
      skipped[[length(skipped) + 1L]] <- list(
        id = e$id,
        reason = "zero-service feed (no readable trips.txt or 0 trip rows)")
      next
    }
    target <- file.path(network_dir, basename(src))
    if (!file.exists(target) ||
        !identical(sha256_file(target), sha256_file(src))) {
      if (!file.copy(src, target, overwrite = TRUE)) {
        stop("could not stage feed ", e$id, " into ", network_dir,
             call. = FALSE)
      }
    }
    feeds[[i]] <- list(
      id = e$id,
      sha256 = as.character(e$sha256),
      role = if (!is.null(e$pin_key_role)) as.character(e$pin_key_role)
             else "derived-namespaced",
      prefix = if (!is.null(e$prefix)) as.character(e$prefix) else NULL,
      staged_file = basename(src)
    )
  }
  # skips leave holes in the preallocated list - compact before reporting
  feeds <- Filter(Negate(is.null), feeds)
  list(
    regime = regime,
    n_feeds = length(feeds),
    feeds = feeds,
    skipped = skipped,
    service_coverage = service_gate
  )
}
