# Once-run input catalogue and validation seam.
#
# This file deliberately contains no downloaded artefacts.  The URLs describe
# the authoritative upstreams; acquire_source remains the only promotion and
# sha256-pinning mechanism.

#' The conservative envelope required for the Bretagne network and its border.
#'
#' SRTM is distributed in one-degree tiles.  The envelope is intentionally
#' rounded out to tile boundaries so that a later crop cannot lose the 25 km
#' routing margin at an administrative boundary.
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
    tile_template = "https://elevation-tiles-prod.s3.amazonaws.com/skadi/{tile}/{tile}.hgt.gz",
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
      dem_path = if (elevation_on) normalizePath(elevation, mustWork = FALSE) else NULL
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
  # The template contains the tile id in both the directory and filename.
  # Replace every occurrence; sub() leaves the filename as `{tile}` and the
  # public tile endpoint responds with 404.
  url <- gsub("{tile}", tile_id, s[["tile_template"]], fixed = TRUE)
  if (grepl("{tile}", url, fixed = TRUE)) {
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
  epsg <- tryCatch(sf::st_crs(terra::crs(r))[["epsg"]], error = function(e) NA_integer_)
  if (!identical(as.integer(epsg), 4326L)) stop("DEM CRS must be EPSG:4326", call. = FALSE)
  e <- terra::ext(r)
  if (e[["xmin"]] > bbox[["xmin"]] || e[["ymin"]] > bbox[["ymin"]] || e[["xmax"]] < bbox[["xmax"]] || e[["ymax"]] < bbox[["ymax"]]) stop("DEM raster does not cover the Bretagne + 25 km envelope", call. = FALSE)
  resolution <- terra::res(r) * 111320
  if (any(abs(resolution - resolution_m) > 5)) stop("DEM resolution is not approximately 30 m", call. = FALSE)
  if (all(is.na(terra::values(r, mat = FALSE)))) stop("DEM raster contains only no-data values", call. = FALSE)
  invisible(list(path = path, crs = "EPSG:4326", resolution_m = resolution, extent = e, no_data_checked = TRUE))
}
