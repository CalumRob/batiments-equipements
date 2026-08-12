# S9 — the OSM network reader: the once-run's routing network and the toy
# region's crop, both derived from the three pinned Geofabrik region PBFs
# (acquire.R manifest, ids "osm_geofabrik_bretagne",
# "osm_geofabrik_pays_de_la_loire", "osm_geofabrik_basse_normandie").
#
# Geofabrik ships no per-departement extract (verified at acquisition,
# 2026-08-12), so the ADR-0002 network needs THREE region files: bretagne
# covers 22/29/35/56; pays-de-la-loire covers the strip deps 44/53/49;
# basse-normandie covers 50/61. They are merged with Osmosis 0.49.2
# (read-pbf x3 -> merge x2 -> write-pbf), then cropped with
# --bounding-polygon to the Bretagne region polygon buffered by
# W + snap = 26.6 km (ADR-0002: W = 25 km fastest-mode reach at the cap,
# snap = 1.6 km r5r point-to-street link radius). The toy-region crop for
# the tracer is the Fougères Agglo EPCI polygon (SIREN 200072452, 28
# communes, dép 35) buffered by the same W + snap.
#
# Derived artifacts are cached under data/acquired/osm/, keyed by the
# pinned source sha256s (merged: the three Geofabrik pins; crop: merged
# pins + the admin-express pin behind the crop polygon) — a re-acquisition
# of any source rebuilds the cache. The osmosis invocation itself is
# wrapped in R (osmosis_run -> system2 against the bundled
# data/tools/osmosis-0.49.2/bin/osmosis.bat) so the whole pipeline is
# re-runnable from one entry point.
#
# LICENCE (ADR-0001): travel is computed on OSM data, so every computed
# output of the once-run is ODbL + (c) OpenStreetMap contributors — this
# reader's artifacts (merged + crops) inherit that licence. The raw
# Geofabrik PBFs are OSM data themselves; Geofabrik's extracts are
# published under the ODbL.
#
# Osmosis gotchas recorded at acquisition (2026-08-12):
#   * 0.49.2's --bounding-polygon reads ONLY the Osmosis .poly format via
#     PolygonFileReader (header name line -> per-polygon numeric section
#     header -> "lon lat" pairs -> END, final END). WKT is NOT accepted
#     despite the docs' implication — verified against the areafilter jar.
#   * The polygon coordinates are lon/lat in EPSG:4326 — the buffer is
#     built in EPSG:2154 (metres) then transformed (spherical 4326
#     buffering would be wrong).
#   * completeWays=yes keeps whole ways crossing the crop boundary — the
#     r5r requirement (a road must not be cut at the crop edge), at the
#     cost of pulling in some out-of-polygon geometry.
#   * Osmosis 0.49.2 runs on this machine's Java 21; give it a real heap
#     (JAVA_OPTS=-Xmx4g) — the merge of ~800 MiB of PBFs is slow (143 s)
#     and the bretagne crop ~13 min.
#   * The merged PBF of the three regions is 840,753,734 B; the bretagne
#     crop (the once-run's network) is 410,390,545 B; the fougeres toy
#     crop is 81,696,992 B.

#' The manifest ids of the three Geofabrik region PBFs.
#'
#' Bretagne (22/29/35/56), Pays de la Loire (strip deps 44/53/49), Basse
#' Normandie (strip deps 50/61). Geofabrik has no per-departement extract,
#' so the ADR-0002 strip requires all three.
osm_geofabrik_source_ids <- function() {
  c(
    "osm_geofabrik_bretagne",
    "osm_geofabrik_pays_de_la_loire",
    "osm_geofabrik_basse_normandie"
  )
}

#' The default source URLs of the three Geofabrik region PBFs.
#'
#' The `latest` millésime is pinned by sha256 at first acquisition (the
#' manifest is authoritative; these defaults only serve a fresh
#' register_source).
osm_geofabrik_source_urls <- function() {
  c(
    osm_geofabrik_bretagne =
      "https://download.geofabrik.de/europe/france/bretagne-latest.osm.pbf",
    osm_geofabrik_pays_de_la_loire =
      "https://download.geofabrik.de/europe/france/pays-de-la-loire-latest.osm.pbf",
    osm_geofabrik_basse_normandie =
      "https://download.geofabrik.de/europe/france/basse-normandie-latest.osm.pbf"
  )
}

#' The crop buffer distance: W + snap, in metres.
#'
#' ADR-0002's network rule: the Bretagne polygon buffered by W (the fastest
#' atomic mode's reach at the cap, 25 km) plus the 1.6 km r5r
#' point-to-street snap margin. Both are named once-run parameters — never
#' hard-coded beyond these defaults.
crop_buffer <- function(W = 25000, snap = 1600) {
  stopifnot(is.numeric(W), length(W) == 1L, !is.na(W), W > 0)
  stopifnot(is.numeric(snap), length(snap) == 1L, !is.na(snap), snap >= 0)
  W + snap
}

#' Locate one acquired Geofabrik PBF + its pinned sha256.
#'
#' Resolves the source entry from the acquisition manifest (acquire.R) and
#' returns the cached download (data/downloads/<id>) with its pin. Errors
#' if the source is not registered or not yet acquired (no pin).
read_osm_pbf <- function(id, data_dir = "data",
                         manifest_path = file.path(data_dir, "manifest.json")) {
  m <- manifest_load(manifest_path)
  entry <- m$sources[[id]]
  if (is.null(entry)) {
    stop("osm source ", id, " not registered in the manifest; acquire it first",
         call. = FALSE)
  }
  if (is.null(entry$sha256) || is.na(entry$sha256) || !nzchar(entry$sha256)) {
    stop("osm source ", id, " has no pinned sha256; acquire it first",
         call. = FALSE)
  }
  pbf <- file.path(data_dir, "downloads", id)
  if (!file.exists(pbf)) {
    stop("cached osm pbf not found at ", pbf, call. = FALSE)
  }
  list(pbf = pbf, sha256 = as.character(entry$sha256))
}

#' The pinned sha256s of all three Geofabrik PBFs, in id order.
osm_source_shas <- function(data_dir = "data",
                            manifest_path = file.path(data_dir, "manifest.json")) {
  vapply(
    osm_geofabrik_source_ids(),
    function(id) read_osm_pbf(id, data_dir, manifest_path)$sha256,
    character(1L)
  )
}

#' A 12-hex cache key from a set of pinned sha256s.
#'
#' The key is the first 12 hex chars of the concatenated 6-char prefixes —
#' compact, and a re-acquisition of any source (new pin) changes the key
#' and silently rebuilds the cache.
osm_cache_key <- function(shas) {
  substr(paste(vapply(shas, function(s) substr(s, 1L, 6L), character(1L)),
               collapse = ""), 1L, 12L)
}

#' The cache path for a derived OSM artifact.
#'
#' data/acquired/osm/<name>_<key12>.<ext> — the key ties the derived
#' artifact to the exact pinned source shas, so a re-acquisition rebuilds
#' the cache.
osm_cache_path <- function(data_dir, name, key, ext = "osm.pbf") {
  dir.create(file.path(data_dir, "acquired", "osm"),
             recursive = TRUE, showWarnings = FALSE)
  file.path(data_dir, "acquired", "osm", sprintf("%s_%s.%s", name, key, ext))
}

#' Run one Osmosis 0.49.2 pipeline.
#'
#' Invokes the bundled osmosis.bat via system2 with an explicit heap
#' (JAVA_OPTS). `args` is the osmosis task chain, e.g.
#' c("--read-pbf", "file=x", "--write-pbf", "file=y"). Returns the osmosis
#' output invisibly; stops with the captured output on a non-zero exit.
#' Windows gotcha recorded live: cmd.exe splits an unquoted relative path on
#' `/`, so the launcher is normalised to an absolute backslash path and
#' quoted — never pass forward slashes to cmd /c.
osmosis_run <- function(args, data_dir = "data", java_heap = "4g") {
  bat_rel <- file.path(data_dir, "tools", "osmosis-0.49.2", "bin", "osmosis.bat")
  if (!file.exists(bat_rel)) {
    stop("osmosis launcher not found at ", bat_rel, call. = FALSE)
  }
  bat <- normalizePath(bat_rel, winslash = "\\", mustWork = TRUE)
  old <- Sys.getenv("JAVA_OPTS")
  on.exit(Sys.setenv(JAVA_OPTS = old), add = TRUE)
  Sys.setenv(JAVA_OPTS = paste0("-Xmx", java_heap))
  out <- system2("cmd", c("/c", shQuote(bat), args),
                 stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(out, "status"))) {
    stop("osmosis failed:\n", paste(out, collapse = "\n"), call. = FALSE)
  }
  invisible(out)
}

#' Merge the three Geofabrik PBFs into one ADR-0002 input.
#'
#' read-pbf x3 -> merge x2 -> write-pbf (deflate) into
#' data/acquired/osm/bretagne_plus_strip_merged_<key>.osm.pbf. On a cache
#' hit the existing file is returned without re-running osmosis. The merged
#' file is the union of Bretagne + the two neighbouring regions, from which
#' both crops are cut.
osm_merge_pbfs <- function(data_dir = "data",
                           manifest_path = file.path(data_dir, "manifest.json"),
                           use_cache = TRUE, java_heap = "4g") {
  shas <- osm_source_shas(data_dir, manifest_path)
  key <- osm_cache_key(shas)
  out <- osm_cache_path(data_dir, "bretagne_plus_strip_merged", key)
  if (use_cache && file.exists(out)) {
    message("cache hit for merged osm pbf: ", out)
    return(out)
  }
  args <- character(0)
  for (id in osm_geofabrik_source_ids()) {
    args <- c(args, "--read-pbf", paste0("file=", read_osm_pbf(id, data_dir, manifest_path)$pbf))
  }
  args <- c(args, "--merge", "--merge", "--write-pbf", paste0("file=", out))
  message("merging three Geofabrik PBFs into ", out, " ...")
  invisible(osmosis_run(args, data_dir, java_heap))
  out
}

#' Write an sf geometry as an Osmosis .poly file.
#'
#' The Osmosis polygon format (PolygonFileReader): a header name line, then
#' one section per polygon part — a numeric section header, "lon lat" pairs
#' (EPSG:4326), END — closed by a final END. Holes are not representable
#' (osmosis unions the sections); the crop buffers have none at this scale.
osm_write_poly <- function(x, path, name = "crop") {
  coords <- sf::st_coordinates(x)
  parts <- unique(coords[, "L1"])
  lines <- name
  for (p in parts) {
    lines <- c(lines, as.character(p))
    pt <- coords[coords[, "L1"] == p, , drop = FALSE]
    for (i in seq_len(nrow(pt))) {
      lines <- c(lines, sprintf("%.7f %.7f", pt[i, "X"], pt[i, "Y"]))
    }
    lines <- c(lines, "END")
  }
  lines <- c(lines, "END")
  writeLines(lines, path)
  invisible(path)
}

#' The crop polygon for a region: base polygon buffered by W + snap.
#'
#' `region` selects the base polygon:
#'   * "fougeres" — the Fougères Agglo EPCI polygon (SIREN 200072452, the
#'     toy region of ticket 04), read_epci_polygon(200072452, crs = 2154);
#'   * "bretagne" — the Bretagne region polygon (code_insee 53),
#'     read_bretagne_polygon(crs = 2154) — the once-run's network (ADR-0002).
#' The buffer is computed in EPSG:2154 (metres) and the result transformed
#' to `crs_out` (default 4326 — the frame osmosis needs). Returns the sf
#' object; the caller can write it with osm_write_poly.
osm_crop_polygon <- function(region = c("fougeres", "bretagne"),
                             W = 25000, snap = 1600, crs_out = 4326,
                             data_dir = "data",
                             manifest_path = file.path(data_dir, "manifest.json")) {
  region <- match.arg(region)
  base <- switch(region,
    fougeres = read_epci_polygon("200072452", crs = 2154, data_dir, manifest_path),
    bretagne = read_bretagne_polygon(crs = 2154, data_dir, manifest_path)
  )
  sf::st_transform(sf::st_buffer(base, crop_buffer(W, snap)), crs_out)
}

#' Crop the merged PBF to one region's W + snap polygon.
#'
#' Runs osmosis --bounding-polygon (completeWays=yes completeRelations=yes —
#' ways crossing the boundary stay whole, the r5r requirement) on the merged
#' PBF and caches the result under data/acquired/osm/, keyed by the merged
#' pins + the admin-express pin behind the crop polygon. Returns the crop
#' path.
osm_crop_network <- function(region = c("fougeres", "bretagne"),
                             W = 25000, snap = 1600,
                             data_dir = "data",
                             manifest_path = file.path(data_dir, "manifest.json"),
                             use_cache = TRUE, java_heap = "4g") {
  region <- match.arg(region)
  buf_km <- crop_buffer(W, snap) / 1000
  merged <- osm_merge_pbfs(data_dir, manifest_path, use_cache, java_heap)
  admin_src <- admin_express_gpkg(data_dir, manifest_path)$sha256
  key <- osm_cache_key(c(osm_source_shas(data_dir, manifest_path), admin_src))
  out <- osm_cache_path(
    data_dir, sprintf("%s_crop_%.1fkm", region, buf_km), key
  )
  if (use_cache && file.exists(out)) {
    message("cache hit for osm crop: ", out)
    return(out)
  }
  poly <- osm_crop_polygon(region, W, snap, 4326, data_dir, manifest_path)
  poly_path <- osm_cache_path(data_dir, sprintf("%s_crop_%.1fkm", region, buf_km),
                              key, ext = "poly")
  osm_write_poly(sf::st_geometry(poly), poly_path)
  args <- c(
    "--read-pbf", paste0("file=", merged),
    "--bounding-polygon", paste0("file=", poly_path),
    "completeWays=yes", "completeRelations=yes",
    "--write-pbf", paste0("file=", out)
  )
  message("cropping merged osm pbf to ", region, " + ", sprintf("%.1f", buf_km),
          " km -> ", out, " ...")
  invisible(osmosis_run(args, data_dir, java_heap))
  out
}

#' The once-run's routing network PBF (ADR-0002): Bretagne + W + snap.
#'
#' The network the matrix computes travel on — the Bretagne region polygon
#' buffered by 26.6 km, cropped from the merged Bretagne + strip-extract
#' PBF. This is the S5/S6-style reader entry point: it returns the cached
#' artifact path and rebuilds it (merging + cropping via osmosis) only when
#' the cache is cold. The toy-region crop for the tracer is
#' osm_crop_network("fougeres", ...).
read_osm_network <- function(W = 25000, snap = 1600,
                             data_dir = "data",
                             manifest_path = file.path(data_dir, "manifest.json"),
                             use_cache = TRUE, java_heap = "4g") {
  osm_crop_network("bretagne", W, snap, data_dir, manifest_path,
                   use_cache, java_heap)
}
