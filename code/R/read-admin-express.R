# S6 — the ADMIN EXPRESS COG reader: official administrative boundaries
# (region / EPCI / commune) consumed by the acquisition readers. Reads the
# pinned ADMIN-EXPRESS-COG GPKG (acquire.R manifest) — edition 2025-01-01,
# aligned with BDNB 2026-02.a (changelog: INSEE COG millésime 2025) and BPE
# 2025 (géographie au 1er janvier 2025). Native CRS is Lambert-93
# (EPSG:2154); callers transform where their contract requires (ADR-0002's
# strip rule is spherical s2 → WGS84). Derived artifacts are cached under
# data/acquired/admin-express/, keyed by the pinned source sha256 — a
# re-acquisition of the source rebuilds the cache.

#' The manifest id of the ADMIN EXPRESS COG source.
admin_express_source_id <- function() "admin-express-cog-2025-01-01"

#' Locate the extracted ADMIN EXPRESS GPKG + its pinned sha256.
#'
#' Resolves the source entry from the acquisition manifest (acquire.R) and
#' finds the single .gpkg under data/acquired/<id>/. Errors if the source is
#' not registered or not yet acquired (no pin).
admin_express_gpkg <- function(data_dir, manifest_path) {
  m <- manifest_load(manifest_path)
  entry <- m$sources[[admin_express_source_id()]]
  if (is.null(entry)) {
    stop("admin-express source not registered in the manifest; acquire it first",
         call. = FALSE)
  }
  if (is.null(entry$sha256) || is.na(entry$sha256) || !nzchar(entry$sha256)) {
    stop("admin-express source has no pinned sha256; acquire it first",
         call. = FALSE)
  }
  gpkgs <- list.files(
    file.path(data_dir, "acquired", admin_express_source_id()),
    pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE
  )
  if (length(gpkgs) != 1L) {
    stop(sprintf(
      "expected exactly one .gpkg under data/acquired/%s but found %d",
      admin_express_source_id(), length(gpkgs)
    ), call. = FALSE)
  }
  list(gpkg = gpkgs[[1L]], sha256 = as.character(entry$sha256))
}

#' The cache path for a derived ADMIN EXPRESS artifact.
#'
#' data/acquired/admin-express/<name>_<crs>_<sha12>.rds — the sha256 prefix
#' ties the derived artifact to the exact pinned source, so a re-acquisition
#' (new pin) silently rebuilds the cache.
admin_express_cache_path <- function(data_dir, name, crs, sha256) {
  dir.create(file.path(data_dir, "acquired", "admin-express"),
             recursive = TRUE, showWarnings = FALSE)
  file.path(
    data_dir, "acquired", "admin-express",
    sprintf("%s_%s_%s.rds", name, crs, substr(sha256, 1L, 12L))
  )
}

#' Read one ADMIN EXPRESS GPKG layer.
read_admin_express_layer <- function(layer, data_dir = "data",
                                     manifest_path = file.path(data_dir, "manifest.json")) {
  gpkg <- admin_express_gpkg(data_dir, manifest_path)$gpkg
  sf::st_read(gpkg, layer = layer, quiet = TRUE)
}

#' The Bretagne region polygon.
#'
#' INSEE code 53, ADMIN EXPRESS `region` layer. `crs` default 4326 (WGS84 —
#' the ADR-0002 strip rule measures spherical s2 metres); pass 2154 for the
#' Lambert-93-native geometry (e.g. OSM crop planning).
read_bretagne_polygon <- function(crs = 4326, data_dir = "data",
                                  manifest_path = file.path(data_dir, "manifest.json"),
                                  use_cache = TRUE) {
  src <- admin_express_gpkg(data_dir, manifest_path)
  cache <- admin_express_cache_path(data_dir, "bretagne_polygon", crs, src$sha256)
  if (use_cache && file.exists(cache)) {
    return(readRDS(cache))
  }
  reg <- read_admin_express_layer("region", data_dir, manifest_path)
  br <- reg[reg$code_insee == "53", ]
  if (nrow(br) != 1L) {
    stop("expected exactly one Bretagne region feature (code_insee 53), got ",
         nrow(br), call. = FALSE)
  }
  br <- sf::st_transform(br, crs)
  saveRDS(br, cache)
  br
}

#' The polygon of one EPCI.
#'
#' Matched by SIREN code on the ADMIN EXPRESS `epci` layer. `crs` default
#' 2154 (native Lambert-93 — the OSM crop buffers in metres); pass 4326 for
#' WGS84.
read_epci_polygon <- function(siren, crs = 2154, data_dir = "data",
                              manifest_path = file.path(data_dir, "manifest.json"),
                              use_cache = TRUE) {
  src <- admin_express_gpkg(data_dir, manifest_path)
  cache <- admin_express_cache_path(data_dir, paste0("epci_", siren), crs, src$sha256)
  if (use_cache && file.exists(cache)) {
    return(readRDS(cache))
  }
  epci <- read_admin_express_layer("epci", data_dir, manifest_path)
  e <- epci[epci$code_siren == siren, ]
  if (nrow(e) != 1L) {
    stop("expected exactly one EPCI feature for siren ", siren, ", got ",
         nrow(e), call. = FALSE)
  }
  e <- sf::st_transform(e, crs)
  saveRDS(e, cache)
  e
}

#' The communes of one EPCI.
#'
#' Matched via `codes_siren_des_epci` on the ADMIN EXPRESS `commune` layer
#' (NA-safe: communes attached to no EPCI are excluded). Returns a data.table
#' of INSEE codes + official names, ordered by code — the BDNB reader's
#' territorial filter.
read_epci_communes <- function(siren, data_dir = "data",
                               manifest_path = file.path(data_dir, "manifest.json"),
                               use_cache = TRUE) {
  src <- admin_express_gpkg(data_dir, manifest_path)
  cache <- admin_express_cache_path(data_dir, paste0("communes_", siren), "none", src$sha256)
  if (use_cache && file.exists(cache)) {
    return(readRDS(cache))
  }
  comm <- read_admin_express_layer("commune", data_dir, manifest_path)
  codes <- comm[["codes_siren_des_epci"]]
  idx <- vapply(codes, function(x) isTRUE(grepl(siren, x, fixed = TRUE)), logical(1L))
  out <- data.table::data.table(
    code_insee  = comm[["code_insee"]][idx],
    nom_commune = comm[["nom_officiel"]][idx]
  )
  data.table::setorder(out, code_insee)
  saveRDS(out, cache)
  out
}
