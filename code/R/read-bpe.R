# S7 — the BPE 2025 reader: the full-France Base Permanente des Équipements,
# classified border-aware per ADR-0002 (never a bare Bretagne filter — the
# acquisition rule "on ne filtre jamais le BPE à la Bretagne"). Reads the
# pinned BPE 2025 fichier détail (acquire.R manifest, id "bpe-2025"),
# classifies dép-first then distance, applies the legacy geoloc-quality
# filter, and verifies the type axis. Derived artifacts are cached under
# data/acquired/bpe/, keyed by the pinned source sha256 — a re-acquisition of
# the source rebuilds the cache.
#
# Reading discipline: this file's data.tables are extracted with `[[` or
# `j`, NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' The manifest id of the BPE 2025 source.
bpe_source_id <- function() "bpe-2025"

#' The department codes of Bretagne (authoritative — dép-first classification).
bpe_bretagne_deps <- function() c("22", "29", "35", "56")

#' The border-département codes whose BPE join the universe only when within
#' W spherical-s2 metres of the Bretagne polygon (ADR-0002 strip rule).
bpe_strip_deps <- function() c("44", "53", "49", "50", "61")

#' The legacy geoloc-quality keep-set (ticket 05): establishments whose
#' QUALITE_GEOLOC is one of {11, 12, 21, 22, _U}, or whose LIBVOIE (address)
#' is non-empty. Its re-review is #198's derivation-layer exercise, not this
#' ticket's.
bpe_geoloc_keep_codes <- function() c("11", "12", "21", "22", "_U")

#' Locate the extracted BPE CSV + its pinned sha256.
#'
#' Resolves the source entry from the acquisition manifest (acquire.R) and
#' finds the single .csv under data/acquired/<id>/. Errors if the source is
#' not registered or not yet acquired (no pin).
bpe_csv <- function(data_dir, manifest_path) {
  m <- manifest_load(manifest_path)
  entry <- m$sources[[bpe_source_id()]]
  if (is.null(entry)) {
    stop("bpe-2025 source not registered in the manifest; acquire it first",
         call. = FALSE)
  }
  if (is.null(entry$sha256) || is.na(entry$sha256) || !nzchar(entry$sha256)) {
    stop("bpe-2025 source has no pinned sha256; acquire it first",
         call. = FALSE)
  }
  csvs <- list.files(
    file.path(data_dir, "acquired", bpe_source_id()),
    pattern = "\\.csv$", recursive = TRUE, full.names = TRUE
  )
  if (length(csvs) != 1L) {
    stop(sprintf(
      "expected exactly one .csv under data/acquired/%s but found %d",
      bpe_source_id(), length(csvs)
    ), call. = FALSE)
  }
  list(csv = csvs[[1L]], sha256 = as.character(entry$sha256))
}

#' The cache path for a derived BPE artifact.
#'
#' data/acquired/bpe/<name>_<sha12>.rds — the sha256 prefix ties the derived
#' artifact to the exact pinned source, so a re-acquisition (new pin) silently
#' rebuilds the cache.
bpe_cache_path <- function(data_dir, name, sha256, schema = bpe_schema_key()) {
  dir.create(file.path(data_dir, "acquired", "bpe"),
             recursive = TRUE, showWarnings = FALSE)
  file.path(
    data_dir, "acquired", "bpe",
    sprintf("%s_%s_%s.rds", name, substr(sha256, 1L, 12L), schema)
  )
}

#' The columns the BPE reader needs from the 95-column fichier détail.
#'
#' Coordinates: LONGITUDE/LATITUDE (WGS84, the s2 strip rule's frame) plus
#' LAMBERT_X/Y (EPSG 5490, kept for reference). All text columns are forced
#' character via colClasses so leading zeros and NA sentinels (_Z) survive.
bpe_cols <- c("AN", "DEP", "DEPCOM", "LIBCOM", "TYPEQU", "SIRET", "NOMRS",
              "STATUT_DIFFUSION", "LIBVOIE", "QUALITE_GEOLOC", "QUALITE_XY",
              "LONGITUDE", "LATITUDE", "LAMBERT_X", "LAMBERT_Y", "EPSG", "EPCI")

bpe_col_classes <- c(
  DEP = "character", DEPCOM = "character", LIBCOM = "character",
  TYPEQU = "character", SIRET = "character", NOMRS = "character",
  STATUT_DIFFUSION = "character",
  LIBVOIE = "character", QUALITE_GEOLOC = "character", QUALITE_XY = "character",
  EPCI = "character"
)

# Derived caches must change when the selected raw schema changes.
bpe_schema_key <- function() {
  substr(digest::digest(bpe_cols, algo = "sha256"), 1L, 12L)
}

#' Read the pinned BPE 2025 fichier détail into a data.table.
#'
#' Raw reader — no classification, no filter. The universe builder
#' (read_bpe_universe) applies the ADR-0002 zone rule and the legacy
#' geoloc-quality filter on top.
read_bpe_raw <- function(data_dir = "data",
                         manifest_path = file.path(data_dir, "manifest.json")) {
  src <- bpe_csv(data_dir, manifest_path)
  data.table::fread(
    src$csv,
    select = bpe_cols,
    colClasses = bpe_col_classes
  )
}

#' The border-aware BPE universe for the once-run.
#'
#' Dép-first, then distance (ADR-0002 / ticket 05):
#' \itemize{
#'   \item \code{bretagne} — DEP ∈ {22, 29, 35, 56} (authoritative, no geometry
#'     test — kills the ~1.4k coastal-precision artifacts and the dép-94
#'     coordinate outlier);
#'   \item \code{zone_frontaliere} — DEP ∈ {44, 53, 49, 50, 61} whose spherical
#'     s2 distance to the Bretagne polygon is < \code{W} metres;
#'   \item everything else is dropped.
#' }
#' The legacy geoloc-quality filter is then applied (QUALITE_GEOLOC ∈
#' {11, 12, 21, 22, _U} or LIBVOIE non-empty). Anonymised rows (INSEE monthly
#' anonymisation: STATUT_DIFFUSION = P, no coordinates, no LIBVOIE) are kept
#' when they are classifiable — they count on the type axis and are flagged by
#' NA coordinates; the once-run's routing excludes them, never the universe.
#' A strip-dép anonymised row (no coordinates) cannot be distance-tested and
#' therefore cannot enter the strip — by construction, not by filter choice.
#'
#' `W` is a named parameter: the fastest atomic mode's reach at the cap
#' (25 km today), re-derived if the cap or speeds change — never hard-coded.
#'
#' Returns a data.table with a `zone` column ("bretagne" | "zone_frontaliere"),
#' cached as bpe_universe_<Wkm>_<sha12>.rds.
read_bpe_universe <- function(W = 25000, data_dir = "data",
                              manifest_path = file.path(data_dir, "manifest.json"),
                              use_cache = TRUE) {
  stopifnot(is.numeric(W), length(W) == 1L, !is.na(W), W > 0)
  src <- bpe_csv(data_dir, manifest_path)
  cache <- bpe_cache_path(
    data_dir, sprintf("bpe_universe_%gkm", W / 1000), src$sha256
  )
  if (use_cache && file.exists(cache)) {
    return(readRDS(cache))
  }

  raw <- read_bpe_raw(data_dir, manifest_path)
  dep <- raw[["DEP"]]

  # Dép-first classification.
  is_bretagne <- dep %in% bpe_bretagne_deps()
  strip_candidate <- dep %in% bpe_strip_deps() &
    !is.na(raw[["LONGITUDE"]]) & !is.na(raw[["LATITUDE"]])
  in_strip <- rep(FALSE, nrow(raw))
  if (any(strip_candidate)) {
    cand <- which(strip_candidate)
    pts <- sf::st_as_sf(
      as.data.frame(raw)[cand, ],
      coords = c("LONGITUDE", "LATITUDE"), crs = 4326
    )
    br <- read_bretagne_polygon(crs = 4326, data_dir, manifest_path)
    d <- as.numeric(sf::st_distance(pts, br))
    in_strip[cand] <- d < W
  }
  zone <- rep(NA_character_, nrow(raw))
  zone[is_bretagne] <- "bretagne"
  zone[in_strip] <- "zone_frontaliere"

  # Legacy geoloc-quality filter.
  geo_ok <- raw[["QUALITE_GEOLOC"]] %in% bpe_geoloc_keep_codes()
  voie_ok <- !is.na(raw[["LIBVOIE"]]) & nzchar(raw[["LIBVOIE"]])
  keep <- (geo_ok | voie_ok) & !is.na(zone)

  out <- raw[keep, ]
  out[, zone := zone[keep]]
  data.table::setorder(out, DEP, DEPCOM, TYPEQU)

  # Type-axis verification (ADR-0002's measurement, re-verified on BPE 2025):
  # the union must carry no type absent from Bretagne.
  types_bretagne <- unique(out[zone == "bretagne", TYPEQU])
  types_union <- unique(out[, TYPEQU])
  strip_only <- setdiff(types_union, types_bretagne)
  if (length(strip_only) > 0L) {
    warning(sprintf(
      "type axis widened by the strip: %d type(s) absent from Bretagne (%s)",
      length(strip_only), paste(strip_only, collapse = ", ")
    ), call. = FALSE)
  } else {
    message(sprintf(
      "type axis verified: Bretagne %d types, union %d, strip-only %d",
      length(types_bretagne), length(types_union), length(strip_only)
    ))
  }

  saveRDS(out, cache)
  out
}
