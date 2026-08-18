# S8 — the BDNB résidentiel reader: the once-run's origins (residential
# buildings), resolved per the 2026-08-10 reader contract (ticket 05,
# research note research/05-bdnb.md). Reads the pinned BDNB 2026-02.a
# department CSV exports of the four Bretagne départements 22/29/35/56
# (acquire.R manifest, ids "bdnb-2026-02-a-dep{22,29,35,56}"; dep 35
# acquired 2026-08-12, 22/29/56 on 2026-08-12 follow-up), filters
# residential usage at the GROUP level — usage lives only there — and
# resolves origins to physical buildings via batiment_construction, with
# the geometry hierarchy: BAN address point (primary) → construction centroid
# (secondary) → group centroid → fictive centroid. Derived
# artifacts are cached under data/acquired/bdnb/, keyed by the sha256 of
# the SORTED vector of the four per-dep source pins plus the params digest
# — a re-acquisition of ANY department rebuilds the cache.
#
# Acquired-model facts recorded at first acquisition (2026-08-12, dép 35):
#   * batiment.csv (fiscal buildings, DGFIP) does NOT ship in the open-data
#     export — the historical restriction stands, so the "actual buildings"
#     resolution is done via batiment_construction (physical footprints,
#     BDTopo-based). See the granularity decision in
#     read_bdnb_residential_universe.
#   * Geometry columns ship as plain WKT (the column is literally named
#     "WKT"), EPSG:2154. batiment_construction carries only the polygon
#     geom_cstr; the point geom_cstr_pos is NOT in the CSV export — the
#     building-level origin point is the centroid of geom_cstr.
#   * batiment_groupe_synthese_propriete_usage ships reduced to 3 columns
#     (batiment_groupe_id, code_departement_insee, usage_principal_bdnb_open).
#     The mixite_usage_residentiel_tertiaire flag (and usage_niveau_2/3,
#     categorie_usage_propriete) is "ayant droit" restricted — NO mixite
#     policy can be honored at this acquisition level (flagged for the
#     maintainer; revisit if ayant-droit access is granted).
#   * rel_batiment_groupe_usage (detailed usage) does not ship either.
#   * rel_batiment_groupe_adresse.WKT is the address↔group connecting
#     MULTILINESTRING (geom_bat_adresse), not a point — the address-fallback
#     geometry comes from the adresse table (geom_adresse, BAN housenumber).
#   * Dépendance signal: rel_batiment_construction_adresse ships in open
#     data. A construction in a multi-construction residential group with no
#     construction-level address while a sibling construction has one is a
#     dépendance candidate (garage/shed — the SP-1.m « maison + dépendance
#     non incorporée » case): measured on dép 35, such constructions are
#     median 16 m² / 86.5% < 40 m² vs 74 m² for their addressed siblings.
#     The universe keeps them, flagged is_dependance_candidate (ADR-0003);
#     the cut is a derivation-layer decision, never baked into the run.
#
# Reading discipline: this file's data.tables are extracted with `[[` or
# `j`, NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' The manifest ids of the four BDNB 2026-02.a département sources
#' (Bretagne: 22/29/35/56).
bdnb_source_ids <- function() {
  sprintf("bdnb-2026-02-a-dep%s", c("22", "29", "35", "56"))
}

#' The residential usage set of the once-run.
#'
#' usage_principal_bdnb_open ∈ {Résidentiel individuel, Résidentiel
#' collectif} (the 2026-08-10 contract). 'Résidentiel indifférencié' is a
#' named policy (include_indifferencie in the universe builder); observed
#' count is 0 on all four deps (verified live 2026-08-12 after the 22/29/56
#' acquisition), so the default exclusion never bites in practice.
bdnb_residential_usages <- function() {
  c("Résidentiel individuel", "Résidentiel collectif")
}

# Classify the canonical usage coverage before filtering it.  In particular,
# an absent row is not a non-residential assertion: the open-data usage table
# is a partial export.  Keeping this seam pure also makes it impossible for a
# deprecated BDTopo usage column to become an implicit fallback.
bdnb_usage_audit <- function(group_ids, usage, residential_usages =
                              bdnb_residential_usages()) {
  if (!all(c("batiment_groupe_id", "usage_principal_bdnb_open") %in%
           names(usage))) {
    stop("canonical BDNB usage table must contain batiment_groupe_id and usage_principal_bdnb_open",
         call. = FALSE)
  }
  usage <- data.table::as.data.table(usage)
  if (anyDuplicated(usage[["batiment_groupe_id"]])) {
    stop("canonical BDNB usage table has duplicate batiment_groupe_id rows",
         call. = FALSE)
  }
  ids <- data.table::data.table(batiment_groupe_id = as.character(group_ids))
  ids <- usage[ids, on = "batiment_groupe_id"]
  ids[, usage_status := data.table::fifelse(
    is.na(usage_principal_bdnb_open), "unknown_coverage",
    data.table::fifelse(usage_principal_bdnb_open %in% residential_usages,
                        "residential", "non_residential"))]
  counts <- ids[, .(n_groups = .N), by = usage_status]
  list(
    groups = ids,
    n_groups = nrow(ids),
    n_canonical_rows = sum(ids[["usage_status"]] != "unknown_coverage"),
    n_unknown_coverage = sum(ids[["usage_status"]] == "unknown_coverage"),
    pct_canonical_coverage = if (nrow(ids)) {
      round(100 * sum(ids[["usage_status"]] != "unknown_coverage") / nrow(ids), 2)
    } else 0,
    counts = counts,
    source_field = "usage_principal_bdnb_open",
    residential_usages = residential_usages
  )
}

#' Locate the extracted BDNB CSV directories + the combined source pin.
#'
#' Resolves the four source entries from the acquisition manifest
#' (acquire.R) and verifies each extraction (csv/ under
#' data/acquired/<id>/) exists. Errors listing which of the four are
#' missing (not registered, not yet acquired — no pin — or not yet
#' extracted). Returns:
#' \itemize{
#'   \item deps — a named list (source id -> list(dir, sha256)) of the four
#'     extracted directories with their per-dep pins;
#'   \item sha256 — the sha256 of the SORTED vector of the four per-dep
#'     pins, the cache key: a re-acquisition of any department changes the
#'     combined pin and rebuilds every derived artifact;
#'   \item pins — the named per-dep pins (dep -> sha256), for lineage.
#' }
bdnb_extracted_dir <- function(data_dir, manifest_path) {
  m <- manifest_load(manifest_path)
  ids <- bdnb_source_ids()
  missing <- character(0)
  deps <- list()
  pins <- character(0)
  for (id in ids) {
    entry <- m$sources[[id]]
    if (is.null(entry)) {
      missing <- c(missing, id)
      next
    }
    # The unpinned marker: a freshly registered source round-trips through
    # JSON as an empty named list (length 0), NOT NULL — the length != 1L
    # guard (same idiom as acquire.R's pin_of) catches both.
    p <- entry$sha256
    if (is.null(p) || length(p) != 1L || is.na(p) || !nzchar(p)) {
      missing <- c(missing, id)
      next
    }
    dir <- file.path(data_dir, "acquired", id)
    if (!dir.exists(file.path(dir, "csv"))) {
      missing <- c(missing, id)
      next
    }
    deps[[id]] <- list(dir = dir, sha256 = as.character(entry$sha256))
    pins[[id]] <- as.character(entry$sha256)
  }
  if (length(missing) > 0L) {
    stop(sprintf(
      "BDNB sources not registered, unpinned, or unextracted: %s — acquire + unzip them first",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  list(deps = deps, sha256 = digest::digest(sort(pins), algo = "sha256"),
       pins = pins)
}

#' The cache path for a derived BDNB artifact.
#'
#' data/acquired/bdnb/<name>_<sha12>.rds — the sha256 prefix is the sha256
#' of the SORTED vector of the four per-dep source pins (the combined pin,
#' see bdnb_extracted_dir), so a re-acquisition of any department silently
#' rebuilds the cache.
bdnb_cache_path <- function(data_dir, name, sha256) {
  dir.create(file.path(data_dir, "acquired", "bdnb"),
             recursive = TRUE, showWarnings = FALSE)
  file.path(
    data_dir, "acquired", "bdnb",
    sprintf("%s_%s.rds", name, substr(sha256, 1L, 12L))
  )
}

#' Read one extracted BDNB table (raw CSV reader), across the four deps.
#'
#' Reads csv/<table>.csv from each of the four extractions with
#' data.table::fread and row-binds them. Column sets are verified identical
#' across the four deps at acquisition (2026-08-12: all tables the reader
#' touches match); rbindlist(use.names = TRUE, fill = FALSE) errors rather
#' than silently misaligning should a future millésime drift. ID/code
#' columns are forced character so leading zeros (code_commune_insee,
#' code_departement_insee, cle_interop_adr) survive; the WKT geometry
#' column stays character.
bdnb_read_table <- function(table, cols = NULL, data_dir = "data",
                            manifest_path = file.path(data_dir, "manifest.json")) {
  src <- bdnb_extracted_dir(data_dir, manifest_path)
  text_cols <- c("batiment_groupe_id", "batiment_construction_id",
                 "code_departement_insee", "code_commune_insee",
                 "code_iris", "code_epci_insee", "libelle_commune_insee",
                 "cle_interop_adr", "origine", "WKT", "geom_groupe")
  if (is.null(cols)) {
    hdr <- names(data.table::fread(
      file.path(src$deps[[1L]]$dir, "csv", paste0(table, ".csv")), nrows = 0
    ))
    text_cols <- intersect(text_cols, hdr)
  } else {
    text_cols <- intersect(text_cols, cols)
  }
  col_classes <- if (length(text_cols) > 0L) {
    c(setNames(rep("character", length(text_cols)), text_cols))
  } else {
    NULL
  }
  parts <- lapply(src$deps, function(d) {
    f <- file.path(d$dir, "csv", paste0(table, ".csv"))
    if (!file.exists(f)) {
      stop(sprintf("table %s not found in the BDNB extraction (%s)", table, f),
           call. = FALSE)
    }
    if (!is.null(col_classes)) {
      data.table::fread(f, select = cols, colClasses = col_classes)
    } else {
      data.table::fread(f, select = cols)
    }
  })
  data.table::rbindlist(parts, use.names = TRUE, fill = FALSE)
}

#' Parse a character vector of plain WKT geometries into sfc (EPSG:2154).
#'
#' The BDNB CSV exports ship geometry as WKT strings (column "WKT"); the
#' department exports are in Lambert-93 (EPSG:2154, see the .prj sidecar).
bdnb_wkt_sfc <- function(wkt, crs = 2154L) {
  if (length(wkt) == 0L) {
    return(sf::st_sfc(crs = crs))
  }
  sf::st_as_sfc(wkt, crs = crs)
}

#' Centroid coordinates (x/y) of an sfc, NA where the geometry is empty.
bdnb_centroid_xy <- function(sfc) {
  cent <- sf::st_centroid(sfc)
  xy <- as.data.frame(sf::st_coordinates(cent))
  names(xy) <- c("x", "y")
  empty <- sf::st_is_empty(cent)
  xy$x[empty] <- NA_real_
  xy$y[empty] <- NA_real_
  xy
}

#' Resolve one origin's point using the once-run geometry policy.
#'
#' This deliberately has no BDNB or sf dependency: the reader supplies the
#' candidate coordinates.  Keeping precedence here makes the BAN-primary
#' contract executable on small fixtures as well as on the full acquisition.
#' Candidate columns are x_2154/y_2154 (construction), gx_2154/gy_2154
#' (group), and ax_2154/ay_2154 (BAN address).
bdnb_resolve_geometry <- function(orig, min_fiabilite = NULL) {
  orig <- data.table::copy(data.table::as.data.table(orig))
  required <- c("x_2154", "y_2154", "gx_2154", "gy_2154",
                "ax_2154", "ay_2154", "fictive_geom_cstr",
                "contient_fictive_geom_groupe", "fiabilite")
  missing <- setdiff(required, names(orig))
  if (length(missing)) {
    stop("geometry candidates missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  n <- nrow(orig)
  source <- rep(NA_character_, n)
  pair_ok <- function(x, y) !is.na(x) & !is.na(y)
  address_ok <- pair_ok(orig[["ax_2154"]], orig[["ay_2154"]]) &
    !is.na(orig[["fiabilite"]])
  if (!is.null(min_fiabilite)) address_ok <- address_ok &
    orig[["fiabilite"]] >= min_fiabilite
  orig[["x_2154"]][address_ok] <- orig[["ax_2154"]][address_ok]
  orig[["y_2154"]][address_ok] <- orig[["ay_2154"]][address_ok]
  source[address_ok] <- "geom_adresse"

  need <- is.na(source)
  cstr_ok <- need & pair_ok(orig[["x_2154"]], orig[["y_2154"]]) &
    !is.na(orig[["fictive_geom_cstr"]]) & orig[["fictive_geom_cstr"]] == 0L
  source[cstr_ok] <- "geom_cstr"
  need <- is.na(source)
  group_ok <- need & pair_ok(orig[["gx_2154"]], orig[["gy_2154"]]) &
    !is.na(orig[["contient_fictive_geom_groupe"]]) &
    orig[["contient_fictive_geom_groupe"]] == 0L
  orig[["x_2154"]][group_ok] <- orig[["gx_2154"]][group_ok]
  orig[["y_2154"]][group_ok] <- orig[["gy_2154"]][group_ok]
  source[group_ok] <- "geom_groupe"

  # Fictive candidates are intentionally retained here; the caller applies
  # drop_fictive_only after unresolved rows have been identified.
  need <- is.na(source)
  fict_cstr <- need & pair_ok(orig[["x_2154"]], orig[["y_2154"]]) &
    !is.na(orig[["fictive_geom_cstr"]]) & orig[["fictive_geom_cstr"]] == 1L
  source[fict_cstr] <- "fictive"
  need <- is.na(source)
  fict_group <- need & pair_ok(orig[["gx_2154"]], orig[["gy_2154"]]) &
    !is.na(orig[["contient_fictive_geom_groupe"]]) &
    orig[["contient_fictive_geom_groupe"]] == 1L
  orig[["x_2154"]][fict_group] <- orig[["gx_2154"]][fict_group]
  orig[["y_2154"]][fict_group] <- orig[["gy_2154"]][fict_group]
  source[fict_group] <- "fictive"
  orig[["geometry_source"]] <- source
  orig[["geometry_resolved"]] <- !is.na(source)
  orig
}

bdnb_geometry_policy <- function() {
  list(name = "ban_address_primary_v1",
       precedence = c("geom_adresse", "geom_cstr", "geom_groupe", "fictive"),
       address = "highest_fiabilite_relation_subject_to_min_fiabilite",
       unresolved = "excluded",
       drop_fictive_only = "caller_parameter")
}

#' The residential groups of the scope, at GROUP level.
#'
#' Usage lives only at the group level (the socle): residential =
#' batiment_groupe_synthese_propriete_usage.usage_principal_bdnb_open ∈
#' bdnb_residential_usages(). Returns a data.table of the filtered groups
#' with their usage, commune, and the group geometry columns (geom_groupe
#' WKT + contient_fictive_geom_groupe) — the shared input of the universe
#' builder and the address-join metrics.
bdnb_residential_groups <- function(departements = c("22", "29", "35", "56"),
                                    communes = NULL,
                                    include_indifferencie = FALSE,
                                    data_dir = "data",
                                    manifest_path = file.path(data_dir, "manifest.json")) {
  grp <- bdnb_read_table(
    "batiment_groupe",
    cols = c("batiment_groupe_id", "code_departement_insee",
             "code_commune_insee", "geom_groupe",
             "contient_fictive_geom_groupe"),
    data_dir, manifest_path
  )
  use <- bdnb_read_table(
    "batiment_groupe_synthese_propriete_usage",
    cols = c("batiment_groupe_id", "usage_principal_bdnb_open"),
    data_dir, manifest_path
  )
  # Scope the groups first: the audit must describe coverage for the actual
  # once-run scope, including groups whose canonical row is absent.
  grp <- grp[grp[["code_departement_insee"]] %in% departements]
  if (!is.null(communes)) {
    grp <- grp[grp[["code_commune_insee"]] %in% communes]
  }
  usage_audit <- bdnb_usage_audit(grp[["batiment_groupe_id"]], use,
                                  residential_usages = c(
                                    bdnb_residential_usages(),
                                    if (isTRUE(include_indifferencie))
                                      "Résidentiel indifférencié"
                                  ))
  residential <- usage_audit$groups[
    usage_status == "residential",
    .(batiment_groupe_id, usage_principal_bdnb_open)
  ]
  out <- grp[residential, on = "batiment_groupe_id"]
  data.table::setorder(out, code_commune_insee, batiment_groupe_id)
  attr(out, "bdnb_usage_audit") <- usage_audit[c("n_groups",
    "n_canonical_rows", "n_unknown_coverage", "pct_canonical_coverage",
    "counts", "source_field", "residential_usages")]
  out
}

#' The residential universe of the once-run (the origins).
#'
#' Granularity (the 2026-08-10 group-check, decided on real data):
#' batiment.csv does NOT ship in the open-data export, so the fiscal
#' building level is unavailable and the "actual buildings" resolution is
#' done at the physical-footprint level: granularity =
#' "batiment_construction" produces one origin per construction of every
#' residential group, plus one group-level origin for residential groups
#' with no construction at all. granularity = "groupe" produces one origin
#' per residential group, located at its best real point.
#'
#' Geometry hierarchy per origin (all EPSG:2154, returned as numeric
#' x_2154/y_2154):
#' \enumerate{
#'   \item the group's best address point (geom_adresse, BAN housenumber —
#'     the relation with the highest fiabilite), gated by min_fiabilite
#'     when set (default NULL = no gate);
#'   \item centroid of a non-fictive construction polygon (geom_cstr);
#'   \item centroid of a non-fictive group polygon (geom_groupe,
#'     contient_fictive_geom_groupe = false);
#'   \item a fictive centroid (construction or group) as the last resort —
#'     valid for statistics, not for point-precision routing: dropped when
#'     drop_fictive_only = TRUE (the default).
#' }
#' Each origin carries n_adresses (relations of its group) and
#' fiabilite_max (NA where the group has no address relation) so
#' downstream passes can re-gate on address quality, plus
#' is_dependance_candidate (TRUE for constructions in multi-construction
#' groups with no construction-level address while a sibling has one —
#' the shed/garage class, ADR-0003; kept in the universe, cut downstream).
#'
#' Policies decided on dép 35, applied to the four-department Bretagne
#' (flagged for the maintainer):
#'   * 'Résidentiel indifférencié' — excluded by default (include_indifferencie
#'     = FALSE); 0 rows observed on dép 35, and the Bretagne-wide count is
#'     reported live per acquisition (22/29/56 first read 2026-08-12).
#'   * mixite_usage_residentiel_tertiaire — cannot be honored: the flag is
#'     "ayant droit" restricted and does not ship in the open-data CSV
#'     (synthese_propriete_usage ships 3 columns).
#'
#' Returns a data.table with one row per origin, cached as
#' residential_universe_<scope>_<params8>_<sha12>.rds — the sha prefix is
#' the sha256 of the SORTED vector of the four per-dep source pins, so a
#' re-acquisition of ANY department rebuilds the cache. The rds carries
#' lineage attributes (millésime, source URLs per dep, pins, crs).
read_bdnb_residential_universe <- function(
    departements = c("22", "29", "35", "56"), communes = NULL,
    granularity = c("batiment_construction", "groupe"),
    include_indifferencie = FALSE, drop_fictive_only = TRUE,
    min_fiabilite = NULL,
    data_dir = "data",
    manifest_path = file.path(data_dir, "manifest.json"),
    use_cache = TRUE) {
  granularity <- match.arg(granularity)
  src <- bdnb_extracted_dir(data_dir, manifest_path)

  params <- digest::digest(list(
    departements = sort(departements), communes = sort(communes),
    granularity = granularity, include_indifferencie = include_indifferencie,
    drop_fictive_only = drop_fictive_only, min_fiabilite = min_fiabilite,
     geometry_policy = bdnb_geometry_policy(),
     dependance_flag = TRUE,
     usage_policy = list(source_field = "usage_principal_bdnb_open",
                         residential_usages = c(
                           bdnb_residential_usages(),
                           if (isTRUE(include_indifferencie))
                             "Résidentiel indifférencié"),
                         missing_row = "unknown_coverage")
  ), algo = "sha256")
  scope <- if (is.null(communes)) "deps" else sprintf("%02dcommunes", length(communes))
  cache <- bdnb_cache_path(
    data_dir,
    sprintf("residential_universe_%s_%s", scope, substr(params, 1L, 8L)),
    src$sha256
  )
  if (use_cache && file.exists(cache)) {
    return(readRDS(cache))
  }

  grp <- bdnb_residential_groups(departements, communes,
                                 include_indifferencie, data_dir, manifest_path)
  usage_audit_metadata <- attr(grp, "bdnb_usage_audit")
  if (nrow(grp) == 0L) {
    stop("no residential groups in scope", call. = FALSE)
  }

  # Constructions of the residential groups (physical footprints).
  cstr <- bdnb_read_table(
    "batiment_construction",
    cols = c("batiment_construction_id", "batiment_groupe_id",
             "WKT", "fictive_geom_cstr"),
    data_dir, manifest_path
  )
  cstr <- cstr[batiment_groupe_id %in% grp[["batiment_groupe_id"]]]

  # Construction centroids (fallback geometry).
  cstr_xy <- bdnb_centroid_xy(bdnb_wkt_sfc(cstr[["WKT"]]))
  cstr[, c("x_2154", "y_2154") := list(cstr_xy$x, cstr_xy$y)]

  # Group centroids (fallback geometry), fictive flagged separately.
  grp_xy <- bdnb_centroid_xy(bdnb_wkt_sfc(grp[["geom_groupe"]]))
  grp[, c("gx_2154", "gy_2154") := list(grp_xy$x, grp_xy$y)]

  # Address relations of the residential groups (n-m, rel_ table per the
  # contract — NOT the batiment_groupe_adresse summary).
  rel <- bdnb_read_table(
    "rel_batiment_groupe_adresse",
    cols = c("batiment_groupe_id", "cle_interop_adr", "fiabilite"),
    data_dir, manifest_path
  )
  rel <- rel[batiment_groupe_id %in% grp[["batiment_groupe_id"]]]
  adr_count <- rel[, .(n_adresses = .N, fiabilite_max = max(fiabilite)),
                   by = batiment_groupe_id]
  # Best relation per group (highest fiabilite) for the address point.
  rel[, .rank := data.table::frank(-fiabilite, ties.method = "first"),
      by = batiment_groupe_id]
  best <- rel[.rank == 1L, .(batiment_groupe_id, cle_interop_adr, fiabilite)]

  # Address points (BAN housenumber, geom_adresse) for the best relations.
  adr <- bdnb_read_table(
    "adresse",
    cols = c("cle_interop_adr", "WKT"),
    data_dir, manifest_path
  )
  adr <- adr[cle_interop_adr %in% best[["cle_interop_adr"]]]
  adr_xy <- bdnb_centroid_xy(bdnb_wkt_sfc(adr[["WKT"]]))
  adr[, c("ax_2154", "ay_2154") := list(adr_xy$x, adr_xy$y)]
  adr_pt <- adr[best, on = "cle_interop_adr"]

  # Construction-level address presence — the dépendance signal (ADR-0003).
  # rel_batiment_construction_adresse ships in open data. A construction in
  # a multi-construction group with NO construction-level relation while a
  # sibling construction HAS one is a shed/garage candidate; the universe
  # keeps it, flagged (the cut is a derivation-layer decision).
  cstr_adr <- bdnb_read_table(
    "rel_batiment_construction_adresse",
    cols = c("batiment_construction_id"),
    data_dir, manifest_path
  )
  cstr[, has_cstr_adr := batiment_construction_id %in%
         cstr_adr[["batiment_construction_id"]]]
  grp_cstr_adr <- cstr[, .(n_cstr = .N, n_cstr_adr = sum(has_cstr_adr)),
                       by = batiment_groupe_id]

  # Assemble origins -------------------------------------------------------
  # construction granularity: one origin per construction + one per
  # construction-less residential group; group granularity: one per group.
  if (granularity == "batiment_construction") {
    cstr_orig <- cstr[, .(origin_id = batiment_construction_id,
                          batiment_groupe_id, x_2154, y_2154,
                          fictive_geom_cstr, has_cstr_adr,
                          granularity = "batiment_construction")]
    no_cstr <- grp[!(batiment_groupe_id %in% cstr[["batiment_groupe_id"]]),
                   .(batiment_groupe_id)]
    grp_orig <- grp[no_cstr, on = "batiment_groupe_id"]
    grp_orig <- grp_orig[, .(origin_id = batiment_groupe_id,
                             batiment_groupe_id, x_2154 = NA_real_,
                             y_2154 = NA_real_,
                             fictive_geom_cstr = NA_integer_,
                             granularity = "groupe")]
    orig <- data.table::rbindlist(list(cstr_orig, grp_orig),
                                  use.names = TRUE, fill = TRUE)
  } else {
    # group granularity: one origin per group; geometry is resolved below.
    cstr_best <- cstr[fictive_geom_cstr == 0L, ]
    if (nrow(cstr_best) > 0L) {
      data.table::setorderv(cstr_best, c("batiment_groupe_id",
                                        "batiment_construction_id"))
      cstr_best[, .rank := seq_len(.N),
                by = batiment_groupe_id]
      cstr_best <- cstr_best[.rank == 1L,
                             .(batiment_groupe_id, cx_2154 = x_2154,
                               cy_2154 = y_2154,
                               fictive_geom_cstr = 0L)]
      # Y[X, on = ] keeps every group; groups without a non-fictive
      # construction get NA cx/cy and fall to group/address geometry.
      grp <- cstr_best[grp, on = "batiment_groupe_id"]
    } else {
      grp[, c("cx_2154", "cy_2154", "fictive_geom_cstr") :=
           list(NA_real_, NA_real_, NA_integer_)]
    }
    orig <- grp[, .(origin_id = batiment_groupe_id, batiment_groupe_id,
                    x_2154 = cx_2154, y_2154 = cy_2154,
                     fictive_geom_cstr,
                    granularity = "groupe")]
  }

  # Geometry hierarchy resolution ------------------------------------------
  grp_meta <- grp[, .(batiment_groupe_id, code_departement_insee,
                      code_commune_insee, usage_principal_bdnb_open,
                      contient_fictive_geom_groupe,
                      gx_2154, gy_2154)]
  orig <- grp_meta[orig, on = "batiment_groupe_id"]
  # Left-join semantics: X[Y, on = ] keeps only X rows matched by Y — the
  # address-less groups would vanish from the universe. Y[X, on = ] keeps
  # every orig row and attaches the address columns (NA where absent).
  orig <- adr_count[orig, on = "batiment_groupe_id"]
  orig <- adr_pt[orig, on = "batiment_groupe_id"]
  # Group construction counts (the dépendance flag needs them).
  orig <- grp_cstr_adr[orig, on = "batiment_groupe_id"]

  # Dépendance candidate (ADR-0003): a construction in a multi-construction
  # residential group with no construction-level address while a sibling
  # construction in the same group has one. Kept in the universe, flagged —
  # the cut is a derivation-layer decision, never baked into the run.
  # Group-granularity origins and construction-less groups are not
  # candidates (FALSE).
  is_dep <- orig[["granularity"]] == "batiment_construction" &
    !is.na(orig[["has_cstr_adr"]]) & orig[["has_cstr_adr"]] == 0L &
    !is.na(orig[["n_cstr"]]) & orig[["n_cstr"]] >= 2L &
    !is.na(orig[["n_cstr_adr"]]) & orig[["n_cstr_adr"]] >= 1L

  resolved <- bdnb_resolve_geometry(orig, min_fiabilite)
  src_label <- resolved[["geometry_source"]]
  orig[["x_2154"]] <- resolved[["x_2154"]]
  orig[["y_2154"]] <- resolved[["y_2154"]]

  keep <- !is.na(src_label)
  n_unresolved <- sum(!keep)
  if (isTRUE(drop_fictive_only)) {
    n_fictive <- sum(src_label == "fictive", na.rm = TRUE)
    keep <- keep & src_label != "fictive"
    if (n_fictive > 0L) {
      message(sprintf(
        "read_bdnb_residential_universe: dropped %d fictive-only origin(s) (drop_fictive_only)",
        n_fictive
      ))
    }
  }
  if (n_unresolved > 0L) {
    message(sprintf(
      "read_bdnb_residential_universe: %d origin(s) had no usable geometry at all (dropped)",
      n_unresolved
    ))
  }
  # One j-selection, no mutation of the join-derived `orig` (data.table::set
  # segfaults on tables built through X[Y, on = ] joins — no selfref guard).
  out <- orig[keep, .(origin_id, batiment_groupe_id, code_departement_insee,
                      code_commune_insee, usage_principal_bdnb_open,
                      granularity, n_adresses, fiabilite_max,
                      x_2154, y_2154, geometry_source = src_label[keep],
                      is_dependance_candidate = is_dep[keep])]
  data.table::setorder(out, code_commune_insee, origin_id)

  # Lineage (research note §7): millésime, per-dep source URLs, pins, crs.
  m_src <- manifest_load(manifest_path)$sources
  attr(out, "bdnb_millesime") <- "2026-02.a"
  attr(out, "bdnb_source") <- vapply(
    bdnb_source_ids(), function(id) m_src[[id]]$source, character(1)
  )
  attr(out, "bdnb_sha256") <- src$sha256
  attr(out, "bdnb_pins") <- src$pins
  attr(out, "bdnb_usage_audit") <- usage_audit_metadata
  attr(out, "bdnb_usage_policy") <- list(
    source_field = "usage_principal_bdnb_open",
    residential_usages = c(bdnb_residential_usages(),
                            if (isTRUE(include_indifferencie))
                              "Résidentiel indifférencié"),
    missing_row = "unknown_coverage"
  )
  attr(out, "bdnb_geometry_policy") <- bdnb_geometry_policy()
  attr(out, "crs") <- 2154L
  attr(out, "granularity") <- granularity
  attr(out, "cache_path") <- cache
  saveRDS(out, cache)
  out
}

#' The group↔address join-quality report (rel_batiment_groupe_adresse).
#'
#' Measures, over the residential groups of the scope: the % of groups with
#' ≥ 1 address relation (FR scale ≈ 84 % — recomputed on the acquired
#' department), the fiabilite distribution (0–20) per relation and per group
#' (max), and the batiment_groupe_adresse summary-table coverage as a
#' cross-check. Uses the n-m rel_ table, never the summary.
bdnb_group_address_metrics <- function(
    departements = c("22", "29", "35", "56"), communes = NULL,
    include_indifferencie = FALSE,
    data_dir = "data",
    manifest_path = file.path(data_dir, "manifest.json"),
    use_cache = TRUE) {
  src <- bdnb_extracted_dir(data_dir, manifest_path)
  params <- digest::digest(list(
    departements = sort(departements), communes = sort(communes),
    include_indifferencie = include_indifferencie
  ), algo = "sha256")
  scope <- if (is.null(communes)) "deps" else sprintf("%02dcommunes", length(communes))
  cache <- bdnb_cache_path(
    data_dir,
    sprintf("address_metrics_%s_%s", scope, substr(params, 1L, 8L)),
    src$sha256
  )
  if (use_cache && file.exists(cache)) {
    return(readRDS(cache))
  }

  grp <- bdnb_residential_groups(departements, communes,
                                 include_indifferencie, data_dir, manifest_path)
  rel <- bdnb_read_table(
    "rel_batiment_groupe_adresse",
    cols = c("batiment_groupe_id", "cle_interop_adr", "fiabilite"),
    data_dir, manifest_path
  )
  rel <- rel[batiment_groupe_id %in% grp[["batiment_groupe_id"]]]

  per_group <- rel[, .(n = .N, fiabilite_max = max(fiabilite)),
                   by = batiment_groupe_id]
  n_with <- sum(per_group[["n"]] > 0L)

  # Cross-check: the batiment_groupe_adresse summary (not the contract's
  # join — a coverage sanity check against the FR ≈ 84 % figure).
  summ <- bdnb_read_table(
    "batiment_groupe_adresse",
    cols = c("batiment_groupe_id"),
    data_dir, manifest_path
  )
  n_summary <- sum(summ[["batiment_groupe_id"]] %in% grp[["batiment_groupe_id"]])

  out <- list(
    scope = if (is.null(communes)) {
      paste(departements, collapse = ",")
    } else {
      paste(communes, collapse = ",")
    },
    n_residential_groups = nrow(grp),
    n_groups_with_address = n_with,
    pct_groups_with_address = round(100 * n_with / nrow(grp), 2),
    n_groups_without_address = nrow(grp) - n_with,
    fiabilite_per_relation = table(rel[["fiabilite"]], useNA = "ifany"),
    fiabilite_max_per_group = table(per_group[["fiabilite_max"]],
                                    useNA = "ifany"),
    n_summary_table_rows = n_summary,
    pct_summary_table_coverage = round(100 * n_summary / nrow(grp), 2)
  )
  saveRDS(out, cache)
  out
}
