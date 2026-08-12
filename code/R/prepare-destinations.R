# S12 — destination preparation for the once-run driver: the destination
# identity scheme + the lossless registry back-link (extracted verbatim from
# run-tracer.R's inline prep, ticket 06 follow-up, maintainer decision
# 2026-08-12). The matrix outputs are TYPEQU-aggregated (batiment_id, TYPEQU,
# mode, tt_nearest, ladder) but the routing layer builds per-destination ids —
# base_id|TYPEQU, with a synthetic no-siret_%06d base id for the 33.2 % of BPE
# 2025 rows with an empty SIRET — that were EPHEMERAL, built inside run_tracer
# and discarded. The destination registry persists them: any destination id can
# be linked back to the original BPE 2025 universe rows.
#
# prepare_bpe_destinations reads the pinned BPE universe (read_bpe_universe,
# S7), applies the routable filter, derives the per-point base ids exactly as
# the tracer's recorded run did (same operations, same order — the ids are
# byte-identical to what was routed), and returns the routing destinations +
# map PLUS the lossless registry. The matrix contract itself is UNCHANGED (the
# registry is a sidecar; the axis-extension rule holds). write_destination_registry
# persists the sidecar parquet (data/matrice/destination_registry.parquet),
# written once per run by the driver before the chunk loop (a pure function of
# the pinned universe — cache-hit derived, no routing involved).
#
# Reading discipline: this file's data.tables are extracted with `[[` or `j`,
# NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' Prepare the routing destinations + the lossless destination registry.
#'
#' Reads the pinned BPE universe (read_bpe_universe, S7 — the setorder'd
#' bpe_universe_<Wkm>_<sha12>.rds cache), keeps the routable rows (non-NA
#' coordinates), and derives the destination identity exactly as the tracer's
#' recorded run did:
#' \itemize{
#'   \item unique routable points first (dedupes exact (SIRET, point) repeats);
#'   \item a base id per point: SIRET where present, else a synthetic
#'     \code{no-siret_%06d} (per-point; the establishment identity is the
#'     point) — sequential over the empty points, in the points' order;
#'   \item one routing destination per (base id, TYPEQU), id = base_id|TYPEQU.
#' }
#' The map then has unique ids (derive_matrix_rows' requirement) and each
#' TYPEQU an establishment hosts counts once (derive.R's establishment
#' semantic). The REGISTRY is the lossless back-link: ONE row per routable
#' universe row (the rds is setorder'd and sha-pinned, so universe_row i is
#' stable), carrying the destination id that row maps to plus the original
#' universe columns (SIRET, DEP, DEPCOM, TYPEQU, lon, lat, zone). It is built
#' from the same point -> base_id mapping as the destinations, so the ids
#' match exactly.
#'
#' @param W Strip width in metres for read_bpe_universe (ADR-0002; 25 km
#'   today).
#' @param data_dir The project data root.
#' @param manifest_path The acquisition manifest.
#' @param use_cache Hit the readers' caches (the acquired universes).
#'
#' @return A list:
#'   \item{destinations}{data.table(id, lon, lat) — byte-identical to what
#'     routing used (r5r's to_id list).}
#'   \item{dest_map}{data.table(id, TYPEQU), unique ids — derive_matrix_rows'
#'     to_id -> TYPEQU map.}
#'   \item{registry}{data.table(universe_row, id, base_id, TYPEQU, SIRET, DEP,
#'     DEPCOM, lon, lat, zone) — one row per routable BPE universe row, the
#'     lossless back-link.}
#'   \item{n_universe}{rows in the pinned universe rds.}
#'   \item{n_routable}{routable rows (non-NA coords).}
#'   \item{n_na_coord}{universe rows excluded for NA coordinates.}
#'   \item{n_bretagne, n_zone_frontaliere}{universe zone split.}
#'   \item{n_siret_na, n_siret_shared, n_empty_siret}{SIRET facts (0 NA; rows
#'     sharing a SIRET; routable empty-SIRET rows).}
#'   \item{n_pts, n_empty_pts}{unique routing points and the empty-SIRET
#'     points among them.}
#' @export
prepare_bpe_destinations <- function(W = 25000, data_dir = "data",
                                     manifest_path = file.path(data_dir, "manifest.json"),
                                     use_cache = TRUE) {
  stopifnot(is.numeric(W), length(W) == 1L, !is.na(W), W > 0)

  # The pinned BPE universe (S7): setorder'd DEP/DEPCOM/TYPEQU, sha-pinned —
  # row i of the rds is stable, so universe_row below indexes it directly.
  bpe <- read_bpe_universe(W = W, data_dir = data_dir,
                           manifest_path = manifest_path, use_cache = use_cache)
  n_universe <- nrow(bpe)

  # universe_row = the row index in the pinned rds, carried BEFORE the
  # routable filter (the registry's lossless grain needs the original rows).
  bpe[, universe_row := .I]
  rout <- bpe[!is.na(bpe[["LONGITUDE"]]) & !is.na(bpe[["LATITUDE"]])]
  n_routable <- nrow(rout)
  n_na_coord <- n_universe - n_routable

  # SIRET sanity (known: 0 NA — the empty-string case is the real quirk).
  n_siret_na <- sum(is.na(bpe[["SIRET"]]))
  n_siret_shared <- n_universe - data.table::uniqueN(bpe[["SIRET"]])

  # Destination identity, per the matrix contract (see the file header):
  #   - unique routable points first (dedupes exact (SIRET, point) repeats);
  #   - a base id per point: SIRET where present, else a synthetic
  #     no-siret_%06d (per-point; the establishment identity is the point);
  #   - one routing destination per (base id, TYPEQU), id = base_id|TYPEQU —
  #     the map then has unique ids (derive_matrix_rows' requirement) and each
  #     TYPEQU an establishment hosts counts once (derive.R's establishment
  #     semantic).
  pts <- unique(rout[, .(SIRET, lon = LONGITUDE, lat = LATITUDE)])
  base_id <- pts[["SIRET"]]
  empty_pts <- which(!nzchar(base_id))
  base_id[empty_pts] <- sprintf("no-siret_%06d", seq_along(empty_pts))
  data.table::set(pts, j = "base_id", value = base_id)
  rout_pts <- unique(rout[, .(SIRET, lon = LONGITUDE, lat = LATITUDE, TYPEQU)])
  mapped <- pts[rout_pts, on = c("SIRET", "lon", "lat")]
  data.table::set(mapped, j = "id",
                  value = paste0(mapped[["base_id"]], "|", mapped[["TYPEQU"]]))
  destinations <- mapped[, .(id, lon, lat)]
  dest_map <- unique(mapped[, .(id, TYPEQU)])

  # The registry — the lossless back-link. One row per ROUTABLE universe row
  # (nrow == n_routable): each rout row maps to the SAME (SIRET, lon, lat)
  # point -> base_id used for the destinations (pts is unique on the join
  # keys, so every rout row gets exactly one base_id), and id = base_id|TYPEQU
  # exactly as the destinations were built — the ids match byte-for-byte.
  registry <- rout[, .(universe_row, SIRET, DEP, DEPCOM, TYPEQU,
                       lon = LONGITUDE, lat = LATITUDE, zone)]
  registry <- pts[registry, on = c("SIRET", "lon", "lat")]
  data.table::set(registry, j = "id",
                  value = paste0(registry[["base_id"]], "|", registry[["TYPEQU"]]))
  data.table::setcolorder(registry, c(
    "universe_row", "id", "base_id", "TYPEQU", "SIRET",
    "DEP", "DEPCOM", "lon", "lat", "zone"
  ))

  list(
    destinations = destinations,
    dest_map = dest_map,
    registry = registry,
    n_universe = n_universe,
    n_routable = n_routable,
    n_na_coord = n_na_coord,
    n_bretagne = sum(bpe[["zone"]] == "bretagne"),
    n_zone_frontaliere = sum(bpe[["zone"]] == "zone_frontaliere"),
    n_siret_na = n_siret_na,
    n_siret_shared = n_siret_shared,
    n_empty_siret = sum(!nzchar(rout[["SIRET"]])),
    n_pts = nrow(pts),
    n_empty_pts = length(empty_pts)
  )
}

#' Write the destination registry sidecar parquet.
#'
#' A pure write mirroring write_matrix_chunk's style (link.R, S10): creates
#' the parent directory and writes `registry` (the lossless back-link table
#' from prepare_bpe_destinations) with arrow::write_parquet to
#' data/matrice/destination_registry.parquet by default. Returns the path
#' invisibly. The registry is a sidecar: the matrix contract is unchanged,
#' and the driver writes it once per run, before the chunk loop.
#'
#' @param registry The registry data.table from prepare_bpe_destinations.
#' @param out_path Where the parquet lands.
#' @return The file path, invisibly.
#' @export
write_destination_registry <- function(registry,
                                       out_path = file.path("data", "matrice",
                                                            "destination_registry.parquet")) {
  stopifnot(is.data.frame(registry))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(registry, out_path)
  invisible(out_path)
}
