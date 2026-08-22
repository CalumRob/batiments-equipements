# S13 — coordinate-level routing: route each exact origin coordinate and each
# exact destination coordinate ONCE, then expand the pairs back to every
# identity BEFORE derive-layer aggregation (#20).
#
# The once-run routes identities that share an exact coordinate over and over:
# co-located BPE listings (one multi-service building hosting several TYPEQU)
# and co-locked residential constructions (identical footprint centroids) are
# routed as separate points although r5r's answer is identical for identical
# coordinates — travel time is a pure function of the coordinate pair on a
# fixed network. The seam here exploits exactly that, and nothing more:
#
#   * deduplication is EXACT coordinate equality — no rounding-based grouping,
#     no spatial snapping to the network beyond what r5r itself does, and no
#     SIRET/NOMRS identity grouping (co-located distinct listings stay
#     distinct identities);
#   * expansion is LOSSLESS — every identity pair the reference (non-
#     deduplicated) routing would produce exists with the same travel time,
#     so the derived matrix is semantically bit-for-bit equivalent;
#   * expansion happens BEFORE derive_matrix_rows / derive_transit_matrix_rows,
#     so downstream derivations never see deduplicated identities.
#
# The driver (run-tracer.R) consumes the seam per mode: it builds both plans
# once (the destination plan before the chunk loop, the origin plan over the
# whole universe so each unique coordinate routes exactly once across chunks),
# chunks over the unique origin coordinates, routes them against the unique
# destination coordinates, and expands each chunk's pairs back to identities.
#
# Reading discipline: this file's data.tables are extracted with `[[` or `j`,
# NEVER `$` — on this R version `dt$col` returns a broken object whose
# unique() collapses to nonsense counts.

#' Build the coordinate-level routing plan of one side (origins or destinations).
#'
#' Groups points by EXACT (lon, lat) equality — never by rounding or snapping —
#' and assigns one synthetic routing id (\code{point_id}) per unique
#' coordinate. Returns the routing table (one row per unique coordinate) plus
#' the lossless link from every identity \code{id} to its \code{point_id}.
#'
#' @param points A data.frame/data.table with columns \code{id}, \code{lon},
#'   \code{lat} — one row per routing identity (BDNB origin ids; BPE listing
#'   ids). Ids must be unique and coordinates non-NA (run_tracer filters NA
#'   origins upstream; NA destinations never reach the routing table).
#' @param prefix Prefix for the synthetic point ids (origins and destinations
#'   use different prefixes so ids cannot collide).
#'
#' @return A list:
#'   \item{points}{data.table(id, lon, lat) — one row per UNIQUE exact
#'     coordinate, id = the synthetic point id; directly consumable by the
#'     r5r wrappers (they require an id column). This is what gets routed.}
#'   \item{link}{data.table(id, point_id) — one row per input identity (id =
#'     the original identity, point_id = its synthetic routing id); this is
#'     what expansion joins back.}
#' @export
coordinate_routing_plan <- function(points, prefix = "coord") {
  stopifnot(is.data.frame(points))
  required <- c("id", "lon", "lat")
  missing <- setdiff(required, names(points))
  if (length(missing) > 0L) {
    stop(sprintf("routing points missing required columns: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  stopifnot(is.character(prefix), length(prefix) == 1L, !is.na(prefix),
            nzchar(prefix))
  pts <- data.table::as.data.table(points)
  pts <- pts[, required, with = FALSE]
  if (anyDuplicated(pts[["id"]])) {
    stop("routing points must carry unique identity ids (duplicate id)",
         call. = FALSE)
  }
  if (anyNA(pts[["lon"]]) || anyNA(pts[["lat"]])) {
    stop("routing points carry NA lon/lat — filter unroutable identities upstream (run_tracer excludes NA origins with a warning)",
         call. = FALSE)
  }

  # EXACT coordinate equality only: unique() on the raw doubles. No rounding,
  # no snapping, no tolerance — near-identical but distinct coordinates stay
  # distinct routing points (#20's hard constraint).
  coords <- unique(pts[, .(lon, lat)])
  data.table::set(coords, j = "point_id",
                  value = sprintf("%s_%06d", prefix, seq_len(nrow(coords))))
  # One row per identity, in the input order: every identity maps to exactly
  # one point_id. Rename before joining so the joined-back column is
  # unambiguous (data.table prefixes Y's columns with i. only on collisions).
  ptsi <- pts[, .(identity_id = id, lon, lat)]
  link <- coords[ptsi, on = .(lon, lat), nomatch = 0L]
  link <- link[, .(id = identity_id, point_id)]
  # The routing table names the synthetic id `id` so the r5r wrappers
  # consume it unchanged (travel_time_matrix requires id/lon/lat).
  list(points = coords[, .(id = point_id, lon, lat)], link = link)
}

#' Expand coordinate-level pairs back to every routing identity.
#'
#' The inverse of the plan step: pairs keyed on synthetic point ids become
#' pairs keyed on identity ids, one output row per (pair row x matching origin
#' identity x matching destination identity). Lossless by construction — the
#' same routed travel time reaches every identity at that exact coordinate —
#' and sparse-preserving: only routed pairs expand, unreachable ones stay
#' absent. Every other column (travel_time, or transit's percentile pair)
#' passes through untouched.
#'
#' @param pairs Coordinate-level route pairs: data.frame with columns
#'   \code{from_id}, \code{to_id} (point ids) plus any value columns.
#' @param origin_link Identity link from coordinate_routing_plan (origins).
#' @param destination_link Identity link from coordinate_routing_plan
#'   (destinations).
#'
#' @return Pairs keyed on identity ids (columns preserved, order preserved).
#' @export
expand_pairs_to_identities <- function(pairs, origin_link, destination_link) {
  stopifnot(is.data.frame(pairs), is.data.frame(origin_link),
            is.data.frame(destination_link))
  p <- data.table::as.data.table(data.table::copy(pairs))
  ol <- data.table::as.data.table(origin_link)
  dl <- data.table::as.data.table(destination_link)
  for (tab in list(list(p, c("from_id", "to_id")),
                   list(ol, c("id", "point_id")),
                   list(dl, c("id", "point_id")))) {
    miss <- setdiff(tab[[2L]], names(tab[[1L]]))
    if (length(miss) > 0L) {
      stop(sprintf("expansion table missing required columns: %s",
                   paste(miss, collapse = ", ")), call. = FALSE)
    }
  }

  value_cols <- setdiff(names(p), c("from_id", "to_id"))
  # Internal-consistency guard: a routed point id absent from its link would
  # silently lose pairs through nomatch = 0L — fail loudly instead.
  if (nrow(p) > 0L) {
    unknown_from <- setdiff(unique(p[["from_id"]]), ol[["point_id"]])
    if (length(unknown_from) > 0L) {
      stop(sprintf("pairs carry origin point ids absent from the origin link: %s",
                   paste(utils::head(unknown_from, 5L), collapse = ", ")),
           call. = FALSE)
    }
    unknown_to <- setdiff(unique(p[["to_id"]]), dl[["point_id"]])
    if (length(unknown_to) > 0L) {
      stop(sprintf("pairs carry destination point ids absent from the destination link: %s",
                   paste(utils::head(unknown_to, 5L), collapse = ", ")),
           call. = FALSE)
    }
  }
  if (nrow(p) == 0L) {
    # A fully unreachable pass: nothing to expand; keep the exact shape.
    return(p[])
  }

  # Origin side (X[Y] drives rows by Y = pairs; X's columns stay unprefixed):
  # one row per (pair row x origin identity). allow.cartesian = TRUE is the
  # POINT of this join — co-located identities legitimately replicate each
  # routed pair — not an accidental many-to-many.
  out <- ol[p, on = c(point_id = "from_id"), nomatch = 0L,
            allow.cartesian = TRUE]
  data.table::set(out, j = "from_id", value = out[["id"]])
  out[, c("id", "point_id") := NULL]
  # Destination side: same expansion for to_id.
  out <- dl[out, on = c(point_id = "to_id"), nomatch = 0L,
            allow.cartesian = TRUE]
  data.table::set(out, j = "to_id", value = out[["id"]])
  out[, c("id", "point_id") := NULL]

  data.table::setcolorder(out, c("from_id", "to_id", value_cols))
  out[]
}

#' Route each exact origin/destination coordinate once; return identity pairs.
#'
#' The deep seam of #20, composing coordinate_routing_plan + routing +
#' expand_pairs_to_identities: \code{origins} and \code{destinations} are
#' identity tables (id, lon, lat); \code{route_fn} is any travel_time_matrix-
#' shaped function of two such tables (e.g. a route_pairs wrapper). It is
#' called ONCE with the deduplicated coordinate tables, and the result is
#' expanded back so the caller sees exactly the pairs the reference (non-
#' deduplicated) routing would have produced.
#'
#' The run_tracer driver consumes the primitives directly instead (plans built
#' once, expansion per chunk); this wrapper is the single-pass form used to
#' prove equivalence against reference routing.
#'
#' @return A list:
#'   \item{pairs}{Identity-level route pairs (expanded, sparse).}
#'   \item{n_origins_input, n_origins_routed}{Identities vs unique origin
#'     coordinates actually handed to the router.}
#'   \item{n_destinations_input, n_destinations_routed}{Same for
#'     destinations.}
#' @export
route_unique_coordinates <- function(origins, destinations, route_fn) {
  stopifnot(is.function(route_fn))
  op <- coordinate_routing_plan(origins, prefix = "coord_o")
  dp <- coordinate_routing_plan(destinations, prefix = "coord_d")
  routed <- route_fn(op$points, dp$points)
  stopifnot(is.data.frame(routed))
  list(
    pairs = expand_pairs_to_identities(routed, op$link, dp$link),
    n_origins_input = nrow(op$link),
    n_origins_routed = nrow(op$points),
    n_destinations_input = nrow(dp$link),
    n_destinations_routed = nrow(dp$points)
  )
}
