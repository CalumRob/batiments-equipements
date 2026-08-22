# Shared test helper: a deterministic stand-in for r5r::travel_time_matrix.
# Travel time is a PURE FUNCTION OF THE COORDINATES ALONE (plus a fixed base
# access/egress) — the property that makes exact-coordinate deduplication
# (#20) provably lossless. Pairs beyond `cap` are omitted: the sparse
# contract, by construction.
stub_route_pairs <- function(origins, destinations, cap = 20,
                             base_minutes = 2, km_per_degree = 80,
                             speed_kmh = 30) {
  stopifnot(is.data.frame(origins), is.data.frame(destinations))
  o <- data.table::as.data.table(origins)
  d <- data.table::as.data.table(destinations)
  oid <- intersect(c("id", "point_id"), names(o))[[1L]]
  did <- intersect(c("id", "point_id"), names(d))[[1L]]
  left <- o[, .(k = 1, from_id = get(oid), from_lon = lon, from_lat = lat)]
  right <- d[, .(k = 1, to_id = get(did), to_lon = lon, to_lat = lat)]
  grid <- left[right, on = "k", allow.cartesian = TRUE][, .(
    from_id, to_id,
    dist_km = sqrt(((from_lon - to_lon) * km_per_degree)^2 +
                     ((from_lat - to_lat) * 111)^2)
  )]
  grid[, travel_time := base_minutes + ceiling(dist_km / speed_kmh * 60)]
  grid[travel_time <= cap, .(from_id, to_id, travel_time)]
}
