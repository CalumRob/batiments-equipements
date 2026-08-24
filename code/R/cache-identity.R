# S16 — cache identity v2 + the cache-hit probe seam (#19).
#
# The expensive r5r network build (Bretagne + zone frontalière, the real
# one happens post-merge under #22's orchestration) is only worth paying
# for when its INPUTS change. Identity therefore hashes every input that
# can alter the built network:
#   * the exact OSM crop pin (id + sha256),
#   * EVERY selected transit pin (id + sha256, canonical radix sort —
#     staging order must not exist as a concept),
#   * the DEM pin, or the deliberate elevation = NONE setting,
#   * the r5r and R5 engine versions,
#   * W via border_width_m() and the cap via cap_minutes() (constants.R —
#     never reintroduced literals).
# The fingerprint string plus its structured components are exposed so run
# metadata folds them in verbatim; the components carry NO filesystem paths,
# so identity metadata is portable across worktrees by construction.

#' The r5r and R5 engine versions, read WITHOUT starting a JVM.
#'
#' packageVersion("r5r") reads installed metadata lazily; the R5 version is
#' recorded by r5r itself ("This version of r5r depends on R5 vX.Y" in the
#' package Description). No rJava load happens here — safe to call before
#' any heap setup.
r5r_runtime_versions <- function() {
  r5r_version <- as.character(utils::packageVersion("r5r"))
  d <- utils::packageDescription("r5r")
  desc <- if (is.null(d[["Description"]])) "" else d[["Description"]]
  m <- regmatches(desc,
                  regexpr("R5 v?[0-9]+[.][0-9]+([.][0-9]+)?", desc))
  if (!length(m)) {
    stop("could not determine the R5 version from r5r's package metadata",
         call. = FALSE)
  }
  list(r5r = r5r_version, r5 = sub("^R5 v?", "", m))
}

as_pin <- function(pin) {
  stopifnot(is.list(pin), !is.null(pin$id), !is.null(pin$sha256))
  stopifnot(length(pin$id) == 1L, nzchar(as.character(pin$id)),
            length(pin$sha256) == 1L, nzchar(as.character(pin$sha256)))
  list(id = as.character(pin$id), sha256 = as.character(pin$sha256))
}

#' Canonical order-independent lines for the selected transit pins.
#'
#' "feed:<id>=<sha256>" per pin, sorted with method = "radix" so the result
#' never depends on the C locale or on staging order.
canonical_transit_lines <- function(transit_pins) {
  if (length(transit_pins) == 0L) return(character(0))
  lines <- vapply(transit_pins, function(f) {
    p <- as_pin(f)
    paste0("feed:", p$id, "=", p$sha256)
  }, character(1L))
  sort(lines, method = "radix")
}

#' Build the structured identity components.
#'
#' Pure structural step over already-selected pins: no filesystem access, no
#' hashing of files (pins ARE the hashes). Returns a
#' network_identity_components object: osm_pin, transit_lines (canonical),
#' elevation_pin / elevation_label, r5r_version, r5_version, W_m, cap_minutes.
#' The canonical lines are always DERIVED from these fields
#' (network_identity_canonical_lines) — never stored alongside, so a mutated
#' component can never hash stale.
network_identity_components <- function(osm_pin, transit_pins,
                                        elevation_pin = NULL,
                                        versions = r5r_runtime_versions(),
                                        W = border_width_m(),
                                        cap = cap_minutes()) {
  osm <- as_pin(osm_pin)
  stopifnot(is.list(transit_pins))
  stopifnot(is.numeric(W), length(W) == 1L, !is.na(W), W > 0)
  stopifnot(is.numeric(cap), length(cap) == 1L, !is.na(cap), cap > 0)
  elevation <- NULL
  if (!is.null(elevation_pin) && !identical(elevation_pin, "NONE")) {
    elevation <- as_pin(elevation_pin)
  }
  elev_label <- if (is.null(elevation)) "NONE"
                else paste0(elevation$id, "=", elevation$sha256)

  structure(
    list(
      osm_pin = osm,
      n_transit_feeds = length(transit_pins),
      transit_lines = canonical_transit_lines(transit_pins),
      elevation_pin = elevation,
      elevation_label = elev_label,
      r5r_version = as.character(versions$r5r),
      r5_version = as.character(versions$r5),
      W_m = as.integer(W),
      cap_minutes = as.integer(cap)
    ),
    class = "network_identity_components"
  )
}

#' The canonical lines an identity hashes — derived from the components.
network_identity_canonical_lines <- function(components) {
  stopifnot(inherits(components, "network_identity_components"))
  c(
    "batiments-equipements/network-cache-identity/v2",
    paste0("osm=", components$osm_pin$id, "=", components$osm_pin$sha256),
    paste0("transit_n=", components$n_transit_feeds),
    components$transit_lines,
    paste0("elevation=", components$elevation_label),
    paste0("r5r=", components$r5r_version),
    paste0("r5=", components$r5_version),
    sprintf("W=%dm", components$W_m),
    sprintf("cap=%dmin", components$cap_minutes)
  )
}

#' The sha256 fingerprint over the canonical component lines.
network_fingerprint <- function(components) {
  stopifnot(inherits(components, "network_identity_components"))
  digest::digest(
    paste(network_identity_canonical_lines(components), collapse = "\n"),
    algo = "sha256", serialize = FALSE
  )
}

#' The network cache identity: fingerprint string + structured components.
#'
#' One call exposes both forms (#19): `fingerprint` decides reuse; the
#' `components` fold into run metadata. Accepts the staging seam's output
#' directly — pass stage_transit_feeds()'s `feeds` as `transit_pins`.
#'
#' @param osm_pin list(id, sha256) of the OSM crop pin the network builds on.
#' @param transit_pins list of list(id, sha256[, ...]) — extra fields ignored.
#' @param elevation_pin NULL (or "NONE") for the deliberate compatibility
#'   setting, else list(id, sha256) of the validated DEM pin.
#' @param versions r5r_runtime_versions() by default.
#' @param W,cap The named constants (border_width_m(), cap_minutes()) — the
#'   defaults consume the single source of truth in constants.R.
network_cache_identity <- function(osm_pin, transit_pins, elevation_pin = NULL,
                                   versions = r5r_runtime_versions(),
                                   W = border_width_m(),
                                   cap = cap_minutes()) {
  components <- network_identity_components(osm_pin, transit_pins,
                                            elevation_pin, versions, W, cap)
  list(fingerprint = network_fingerprint(components),
       components = components)
}

#' The marker file name recording a network directory's built identity.
network_identity_marker_name <- function() ".network-identity.json"

network_identity_marker_path <- function(network_dir) {
  file.path(network_dir, network_identity_marker_name())
}

#' Commit an identity marker after a successful network build.
#'
#' Called ONCE, after setup_r5 has produced the network in `network_dir` and
#' every staged feed has passed verification. Writes fingerprint +
#' components as pretty JSON — no filesystem paths inside, portable by
#' construction. Returns the marker path invisibly.
commit_network_cache <- function(network_dir, identity) {
  stopifnot(is.character(network_dir), length(network_dir) == 1L)
  if (!dir.exists(network_dir)) {
    stop("cannot commit identity marker outside an existing network directory: ",
         network_dir, call. = FALSE)
  }
  stopifnot(!is.null(identity$fingerprint),
            inherits(identity$components, "network_identity_components"))
  p <- network_identity_marker_path(network_dir)
  jsonlite::write_json(
    list(version = 1L,
         fingerprint = identity$fingerprint,
         canonical_lines = network_identity_canonical_lines(identity$components),
         components = unclass(identity$components)),
    p, auto_unbox = TRUE, pretty = TRUE
  )
  invisible(p)
}

#' Probe a built network directory for identity match.
#'
#' THE cache-hit seam (#19): a second setup invocation calls this BEFORE any
#' rebuild. Hit = the directory carries a committed marker whose fingerprint
#' equals the requested identity's — reuse the network.dat without paying
#' the Bretagne build again. Every other outcome is a MISS with the reason
#' spelled out (no dir, no marker, corrupt marker, fingerprint mismatch);
#' the caller then rebuilds and re-commits. The real post-merge Bretagne
#' build uses exactly this path at fixture scale proven here with a marker
#' standing in for network.dat.
probe_network_cache <- function(network_dir, identity) {
  expected <- if (inherits(identity, "network_identity_components")) {
    network_fingerprint(identity)
  } else {
    identity$fingerprint
  }
  miss <- function(reason, found = NULL) {
    invisible(list(cache_hit = FALSE, reason = reason,
                   expected_fingerprint = expected,
                   found_fingerprint = found))
  }
  if (!dir.exists(network_dir)) return(miss("network directory absent"))
  marker <- network_identity_marker_path(network_dir)
  if (!file.exists(marker)) return(miss("no identity marker committed"))
  j <- tryCatch(jsonlite::fromJSON(marker, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j) || is.null(j$fingerprint)) {
    return(miss("identity marker unreadable (corrupt JSON)"))
  }
  found <- as.character(j$fingerprint)
  if (!identical(found, expected)) {
    return(miss(sprintf(
      "fingerprint mismatch — marker %s vs requested %s (an input changed)",
      found, expected), found))
  }
  invisible(list(cache_hit = TRUE,
                 reason = "identity match",
                 expected_fingerprint = expected,
                 found_fingerprint = found))
}
