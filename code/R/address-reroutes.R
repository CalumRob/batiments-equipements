# Address-level supplemental routing for a completed once-run.
#
# The once-run's canonical origins are residential constructions.  This module
# builds the deliberately narrower address-origin universe used for the honest
# public address view:
#
#   * an address is eligible when it is linked to at least one residential
#     construction that is not an ADR-0003 dépendance candidate;
#   * each address identity appears once, even when it is linked to several
#     constructions or groups;
#   * exact coordinate equality is the only routing deduplication rule;
#   * coordinates already present in the completed once-run are reused;
#   * only genuinely new address coordinates are sent through r5r.
#
# Matrix parquet artifacts are written by run_resumable() into
#   <base-run>/chunks/reroutes/{walk,transit,bike,car}/
# with the address universe and crosswalks beside them under plan/.

address_reroute_required_origin_columns <- function() {
  c("origin_id", "batiment_groupe_id", "is_dependance_candidate")
}

address_reroute_required_relation_columns <- function() {
  c("batiment_construction_id", "cle_interop_adr")
}

address_reroute_required_point_columns <- function() {
  c("address_id", "lon", "lat")
}

address_reroute_root <- function(base_run_dir) {
  file.path(base_run_dir, "chunks", "reroutes")
}

#' Convert BDNB address WKT into one WGS84 point per address identity.
address_reroute_address_points_from_wkt <- function(addresses) {
  addresses <- data.table::as.data.table(addresses)
  required <- c("cle_interop_adr", "WKT")
  missing <- setdiff(required, names(addresses))
  if (length(missing)) {
    stop("address table missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  addresses <- addresses[, .(address_id = as.character(cle_interop_adr), WKT)]
  addresses <- addresses[!is.na(address_id) & nzchar(address_id)]
  geometry_counts <- addresses[, .(n_geometries = data.table::uniqueN(WKT)),
                               by = address_id]
  if (any(geometry_counts[["n_geometries"]] > 1L)) {
    bad <- geometry_counts[n_geometries > 1L, address_id]
    stop("address identities have conflicting geometries: ",
         paste(utils::head(bad, 5L), collapse = ", "), call. = FALSE)
  }
  addresses <- unique(addresses)
  if (anyNA(addresses[["WKT"]]) || any(!nzchar(addresses[["WKT"]]))) {
    stop("eligible address identities must have non-empty WKT geometry",
         call. = FALSE)
  }
  xy <- bdnb_centroid_xy(bdnb_wkt_sfc(addresses[["WKT"]]))
  addresses[, `:=`(x_2154 = xy[["x"]], y_2154 = xy[["y"]])]
  if (anyNA(addresses[["x_2154"]]) || anyNA(addresses[["y_2154"]])) {
    stop("eligible address identities must have resolved coordinates",
         call. = FALSE)
  }
  pts <- sf::st_as_sf(as.data.frame(addresses),
                      coords = c("x_2154", "y_2154"),
                      crs = 2154L, remove = FALSE)
  pts <- sf::st_transform(pts, 4326L)
  lonlat <- as.data.frame(sf::st_coordinates(pts))
  addresses[, `:=`(lon = lonlat[["X"]], lat = lonlat[["Y"]])]
  addresses[, c("WKT") := NULL]
  addresses[]
}

#' Build the address universe and the new-coordinate routing plan.
#'
#' This is the pure data seam: readers and WKT conversion stay outside it so
#' the eligibility rule can be tested without the full BDNB acquisition.
#'
#' @return list(addresses, address_construction_link, new_origins,
#'   new_origin_plan, counts).
build_address_reroute_universe <- function(origins, construction_addresses,
                                           address_points, existing_points) {
  origins <- data.table::as.data.table(origins)
  construction_addresses <- data.table::as.data.table(construction_addresses)
  address_points <- data.table::as.data.table(address_points)
  existing_points <- data.table::as.data.table(existing_points)

  missing <- setdiff(address_reroute_required_origin_columns(), names(origins))
  if (length(missing)) {
    stop("origins missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  missing <- setdiff(address_reroute_required_relation_columns(),
                     names(construction_addresses))
  if (length(missing)) {
    stop("construction-address relation missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(address_reroute_required_point_columns(), names(address_points))
  if (length(missing)) {
    stop("address points missing column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!all(c("id", "lon", "lat") %in% names(existing_points))) {
    stop("existing points must contain id, lon, and lat", call. = FALSE)
  }

  origins <- origins[, .(
    construction_id = as.character(origin_id),
    batiment_groupe_id = as.character(batiment_groupe_id),
    is_dependance_candidate
  )]
  # NA is not a positive dependence signal, but only construction origins are
  # eligible here; an unknown flag is therefore refused rather than silently
  # promoted into the public address universe.
  origins <- origins[!is.na(construction_id) & nzchar(construction_id) &
                       !is.na(is_dependance_candidate) &
                       is_dependance_candidate == FALSE]

  relation <- construction_addresses[, .(
    construction_id = as.character(batiment_construction_id),
    address_id = as.character(cle_interop_adr)
  )]
  relation <- unique(relation[!is.na(construction_id) & nzchar(construction_id) &
                                !is.na(address_id) & nzchar(address_id)])
  links <- merge(relation, origins, by = "construction_id", all = FALSE)
  links <- unique(links[, .(address_id, construction_id,
                            batiment_groupe_id)])
  if (nrow(links) == 0L) {
    stop("no eligible residential non-dependance construction-address links",
         call. = FALSE)
  }

  address_points <- address_points[, .(
    address_id = as.character(address_id),
    lon = as.numeric(lon), lat = as.numeric(lat),
    x_2154 = if ("x_2154" %in% names(address_points))
      as.numeric(x_2154) else NA_real_,
    y_2154 = if ("y_2154" %in% names(address_points))
      as.numeric(y_2154) else NA_real_
  )]
  coordinate_counts <- address_points[, .(
    n_coordinates = data.table::uniqueN(data.table::data.table(lon, lat))
  ), by = address_id]
  if (any(coordinate_counts[["n_coordinates"]] > 1L)) {
    bad <- coordinate_counts[n_coordinates > 1L, address_id]
    stop("address identities have conflicting lon/lat coordinates: ",
         paste(utils::head(bad, 5L), collapse = ", "), call. = FALSE)
  }
  address_points <- unique(address_points)

  address_stats <- links[, .(
    n_non_dependance_constructions = data.table::uniqueN(construction_id),
    n_residential_groups = data.table::uniqueN(batiment_groupe_id)
  ), by = address_id]
  # Keep the eligible-address set as the left side of the join.  Reversing
  # this join would retain every row in the national adresse table, including
  # addresses never linked to a residential non-dependance construction.
  addresses <- merge(address_stats, address_points, by = "address_id",
                     all.x = TRUE, sort = FALSE)
  missing_coords <- addresses[is.na(lon) | is.na(lat), address_id]
  if (length(missing_coords)) {
    stop("eligible address identities missing geometry: ",
         paste(utils::head(missing_coords, 5L), collapse = ", "),
         call. = FALSE)
  }
  existing <- existing_points[, .(
    existing_point_id = as.character(id), lon = as.numeric(lon),
    lat = as.numeric(lat)
  )]
  if (anyNA(existing[["lon"]]) || anyNA(existing[["lat"]])) {
    stop("existing routing points must not contain NA coordinates", call. = FALSE)
  }
  if (anyDuplicated(existing[, .(lon, lat)])) {
    stop("existing routing points must already be exact-coordinate unique",
         call. = FALSE)
  }
  addresses <- merge(addresses, existing, by = c("lon", "lat"),
                     all.x = TRUE, sort = FALSE)
  addresses[, routing_source := data.table::fifelse(
    is.na(existing_point_id), "reroute", "existing"
  )]

  new_origins <- addresses[routing_source == "reroute",
                           .(id = address_id, lon, lat)]
  # The resumable runner rebuilds this plan from origins_provider. Keep the
  # exact same order and prefix so the persisted address crosswalk names the
  # coordinates the workers actually consume.
  data.table::setorder(new_origins, id)
  new_origin_plan <- coordinate_routing_plan(new_origins, prefix = "coord_o")
  new_link <- data.table::as.data.table(new_origin_plan$link)
  data.table::setnames(new_link, "id", "address_id")
  addresses <- new_link[addresses, on = "address_id"]
  addresses[, routing_point_id := data.table::fifelse(
    routing_source == "existing", existing_point_id, point_id
  )]
  addresses[, point_id := NULL]
  data.table::setorder(addresses, address_id)

  list(
    addresses = addresses,
    address_construction_link = links,
    new_origins = new_origins,
    new_origin_plan = new_origin_plan,
    counts = list(
      n_eligible_constructions = nrow(origins),
      n_eligible_addresses = nrow(addresses),
      n_new_address_identities = nrow(new_origins),
      n_new_coordinates = nrow(new_origin_plan$points)
    )
  )
}

#' Read real BDNB inputs and build the address reroute universe.
read_address_reroute_universe <- function(base_run_dir,
                                          data_dir = "data",
                                          manifest_path = file.path(data_dir,
                                                                    "manifest.json"),
                                          use_cache = TRUE) {
  origins <- read_bdnb_residential_universe(
    departements = c("22", "29", "35", "56"),
    data_dir = data_dir, manifest_path = manifest_path, use_cache = use_cache
  )
  relation <- bdnb_read_table(
    "rel_batiment_construction_adresse",
    cols = c("batiment_construction_id", "cle_interop_adr"),
    data_dir = data_dir, manifest_path = manifest_path
  )
  addresses <- bdnb_read_table(
    "adresse", cols = c("cle_interop_adr", "WKT"),
    data_dir = data_dir, manifest_path = manifest_path
  )
  eligible_construction_ids <- origins[
    !is.na(is_dependance_candidate) & !is_dependance_candidate,
    as.character(origin_id)
  ]
  eligible_address_ids <- unique(relation[
    data.table::chmatch(
      as.character(batiment_construction_id), eligible_construction_ids,
      nomatch = 0L
    ) > 0L,
    as.character(cle_interop_adr)
  ])
  addresses <- addresses[
    data.table::chmatch(
      as.character(cle_interop_adr), eligible_address_ids, nomatch = 0L
    ) > 0L
  ]
  points <- address_reroute_address_points_from_wkt(addresses)
  existing_path <- file.path(base_run_dir, "plan", "origin_points.parquet")
  if (!file.exists(existing_path)) {
    stop("completed run origin plan not found: ", existing_path, call. = FALSE)
  }
  existing <- arrow::read_parquet(existing_path)
  built <- build_address_reroute_universe(
    origins = origins,
    construction_addresses = relation,
    address_points = points,
    existing_points = existing
  )
  built$source <- list(
    base_run_dir = base_run_dir,
    bdnb_millesime = attr(origins, "bdnb_millesime"),
    bdnb_sha256 = attr(origins, "bdnb_sha256")
  )
  built
}

#' Persist the address universe and crosswalks used by the reroute.
write_address_reroute_plan <- function(inputs, reroute_root) {
  stopifnot(is.list(inputs), is.character(reroute_root), length(reroute_root) == 1L)
  plan_dir <- file.path(reroute_root, "plan")
  paths <- list(
    address_origins = file.path(plan_dir, "address_origins.parquet"),
    address_construction_link = file.path(
      plan_dir, "address_construction_link.parquet"
    ),
    new_origin_points = file.path(plan_dir, "new_origin_points.parquet"),
    new_origin_link = file.path(plan_dir, "new_origin_link.parquet")
  )
  write_parquet_atomic(inputs$addresses, paths$address_origins)
  write_parquet_atomic(inputs$address_construction_link,
                       paths$address_construction_link)
  write_parquet_atomic(inputs$new_origin_plan$points, paths$new_origin_points)
  write_parquet_atomic(inputs$new_origin_plan$link, paths$new_origin_link)
  paths
}

address_reroute_destinations_provider <- function(base_run_dir) {
  plan_dir <- file.path(base_run_dir, "plan")
  points <- data.table::as.data.table(arrow::read_parquet(
    file.path(plan_dir, "destination_points.parquet")
  ))
  link <- data.table::as.data.table(arrow::read_parquet(
    file.path(plan_dir, "destination_link.parquet")
  ))
  destinations <- merge(
    link, points[, .(point_id = id, lon, lat)], by = "point_id", all = FALSE
  )[, .(id, lon, lat)]
  list(
    destinations = destinations,
    dest_map = arrow::read_parquet(file.path(plan_dir, "destination_map.parquet")),
    registry = arrow::read_parquet(
      file.path(base_run_dir, "destination_registry.parquet")
    )
  )
}

find_network_dir_for_fingerprint <- function(data_dir, fingerprint) {
  markers <- list.files(file.path(data_dir, "acquired"),
                        pattern = "^\\.network-identity[.]json$",
                        recursive = TRUE, full.names = TRUE, all.files = TRUE,
                        include.dirs = FALSE)
  for (marker in markers) {
    found <- tryCatch(jsonlite::fromJSON(marker, simplifyVector = FALSE),
                      error = function(e) NULL)
    if (!is.null(found) && identical(as.character(found$fingerprint),
                                    as.character(fingerprint))) {
      return(dirname(marker))
    }
  }
  stop("no network cache matches the completed run fingerprint: ", fingerprint,
       call. = FALSE)
}

find_network_dir_for_osm_pin <- function(data_dir, osm_id) {
  roots <- c(file.path(data_dir, "networks"),
             file.path(data_dir, "acquired", "osm"))
  dirs <- unique(unlist(lapply(roots, function(root) {
    if (!dir.exists(root)) return(character(0))
    list.dirs(root, recursive = FALSE, full.names = TRUE)
  })))
  for (dir in dirs) {
    settings_path <- file.path(dir, "network_settings.json")
    if (!file.exists(settings_path)) next
    settings <- paste(readLines(settings_path, warn = FALSE), collapse = "")
    hit <- regexec('"pbf_file_name":"([^"]+)"', settings, perl = TRUE)
    match <- regmatches(settings, hit)[[1L]]
    if (length(match) < 2L) next
    pbf_name <- basename(gsub("\\\\", "/", match[[2L]]))
    if (identical(pbf_name, as.character(osm_id))) return(dir)
  }
  stop("no built network directory contains the completed run OSM pin: ",
       osm_id, call. = FALSE)
}

find_network_dir_for_base_manifest <- function(data_dir, base_manifest) {
  expected <- base_manifest$identity$network_fingerprint
  roots <- c(file.path(data_dir, "networks"),
             file.path(data_dir, "acquired", "osm"))
  candidates <- unique(unlist(lapply(roots, function(root) {
    if (!dir.exists(root)) return(character(0))
    list.dirs(root, recursive = TRUE, full.names = TRUE)
  })))
  for (dir in candidates) {
    summary_path <- file.path(dir, "build-summary.json")
    if (!file.exists(summary_path)) next
    summary <- tryCatch(
      jsonlite::fromJSON(summary_path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(summary) &&
        identical(as.character(summary$identity$fingerprint),
                  as.character(expected))) {
      return(dir)
    }
  }
  find_network_dir_for_osm_pin(
    data_dir, base_manifest$identity$network_identity_components$osm_pin$id
  )
}

network_identity_for_address_reroute <- function(base_manifest, network_dir,
                                                 data_dir = "data") {
  components <- base_manifest$identity$network_identity_components
  if (is.null(components) || is.null(components$osm_pin)) {
    stop("completed run manifest lacks the network identity components needed",
         call. = FALSE)
  }
  summary_path <- file.path(network_dir, "build-summary.json")
  if (!file.exists(summary_path)) {
    stop("network cache has no build summary; refusing to infer base-run identity: ",
         network_dir, call. = FALSE)
  }
  summary <- tryCatch(
    jsonlite::fromJSON(summary_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(summary) ||
      !identical(as.character(summary$identity$fingerprint),
                 as.character(base_manifest$identity$network_fingerprint))) {
    stop("network cache build summary does not match the completed base run: ",
         network_dir, call. = FALSE)
  }
  settings_path <- file.path(network_dir, "network_settings.json")
  if (!file.exists(settings_path)) {
    stop("network settings not found: ", settings_path, call. = FALSE)
  }
  settings_text <- paste(readLines(settings_path, warn = FALSE), collapse = "")
  read_setting <- function(name) {
    hit <- regexec(sprintf('"%s":"([^"]+)"', name), settings_text,
                   perl = TRUE)
    match <- regmatches(settings_text, hit)[[1L]]
    if (length(match) < 2L) NULL else match[[2L]]
  }
  settings <- list(
    pbf_file_name = read_setting("pbf_file_name"),
    r5r_version = read_setting("r5r_version"),
    r5_version = read_setting("r5_version")
  )
  if (is.null(settings$r5r_version) || is.null(settings$r5_version) ||
      is.null(settings$pbf_file_name)) {
    stop("network settings do not record r5r/R5 versions: ", settings_path,
         call. = FALSE)
  }
  pbf_name <- basename(gsub("\\\\", "/",
                            as.character(settings$pbf_file_name)))
  pbf_path <- file.path(network_dir, pbf_name)
  if (!file.exists(pbf_path)) {
    stop("network settings point to a missing OSM PBF: ", pbf_path,
         call. = FALSE)
  }
  observed_osm_sha <- sha256_file(pbf_path)
  if (!identical(observed_osm_sha,
                 as.character(components$osm_pin$sha256))) {
    stop("network OSM PBF sha256 does not match the completed run pin",
         call. = FALSE)
  }
  lines <- as.character(components$transit_lines)
  transit_pins <- lapply(lines, function(line) {
    value <- sub("^feed:", "", line)
    pieces <- strsplit(value, "=", fixed = TRUE)[[1L]]
    if (length(pieces) != 2L) {
      stop("invalid transit identity line: ", line, call. = FALSE)
    }
    list(id = pieces[[1L]], sha256 = pieces[[2L]])
  })
  identity <- network_cache_identity(
    osm_pin = components$osm_pin,
    transit_pins = transit_pins,
    elevation_pin = if (is.null(components$elevation_pin)) "NONE" else
      components$elevation_pin,
    # The build summary and completed manifest are the provenance boundary
    # for this cache. network_settings.json is emitted by the Java builder and
    # may report its embedded engine independently of r5r's dependency pin.
    versions = list(r5r = as.character(components$r5r_version),
                    r5 = as.character(components$r5_version)),
    W = as.numeric(components$W_m),
    cap = as.numeric(components$cap_minutes)
  )
  if (!identical(as.character(identity$fingerprint),
                 as.character(base_manifest$identity$network_fingerprint))) {
    stop("network cache inputs do not reproduce the completed base run fingerprint",
         call. = FALSE)
  }
  if (!file.exists(file.path(network_dir, "network.dat"))) {
    stop("network cache is missing network.dat: ", network_dir, call. = FALSE)
  }
  marker <- network_identity_marker_path(network_dir)
  found <- if (file.exists(marker)) {
    tryCatch(jsonlite::fromJSON(marker, simplifyVector = FALSE),
             error = function(e) NULL)
  } else NULL
  if (is.null(found) ||
      !identical(as.character(found$fingerprint), identity$fingerprint)) {
    # The marker is derived metadata, and the previous interrupted attempt
    # wrote a different fingerprint. Repair it only after the immutable PBF,
    # build summary, network.dat, and base identity have all been verified.
    commit_network_cache(network_dir, identity)
  }
  identity
}

address_reroute_canonical_table <- function(x) {
  x <- data.table::as.data.table(x)
  columns <- sort(names(x), method = "radix")
  if (length(columns)) {
    data.table::setorderv(x, columns)
    x <- x[, ..columns]
  }
  canonical_vector <- function(value) {
    if (inherits(value, "POSIXt")) {
      return(vapply(seq_along(value), function(i) {
        if (is.na(value[[i]])) "<NA>" else format(value[[i]], tz = "UTC")
      }, character(1L)))
    }
    if (is.numeric(value)) {
      return(vapply(seq_along(value), function(i) {
        if (is.na(value[[i]])) "<NA>" else as.character(value[[i]])
      }, character(1L)))
    }
    if (is.logical(value)) {
      return(vapply(seq_along(value), function(i) {
        if (is.na(value[[i]])) "<NA>" else as.character(value[[i]])
      }, character(1L)))
    }
    if (is.list(value)) {
      return(vapply(value, function(item) {
        if (is.null(item)) "<NULL>" else paste(as.character(item), collapse = "\u001f")
      }, character(1L)))
    }
    vapply(seq_along(value), function(i) {
      if (is.na(value[[i]])) "<NA>" else as.character(value[[i]])
    }, character(1L))
  }
  stats::setNames(lapply(columns, function(column) {
    unname(canonical_vector(x[[column]]))
  }), columns)
}

address_reroute_plan_key_from_tables <- function(addresses,
                                                  address_construction_link,
                                                  new_origin_points,
                                                  new_origin_link) {
  table_key <- function(x) {
    columns <- address_reroute_canonical_table(x)
    column_keys <- vapply(names(columns), function(name) {
      digest::digest(
        paste(columns[[name]], collapse = "\u001e"),
        algo = "sha256", serialize = FALSE
      )
    }, character(1L))
    digest::digest(
      paste(names(column_keys), column_keys, sep = "=", collapse = "\u001f"),
      algo = "sha256", serialize = FALSE
    )
  }
  table_keys <- c(
    addresses = table_key(addresses),
    address_construction_link = table_key(address_construction_link),
    new_origin_points = table_key(new_origin_points),
    new_origin_link = table_key(new_origin_link)
  )
  digest::digest(
    paste(names(table_keys), table_keys, sep = "=", collapse = "\u001f"),
    algo = "sha256", serialize = FALSE
  )
}

address_reroute_plan_key <- function(inputs) {
  address_reroute_plan_key_from_tables(
    inputs$addresses,
    inputs$address_construction_link,
    inputs$new_origin_plan$points,
    inputs$new_origin_plan$link
  )
}

address_reroute_plan_key_from_files <- function(plan_paths) {
  address_reroute_plan_key_from_tables(
    arrow::read_parquet(plan_paths$address_origins),
    arrow::read_parquet(plan_paths$address_construction_link),
    arrow::read_parquet(plan_paths$new_origin_points),
    arrow::read_parquet(plan_paths$new_origin_link)
  )
}

address_reroute_code_fingerprint <- function(code_dir = resolve_code_dir(getwd())) {
  files <- sort(list.files(file.path(code_dir, "R"), pattern = "[.]R$",
                           full.names = TRUE))
  digest::digest(list(
    files = basename(files),
    contents = lapply(files, readLines, warn = FALSE)
  ), algo = "sha256")
}

#' Build the address universe and route only its genuinely new coordinates.
#'
#' This is the production entry point. It resumes through the ordinary
#' one-child-at-a-time runner, while placing each mode's artifacts under its
#' own `chunks/reroutes/<mode>/` directory.
run_address_reroutes <- function(base_run_label = "full-bretagne-2026-09-04",
                                 data_dir = "data",
                                 manifest_path = file.path(data_dir, "manifest.json"),
                                 out_dir = file.path(data_dir, "matrice"),
                                 network_dir = NULL,
                                 chunk_size = 12500L,
                                 heap = "-Xmx12G",
                                 n_threads = 4L,
                                 use_cache = TRUE,
                                 verbose = TRUE) {
  base_run_dir <- matrice_run_dir(out_dir, base_run_label)
  base_manifest <- load_run_manifest(file.path(base_run_dir, "manifest.json"))
  reroute_root <- address_reroute_root(base_run_dir)
  inputs <- read_address_reroute_universe(
    base_run_dir = base_run_dir, data_dir = data_dir,
    manifest_path = manifest_path, use_cache = use_cache
  )
  reroute_manifest_path <- file.path(reroute_root, "manifest.json")
  plan_paths <- list(
    address_origins = file.path(reroute_root, "plan", "address_origins.parquet"),
    address_construction_link = file.path(
      reroute_root, "plan", "address_construction_link.parquet"
    ),
    new_origin_points = file.path(reroute_root, "plan", "new_origin_points.parquet"),
    new_origin_link = file.path(reroute_root, "plan", "new_origin_link.parquet")
  )
  # Never overwrite the persisted crosswalk before the resumable runner has
  # compared its identity. A changed crosswalk must refuse resume, not mix
  # new metadata with old matrix chunks.
  if (!file.exists(reroute_manifest_path)) {
    write_address_reroute_plan(inputs, reroute_root)
  } else if (!all(file.exists(unlist(plan_paths)))) {
    stop("reroute manifest exists but its address plan is incomplete; refusing resume",
         call. = FALSE)
  } else if (!identical(address_reroute_plan_key(inputs),
                        address_reroute_plan_key_from_files(plan_paths))) {
    stop("persisted address reroute plan differs from the current BDNB inputs; refusing resume",
         call. = FALSE)
  }
  if (isTRUE(verbose)) {
    message(sprintf(
      "address reroutes: %d eligible addresses; %d new address identities; %d new coordinates",
      inputs$counts$n_eligible_addresses,
      inputs$counts$n_new_address_identities,
      inputs$counts$n_new_coordinates
    ))
  }

  components <- base_manifest$identity$network_identity_components
  if (is.null(components)) {
    stop("completed run manifest carries no network identity components",
         call. = FALSE)
  }
  if (is.null(network_dir)) {
    network_dir <- find_network_dir_for_base_manifest(data_dir, base_manifest)
  }
  network_identity <- network_identity_for_address_reroute(
    base_manifest, network_dir, data_dir = data_dir
  )
  rt <- base_manifest$identity$routing_parameters
  departure <- if (is.null(rt$departure_datetime)) NULL else as.POSIXct(
    as.character(rt$departure_datetime), format = "%Y-%m-%dT%H:%M:%S%z",
    tz = "UTC"
  )
  modes <- atomic_modes()
  artifact_dirs <- stats::setNames(file.path(reroute_root, modes), modes)
  identity_extra <- list(
    base_run_label = base_run_label,
    bdnb_millesime = inputs$source$bdnb_millesime,
    bdnb_sha256 = inputs$source$bdnb_sha256,
    address_reroute_plan = address_reroute_plan_key(inputs),
    artifact_layout = modes
  )
  out <- run_resumable(
    run_label = "reroutes",
    modes = modes,
    chunk_size = chunk_size,
    W = as.numeric(components$W_m),
    walk_speed = as.numeric(rt$walk_speed),
    bike_speed = as.numeric(rt$bike_speed),
    max_trip_duration = as.numeric(rt$max_trip_duration),
    elevation = as.character(rt$elevation),
    departure_datetime = departure,
    time_window = as.integer(rt$time_window),
    percentiles = as.integer(rt$percentiles),
    transit_service_date = as.character(rt$service_date),
    transit_required_ids = as.character(rt$transit_required_ids),
    transit_activity_window = rt$feed_activity_window,
    max_rides = as.integer(rt$max_rides),
    draws_per_minute = as.integer(rt$draws_per_minute),
    network_identity = network_identity,
    network_dir = network_dir,
    heap = heap,
    n_threads = n_threads,
    data_dir = data_dir,
    manifest_path = manifest_path,
    use_cache = use_cache,
    out_dir = file.path(out_dir, gsub("[^A-Za-z0-9_.-]+", "-", base_run_label),
                        "chunks"),
    artifacts_dirs = artifact_dirs,
    identity_extra = identity_extra,
    origins_provider = function() inputs$new_origins,
    destinations_provider = function() address_reroute_destinations_provider(
      base_run_dir
    ),
    git_sha = address_reroute_code_fingerprint(),
    code_dir = resolve_code_dir(getwd()),
    verbose = verbose
  )
  out$address_plan <- plan_paths
  out$address_counts <- inputs$counts
  out
}
