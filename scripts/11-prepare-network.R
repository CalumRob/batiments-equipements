#!/usr/bin/env Rscript

# Build or reuse the dedicated r5r network directory and persist the exact
# identity consumed by 20-run-once.R. This is the only public entrypoint that
# may pay the network-build cost.

source(file.path("scripts", "00-config.R"))
be_source("network")

be_require_dir(be_config$data_dir, "BE_DATA_DIR")
be_require_file(be_config$manifest_path, "BE_MANIFEST_PATH")
dir.create(be_config$network_dir, recursive = TRUE, showWarnings = FALSE)

network_pbf <- be_config$network_pbf
if (is.null(network_pbf)) {
  network_pbf <- read_osm_network(
    data_dir = be_config$data_dir,
    manifest_path = be_config$manifest_path,
    use_cache = be_config$use_cache,
    java_heap = be_config$java_heap
  )
}
be_require_file(network_pbf, "network PBF")
if (!identical(toupper(be_config$elevation), "NONE")) {
  stop("the public network entrypoint currently supports BE_ELEVATION=NONE only",
       call. = FALSE)
}

target_pbf <- file.path(be_config$network_dir, basename(network_pbf))
network_pbf_sha256 <- sha256_file(network_pbf)
if (!file.exists(target_pbf) ||
    !identical(sha256_file(target_pbf), network_pbf_sha256)) {
  message("copying network PBF into ", be_config$network_dir)
  if (!file.copy(network_pbf, target_pbf, overwrite = TRUE)) {
    stop("could not copy network PBF into ", be_config$network_dir, call. = FALSE)
  }
}

# setup_r5 scans every PBF in data_path. Keep this directory dedicated to the
# selected network, even if a previous run used a different extract.
other_pbf <- setdiff(
  list.files(be_config$network_dir, pattern = "[.]osm[.]pbf$",
             full.names = TRUE, ignore.case = TRUE),
  target_pbf
)
if (length(other_pbf)) unlink(other_pbf, force = TRUE)

transit_staged <- NULL
if ("transit" %in% be_config$modes) {
  transit_staged <- stage_transit_feeds(
    network_dir = be_config$network_dir,
    data_dir = be_config$data_dir,
    manifest_path = be_config$manifest_path,
    regime = "current",
    service_date = be_config$transit_service_date,
    required_ids = full_run_transit_required_ids(),
    activity_window = full_run_transit_activity_window()
  )
}

network_identity <- network_cache_identity(
  osm_pin = list(id = basename(target_pbf), sha256 = network_pbf_sha256),
  transit_pins = if (is.null(transit_staged)) list() else transit_staged$feeds,
  elevation_pin = "NONE",
  W = border_width_m(),
  cap = cap_minutes()
)
cache_probe <- probe_network_cache(be_config$network_dir, network_identity)
if (isTRUE(cache_probe$cache_hit) &&
    !file.exists(file.path(be_config$network_dir, "network.dat"))) {
  cache_probe$cache_hit <- FALSE
  cache_probe$reason <- "identity marker matches but network.dat is absent"
}

if (!isTRUE(cache_probe$cache_hit)) {
  message("network cache miss: ", cache_probe$reason)
  link_network(
    data_path = be_config$network_dir,
    elevation = be_config$elevation,
    verbose = be_config$verbose,
    overwrite = TRUE
  )
} else {
  message("network cache hit: ", network_identity$fingerprint)
}

commit_network_cache(be_config$network_dir, network_identity)
dir.create(dirname(be_config$network_identity_path), recursive = TRUE,
           showWarnings = FALSE)
saveRDS(network_identity, be_config$network_identity_path)

message("network identity written to ", be_config$network_identity_path)
