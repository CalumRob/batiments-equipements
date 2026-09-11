#!/usr/bin/env Rscript

# Run the resumable coordinate-level matrix computation.

source(file.path("scripts", "00-config.R"))
be_source("network")

be_require_file(be_config$manifest_path, "BE_MANIFEST_PATH")
be_require_file(be_config$network_identity_path, "BE_NETWORK_IDENTITY_PATH")
network_identity <- readRDS(be_config$network_identity_path)

if (!is.list(network_identity) || is.null(network_identity$fingerprint)) {
  stop("network identity RDS is not a network_cache_identity result",
       call. = FALSE)
}

be_config_summary()
run_resumable(
  run_label = be_config$run_label,
  modes = be_config$modes,
  chunk_size = be_config$chunk_size,
  departure_datetime = be_config$departure_datetime,
  transit_service_date = be_config$transit_service_date,
  network_identity = network_identity,
  network_dir = be_config$network_dir,
  heap = be_config$heap,
  n_threads = be_config$n_threads,
  data_dir = be_config$data_dir,
  manifest_path = be_config$manifest_path,
  use_cache = be_config$use_cache,
  scope = "bretagne",
  out_dir = be_config$output_dir,
  git_sha = current_git_sha(),
  code_dir = file.path(be_config$repo_root, "code"),
  verbose = be_config$verbose
)
