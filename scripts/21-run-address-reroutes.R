#!/usr/bin/env Rscript

# Route only the genuinely new address coordinates and persist the reroute
# crosswalk under the base run.

source(file.path("scripts", "00-config.R"))
be_source("address")

be_require_file(be_config$manifest_path, "BE_MANIFEST_PATH")
be_require_file(file.path(be_run_dir(), "manifest.json"), "base run manifest")

be_config_summary()
run_address_reroutes(
  base_run_label = be_config$run_label,
  data_dir = be_config$data_dir,
  manifest_path = be_config$manifest_path,
  out_dir = be_config$output_dir,
  network_dir = be_config$network_dir,
  chunk_size = be_config$reroute_chunk_size,
  heap = be_config$reroute_heap,
  n_threads = be_config$n_threads,
  use_cache = be_config$use_cache,
  verbose = be_config$verbose
)
