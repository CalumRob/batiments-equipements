#!/usr/bin/env Rscript

# Derive transit-gain chunks from persisted walk/transit parquet artifacts.
# No routing network is opened here.

source(file.path("scripts", "00-config.R"))
be_source("address")

base_run_dir <- be_run_dir()
reroute_root <- address_reroute_root(base_run_dir)
be_require_dir(base_run_dir, "base run")
be_require_dir(reroute_root, "address reroute root")

walk_dir <- file.path(base_run_dir, "chunks", "walk")
transit_dir <- file.path(base_run_dir, "chunks", "transit")
gain_dir <- file.path(base_run_dir, "chunks", "transit_gain")
walk_files <- list.files(walk_dir, pattern = "^walk_[0-9]+[.]parquet$",
                         full.names = TRUE)
transit_files <- list.files(transit_dir, pattern = "^transit_[0-9]+[.]parquet$",
                            full.names = TRUE)
walk_ids <- as.integer(sub("^walk_([0-9]+)[.]parquet$", "\\1",
                           basename(walk_files)))
transit_ids <- as.integer(sub("^transit_([0-9]+)[.]parquet$", "\\1",
                              basename(transit_files)))
chunk_ids <- sort(intersect(walk_ids, transit_ids))
if (!length(chunk_ids)) {
  stop("no matching walk/transit chunks found under ", base_run_dir,
       call. = FALSE)
}

for (chunk_id in chunk_ids) {
  derive_transit_gain_chunk(
    transit_path = file.path(transit_dir, sprintf("transit_%d.parquet", chunk_id)),
    walk_path = file.path(walk_dir, sprintf("walk_%d.parquet", chunk_id)),
    chunk_id = chunk_id,
    out_dir = gain_dir,
    overwrite = be_config$overwrite
  )
}

derive_reroute_transit_gains(
  reroute_root = reroute_root,
  overwrite = be_config$overwrite,
  verbose = be_config$verbose
)
message("transit gains complete: ", length(chunk_ids), " base chunks")
