#!/usr/bin/env Rscript

# Check the machine-local acquisition inputs used by the canonical readers.
# Acquisition itself is intentionally outside this public, lean surface.

source(file.path("scripts", "00-config.R"))
be_source("inputs")

be_require_dir(be_config$data_dir, "BE_DATA_DIR")
be_require_file(be_config$manifest_path, "BE_MANIFEST_PATH")
manifest <- manifest_load(be_config$manifest_path)

if (is.null(manifest$sources) || !length(manifest$sources)) {
  stop("the local acquisition manifest contains no sources", call. = FALSE)
}

be_config_summary()
message("local acquisition manifest: ", length(manifest$sources), " sources")
message("inputs ready")
