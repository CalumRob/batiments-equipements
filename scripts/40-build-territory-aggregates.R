#!/usr/bin/env Rscript

# Publish the address-weighted commune/EPCI/department/region tables by
# delegating to the canonical derivation script. Keeping that implementation
# in code/scripts avoids maintaining two copies of the aggregation contract.

source(file.path("scripts", "00-config.R"))
be_require_dir(be_config$data_dir, "BE_DATA_DIR")
be_require_file(be_config$manifest_path, "BE_MANIFEST_PATH")
be_require_file(file.path(be_run_dir(), "address-accessibility.parquet"),
               "address accessibility monolith")

canonical <- file.path(be_config$repo_root, "code", "scripts",
                       "build-address-territory-aggregates.R")
be_require_file(canonical, "canonical territory aggregate script")

if (!identical(
    normalizePath(be_config$manifest_path, winslash = "/", mustWork = TRUE),
    normalizePath(file.path(be_config$data_dir, "manifest.json"),
                  winslash = "/", mustWork = FALSE)
  )) {
  stop("40-build-territory-aggregates.R requires BE_MANIFEST_PATH to be "
       , "BE_DATA_DIR/manifest.json because the canonical publisher uses that "
       , "local manifest path", call. = FALSE)
}

rscript <- Sys.which("Rscript")
if (!nzchar(rscript)) stop("Rscript is not on PATH", call. = FALSE)
# The canonical publisher is a separate R process because it is an existing
# command-line script. Prefer the active renv when it has the declared
# dependencies; otherwise --vanilla preserves the repository's documented
# local-library workflow without attempting an implicit restore.
rscript_options <- if (
  grepl("[/\\\\]renv[/\\\\]library[/\\\\]", .libPaths()[[1L]]) &&
  requireNamespace("dplyr", quietly = TRUE)
) {
  character()
} else {
  message("dplyr is unavailable in the active library; invoking publisher with --vanilla")
  "--vanilla"
}
status <- system2(
  rscript,
  c(rscript_options, shQuote(canonical), shQuote(be_config$data_dir),
    shQuote(be_run_dir()))
)
if (!identical(as.integer(status), 0L)) {
  stop("canonical territory aggregate script exited with status ", status,
       call. = FALSE)
}
