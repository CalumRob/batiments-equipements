#!/usr/bin/env Rscript

# Assemble the persisted base and reroute chunks into the address-level
# accessibility monolith.

source(file.path("scripts", "00-config.R"))
be_source("address")

base_run_dir <- be_run_dir()
be_require_dir(base_run_dir, "base run")
reroute_root <- address_reroute_root(base_run_dir)
be_require_dir(reroute_root, "address reroute root")

output_path <- file.path(
  base_run_dir,
  env_value("BE_ADDRESS_MONOLITH_PATH",
            default = "address-accessibility.parquet")
)
if (!grepl("^[A-Za-z]:[/\\\\]", output_path) &&
    !startsWith(output_path, "/")) {
  output_path <- file.path(base_run_dir, output_path)
}

build_address_accessibility_monolith(
  base_run_dir = base_run_dir,
  output_path = output_path,
  reroute_root = reroute_root,
  verbose = be_config$verbose,
  resume = be_config$resume
)
message("address monolith written to ", output_path)
