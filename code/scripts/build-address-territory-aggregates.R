#!/usr/bin/env Rscript

# Build the address-weighted public geography tables from the persisted
# address accessibility monolith.  This is a derivation-only job: it never
# invokes routing and scans the monolith through Arrow histograms.

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args)) args[[1L]] else "data"
run_dir <- if (length(args) > 1L) {
  args[[2L]]
} else {
  file.path(data_dir, "matrice", "full-bretagne-2026-09-04")
}

source_dir <- file.path("code", "R")
for (path in sort(list.files(source_dir, pattern = "[.]R$",
                             full.names = TRUE))) source(path)

plan_dir <- file.path(run_dir, "chunks", "reroutes", "plan")
territory_dir <- file.path(run_dir, "territory")
dir.create(territory_dir, recursive = TRUE, showWarnings = FALSE)

origins <- read_bdnb_residential_universe(
  data_dir = data_dir,
  manifest_path = file.path(data_dir, "manifest.json"),
  use_cache = TRUE
)
address_origins <- data.table::as.data.table(
  arrow::read_parquet(file.path(plan_dir, "address_origins.parquet"))
)
address_link <- data.table::as.data.table(
  arrow::read_parquet(file.path(plan_dir, "address_construction_link.parquet"))
)
territory_crosswalk <- build_address_territory_crosswalk(
  address_origins[, .(address_id)], address_link, origins,
  conflict_resolution = "address_id_prefix"
)
cog <- read_admin_express_commune_crosswalk(
  data_dir = data_dir,
  manifest_path = file.path(data_dir, "manifest.json")
)
address_spine <- merge(
  address_origins, territory_crosswalk, by = "address_id", sort = FALSE
)
address_spine <- merge(
  address_spine, cog, by = c("code_insee", "code_departement"),
  all.x = FALSE, sort = FALSE
)
if (nrow(address_spine) != nrow(address_origins)) {
  stop("ADMIN EXPRESS join changed the address denominator", call. = FALSE)
}
if (anyDuplicated(address_spine[["address_id"]])) {
  stop("address spine contains duplicate address_id values", call. = FALSE)
}

bpe <- read_bpe_universe(
  data_dir = data_dir,
  manifest_path = file.path(data_dir, "manifest.json"),
  use_cache = TRUE
)
type_catalogue <- data.table::data.table(
  TYPEQU = sort(unique(as.character(bpe[["TYPEQU"]])))
)
destination_map <- arrow::read_parquet(file.path(plan_dir, "destination_map.parquet"))
if (!setequal(type_catalogue[["TYPEQU"]],
             unique(as.character(destination_map[["TYPEQU"]])))) {
  stop("BPE type catalogue and destination map have different TYPEQU axes",
       call. = FALSE)
}

address_spine_path <- file.path(territory_dir, "address_spine.parquet")
type_catalogue_path <- file.path(territory_dir, "type_catalogue.parquet")
write_parquet_atomic(address_spine, address_spine_path)
write_parquet_atomic(type_catalogue, type_catalogue_path)

metric_columns <- c(
  grep("^count_(5|10|15|20)_(walk|bike|car)$",
       names(arrow::open_dataset(
         file.path(run_dir, "address-accessibility.parquet"),
         format = "parquet"
       )[["schema"]]), value = TRUE),
  grep("^transit_gain_count_(5|10|15|20)_(p1|p50)$",
       names(arrow::open_dataset(
         file.path(run_dir, "address-accessibility.parquet"),
         format = "parquet"
       )[["schema"]]), value = TRUE)
)
metric_columns <- unique(metric_columns)
if (length(metric_columns) != 20L) {
  stop("expected 20 territory count metrics, found ", length(metric_columns),
       call. = FALSE)
}

dataset <- arrow::open_dataset(
  file.path(run_dir, "address-accessibility.parquet"), format = "parquet"
)
aggregates <- aggregate_address_type_metrics_arrow_levels(
  dataset = dataset,
  address_spine = address_spine,
  type_catalogue = type_catalogue,
  value_columns = metric_columns
)

output_paths <- character()
for (level in names(aggregates)) {
  path <- file.path(territory_dir, paste0(level, "_typequ.parquet"))
  write_parquet_atomic(aggregates[[level]], path)
  output_paths[[level]] <- path
}

denominator <- address_spine[,
  .(n_addresses = .N), by = .(code_insee, code_departement, nom_commune,
                              code_region, epci)
][, sum(n_addresses)]
metadata <- list(
  source_monolith = file.path(run_dir, "address-accessibility.parquet"),
  source_type_catalogue = "BPE 2025 universe",
  address_denominator = denominator,
  n_addresses = nrow(address_spine),
  n_communes = data.table::uniqueN(address_spine[["code_insee"]]),
  n_types = nrow(type_catalogue),
  value_columns = metric_columns,
  conflict_resolution = "address_id_prefix (only for detected multi-commune links)",
  territory_levels = names(aggregates),
  output_paths = unname(output_paths),
  output_rows = vapply(aggregates, nrow, integer(1L)),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
atomic_write_json(metadata, file.path(territory_dir, "aggregate-metadata.json"))
message("Wrote address territory aggregates under ", territory_dir)
