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
territory_names <- read_admin_express_territory_names(
  data_dir = data_dir,
  manifest_path = file.path(data_dir, "manifest.json")
)
cog[, epci_code := as.character(epci)]
cog[epci_code == "NR", epci_code := NA_character_]
cog[, epci := NULL]
cog[territory_names$epci,
    nom_epci := i.nom_epci,
    on = .(epci_code = epci_code)]
cog[territory_names$departement,
    nom_departement := i.nom_departement,
    on = .(code_departement = code_departement)]
cog[territory_names$region,
    nom_region := i.nom_region,
    on = .(code_region = code_region)]
if (anyNA(cog[["nom_departement"]]) || anyNA(cog[["nom_region"]])) {
  stop("ADMIN EXPRESS territory names do not cover the COG commune axis",
       call. = FALSE)
}
if (any(!is.na(cog[["epci_code"]]) & is.na(cog[["nom_epci"]]))) {
  stop("ADMIN EXPRESS territory names do not cover the assigned EPCI axis",
       call. = FALSE)
}
uncovered_communes <- setdiff(
  as.character(cog[["code_insee"]]),
  unique(as.character(territory_crosswalk[["code_insee"]]))
)
expected_uncovered_communes <- c("29083", "29084")
if (!setequal(uncovered_communes, expected_uncovered_communes)) {
  stop(
    "unexpected uncovered commune set; review the coverage decision before continuing: ",
    paste(sort(uncovered_communes), collapse = ", "),
    call. = FALSE
  )
}
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
type_nomenclature <- read_bpe_type_nomenclature(data_dir = data_dir)
type_catalogue <- merge(
  type_catalogue, type_nomenclature, by = "TYPEQU", all.x = TRUE, sort = FALSE
)
if (anyNA(type_catalogue[["nom_typequ"]])) {
  stop("BPE type catalogue has TYPEQU values without official labels",
       call. = FALSE)
}
data.table::setorderv(type_catalogue, "TYPEQU")
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

dataset <- arrow::open_dataset(
  file.path(run_dir, "address-accessibility.parquet"), format = "parquet"
)
schema_names <- names(dataset[["schema"]])
metric_columns <- c(
  grep("^count_(5|10|15|20)_(walk|bike|car)$",
       schema_names, value = TRUE),
  grep("^transit_gain_count_(5|10|15|20)_p1$",
       schema_names, value = TRUE)
)
metric_columns <- unique(metric_columns)
if (length(metric_columns) != 16L) {
  stop("expected 16 territory count metrics, found ", length(metric_columns),
       call. = FALSE)
}
aggregates <- aggregate_address_type_metrics_arrow_levels(
  dataset = dataset,
  address_spine = address_spine,
  type_catalogue = type_catalogue,
  value_columns = metric_columns
)

rename_transitgain_columns <- function(x) {
  old <- grep("^transit_gain_count_(5|10|15|20)_p1_",
              names(x), value = TRUE)
  new <- sub(
    "^transit_gain_count_(5|10|15|20)_p1_(.*)$",
    "count_\\1_transitgain_\\2",
    old
  )
  data.table::setnames(x, old, new)
  x[]
}
aggregates <- lapply(aggregates, rename_transitgain_columns)

territory_levels <- address_aggregate_default_territory_levels()
territory_axes <- list(
  commune = cog[, .(code_insee, nom_commune, epci_code, nom_epci,
                   code_departement, nom_departement, code_region, nom_region)],
  epci = unique(cog[!is.na(epci_code),
                    .(epci_code, nom_epci, code_region, nom_region)]),
  departement = unique(cog[, .(code_departement, nom_departement,
                              code_region, nom_region)]),
  region = unique(cog[, .(code_region, nom_region)])
)
type_label_columns <- setdiff(names(type_catalogue), "TYPEQU")

complete_level_axis <- function(level, aggregate) {
  axis <- data.table::copy(territory_axes[[level]])
  aggregate_columns <- c(territory_levels[[level]], "TYPEQU")
  axis[, `.__aggregate_join_key` := 1L]
  axis_columns <- setdiff(names(axis), ".__aggregate_join_key")
  types <- data.table::copy(type_catalogue)
  types[, `.__aggregate_join_key` := 1L]
  grid <- merge(axis, types, by = ".__aggregate_join_key",
                allow.cartesian = TRUE, sort = FALSE)
  grid[, `.__aggregate_join_key` := NULL]
  out <- merge(
    grid, aggregate, by = aggregate_columns, all.x = TRUE, sort = FALSE
  )
  metric_names <- setdiff(
    names(aggregate), c(territory_levels[[level]], "TYPEQU",
                        "n_addresses", "n_observed")
  )
  out[is.na(n_addresses), n_addresses := 0L]
  out[is.na(n_observed), n_observed := 0L]
  out[, coverage_status := data.table::fifelse(
    n_addresses > 0L, "covered", "uncovered"
  )]
  if (level == "commune") {
    out[code_insee %in% expected_uncovered_communes & n_addresses == 0L,
        coverage_status := "uncovered_no_bdnb_residential_origins"]
  }
  out[n_addresses == 0L, (metric_names) := lapply(.SD, function(x) {
    rep(NA_real_, length(x))
  }), .SDcols = metric_names]
  data.table::setcolorder(out, c(
    axis_columns, "TYPEQU", type_label_columns, "coverage_status",
    "n_addresses", "n_observed", metric_names
  ))
  data.table::setorderv(out, c(axis_columns, "TYPEQU"))
  out[]
}

aggregates <- lapply(names(territory_axes), function(level) {
  complete_level_axis(level, aggregates[[level]])
})
names(aggregates) <- names(territory_axes)

output_paths <- character()
for (level in names(aggregates)) {
  path <- file.path(territory_dir, paste0(level, "_typequ.parquet"))
  write_parquet_atomic(aggregates[[level]], path)
  output_paths[[level]] <- path
}

denominator <- address_spine[,
  .(n_addresses = .N), by = .(code_insee, code_departement, nom_commune,
                              code_region, epci_code)
][, sum(n_addresses)]
aggregate_metric_columns <- setdiff(
  names(aggregates[["commune"]]),
  c(names(territory_axes[["commune"]]), "TYPEQU", type_label_columns,
    "coverage_status", "n_addresses", "n_observed")
)
metadata <- list(
  source_monolith = file.path(run_dir, "address-accessibility.parquet"),
  source_type_catalogue = "BPE 2025 universe",
  source_type_nomenclature = bpe_type_nomenclature_url(),
  source_territory_names = "ADMIN EXPRESS COG 2025-01-01",
  address_denominator = denominator,
  n_addresses = nrow(address_spine),
  n_communes = nrow(cog),
  n_types = nrow(type_catalogue),
  value_columns = metric_columns,
  output_metric_columns = aggregate_metric_columns,
  conflict_resolution = "address_id_prefix (only for detected multi-commune links)",
  territory_levels = names(aggregates),
  output_paths = unname(output_paths),
  output_rows = vapply(aggregates, nrow, integer(1L)),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
atomic_write_json(metadata, file.path(territory_dir, "aggregate-metadata.json"))
message("Wrote address territory aggregates under ", territory_dir)
