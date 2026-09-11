# Shared configuration for the public reproduction entrypoints.
#
# The public surface deliberately has no data or acquisition manifest. Put
# those machine-local inputs in .env (which is ignored) and run scripts from
# the repository root.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!dir.exists(file.path(repo_root, "code", "R"))) {
  stop("Run public scripts from the repository root: ", repo_root,
       call. = FALSE)
}

dotenv_path <- file.path(repo_root, ".env")
if (!file.exists(dotenv_path)) {
  stop("Missing local .env. Copy .env.example and set paths to acquired inputs.",
       call. = FALSE)
}
readRenviron(dotenv_path)

env_value <- function(name, default = NULL, required = FALSE) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) value <- default
  if (isTRUE(required) && (is.null(value) || !nzchar(value))) {
    stop("Missing required .env value: ", name, call. = FALSE)
  }
  value
}

path_value <- function(name, default = NULL, required = FALSE) {
  value <- env_value(name, default = default, required = required)
  if (is.null(value)) return(NULL)
  normalizePath(value, winslash = "/", mustWork = FALSE)
}

integer_value <- function(name, default) {
  value <- env_value(name, default = as.character(default))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  parsed
}

logical_value <- function(name, default = FALSE) {
  value <- tolower(env_value(name, default = if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true/false", call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

csv_value <- function(name, default) {
  value <- env_value(name, default = default)
  out <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  out[nzchar(out)]
}

be_require_dir <- function(path, label = "directory") {
  if (!dir.exists(path)) stop(label, " does not exist: ", path, call. = FALSE)
  invisible(path)
}

be_require_file <- function(path, label = "file") {
  if (!file.exists(path)) stop(label, " does not exist: ", path, call. = FALSE)
  invisible(path)
}

be_parse_departure <- function(value) {
  if (is.null(value) || !nzchar(value)) return(NULL)
  parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  if (is.na(parsed)) {
    stop("BE_DEPARTURE_DATETIME must use YYYY-MM-DDTHH:MM:SS+0000",
         call. = FALSE)
  }
  parsed
}

be_config <- list(
  repo_root = repo_root,
  data_dir = path_value("BE_DATA_DIR", required = TRUE),
  manifest_path = path_value(
    "BE_MANIFEST_PATH",
    default = file.path(env_value("BE_DATA_DIR", required = TRUE), "manifest.json")
  ),
  output_dir = path_value(
    "BE_OUTPUT_DIR",
    default = file.path(env_value("BE_DATA_DIR", required = TRUE), "matrice")
  ),
  run_label = env_value("BE_RUN_LABEL", default = "full-bretagne-2026-09-04"),
  network_dir = path_value("BE_NETWORK_DIR", required = TRUE),
  network_pbf = path_value("BE_NETWORK_PBF"),
  network_identity_path = path_value(
    "BE_NETWORK_IDENTITY_PATH",
    default = file.path(
      env_value("BE_DATA_DIR", required = TRUE), "acquired", "osm",
      "public-network-identity.rds"
    )
  ),
  modes = csv_value("BE_MODES", "walk,transit,bike,car"),
  chunk_size = integer_value("BE_CHUNK_SIZE", 50000L),
  reroute_chunk_size = integer_value("BE_REROUTE_CHUNK_SIZE", 12500L),
  heap = env_value("BE_HEAP", "-Xmx24G"),
  reroute_heap = env_value("BE_REROUTE_HEAP", "-Xmx12G"),
  n_threads = integer_value("BE_N_THREADS", 4L),
  departure_datetime = be_parse_departure(
    env_value("BE_DEPARTURE_DATETIME", "2026-09-16T08:00:00+0000")
  ),
  transit_service_date = env_value("BE_TRANSIT_SERVICE_DATE", "2026-09-16"),
  elevation = env_value("BE_ELEVATION", "NONE"),
  java_heap = env_value("BE_JAVA_HEAP", "4g"),
  use_cache = logical_value("BE_USE_CACHE", TRUE),
  overwrite = logical_value("BE_OVERWRITE", FALSE),
  resume = logical_value("BE_RESUME", TRUE),
  verbose = logical_value("BE_VERBOSE", TRUE)
)

if (!all(be_config$modes %in% c("walk", "transit", "bike", "car"))) {
  stop("BE_MODES contains an unsupported atomic mode", call. = FALSE)
}

be_code_files <- list(
  inputs = c(
    "acquire.R", "constants.R", "durable-root.R", "metadata-portable.R",
    "read-admin-express.R", "read-bdnb.R", "read-bpe.R"
  ),
  network = c(
    "acquire.R", "acquire-osm.R", "cache-identity.R", "constants.R",
    "durable-root.R", "full-run-inputs.R", "link.R", "metadata-portable.R",
    "prepare-destinations.R", "read-admin-express.R", "read-bdnb.R",
    "read-bpe.R", "route-coordinates.R", "run-tracer.R",
    "run-chunk-worker.R", "run-resumable.R", "validate-matrix.R"
  ),
  address = c(
    "acquire.R", "acquire-osm.R", "address-monolith.R", "address-reroutes.R",
    "cache-identity.R", "constants.R", "durable-root.R", "full-run-inputs.R",
    "link.R", "metadata-portable.R", "prepare-destinations.R",
    "read-admin-express.R", "read-bdnb.R", "read-bpe.R", "route-coordinates.R",
    "run-tracer.R", "run-chunk-worker.R", "run-resumable.R",
    "transit-gains.R", "validate-matrix.R"
  ),
  territory = c(
    "acquire.R", "address-aggregates.R", "address-territories.R", "constants.R",
    "durable-root.R", "metadata-portable.R", "read-admin-express.R",
    "read-bdnb.R", "read-bpe.R", "run-resumable.R"
  )
)

be_source <- function(stage) {
  files <- be_code_files[[stage]]
  if (is.null(files)) stop("Unknown public script stage: ", stage, call. = FALSE)
  for (name in unique(files)) {
    source(file.path(repo_root, "code", "R", name), local = .GlobalEnv)
  }
  invisible(files)
}

be_run_dir <- function(label = be_config$run_label) {
  file.path(be_config$output_dir, gsub("[^A-Za-z0-9_.-]+", "-", label))
}

be_config_summary <- function() {
  message("data_dir: ", be_config$data_dir)
  message("manifest: ", be_config$manifest_path)
  message("output_dir: ", be_config$output_dir)
  message("run_label: ", be_config$run_label)
  message("network_dir: ", be_config$network_dir)
  message("modes: ", paste(be_config$modes, collapse = ", "))
}
