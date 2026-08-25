# 22f-classify-partials.R — Phase F of the #22 gate: classify the existing
# partial artifacts in data/matrice/ as DIAGNOSTIC (the 30-minute attempt
# era). Checksums + validator evidence; a written classification register;
# NOTHING moved or deleted (durable-root discipline).
#
# Run from the worktree root:
#   Rscript .scratch/tracer-matrice/research/22f-classify-partials.R

source(".scratch/tracer-matrice/research/22-common.R")

cat("== #22 phase F: partial-artifact classification ==\n")

targets <- c(
  "car_1.parquet",
  "walk_1.parquet",
  "destination_registry.parquet",
  "experiment_geom_car_1.parquet",
  "experiment_key2024_car_1.parquet",
  "experiment_key2025_car_1.parquet",
  "experiment_vintage_car_1.parquet",
  "corrected_ticket8/car_1.parquet",
  "corrected_ticket8/walk_1.parquet",
  "corrected_ticket8/destination_registry.parquet"
)

rows <- list()
for (rel in targets) {
  p <- file.path(DATA, "matrice", rel)
  if (!file.exists(p)) {
    rows[[length(rows) + 1L]] <- data.table::data.table(
      path = rel, status = "ABSENT", bytes = NA_real_, sha256 = NA_character_,
      n_rows = NA_integer_, n_cols = NA_integer_, modes = "",
      ladder_cols = "", max_tt_nearest = NA_real_,
      validate_matrix = "absent", classification = "ABSENT",
      note = "not present on disk")
    next
  }
  info <- file.info(p)
  sha <- sha256_file(p)
  is_matrix <- !grepl("destination_registry", rel, fixed = TRUE)
  if (is_matrix) {
    m <- read_matrix(p)
    vm <- tryCatch({ validate_matrix(m); "PASS" },
                   error = function(e) paste("FAIL:", conditionMessage(e)))
    first_violation <- sub("^invalid matrix:\n- ", "", vm)
    first_violation <- substr(first_violation[[1L]], 1L, 240L)
    ladder <- paste(sort(grep("^count_", names(m), value = TRUE)), collapse = ",")
    max_tt <- if ("tt_nearest" %in% names(m)) suppressWarnings(max(m[["tt_nearest"]], na.rm = TRUE)) else NA_real_
    modes <- paste(sort(unique(as.character(m[["mode"]]))), collapse = ",")
    cls <- if (startsWith(vm, "FAIL")) "DIAGNOSTIC" else "UNEXPECTED-PASS"
    rows[[length(rows) + 1L]] <- data.table::data.table(
      path = rel, status = "present", bytes = info[["size"]], sha256 = sha,
      n_rows = nrow(m), n_cols = ncol(m), modes = modes,
      ladder_cols = ladder, max_tt_nearest = max_tt,
      validate_matrix = substr(vm, 1L, 400L), classification = cls,
      note = first_violation)
  } else {
    rows[[length(rows) + 1L]] <- data.table::data.table(
      path = rel, status = "present", bytes = info[["size"]], sha256 = sha,
      n_rows = NA_integer_, n_cols = NA_integer_, modes = "(registry sidecar)",
      ladder_cols = "", max_tt_nearest = NA_real_,
      validate_matrix = "not-a-matrix (sidecar)", classification = "DIAGNOSTIC",
      note = "sidecar of the 30-minute attempt era; superseded by per-run registries")
  }
  cat("classified:", rel, "\n")
}
tbl <- data.table::rbindlist(rows)

register <- list(
  kind = "matrice-partial-artifact-classification",
  version = 1L,
  recorded_at = utc_stamp(),
  ticket = "22 — cap-20 performance and partial-artifact gate",
  verdict = paste(
    "All listed artifacts predate the cap-20 contract (#17): they are outputs",
    "of the 30-minute attempt era and FAIL validate_matrix by contract",
    "(off-ladder count_30 column; tt_nearest readings above the 20-minute",
    "cap). None is resumable by the new manifest (no manifest exists for",
    "them) and NONE is promoted: they remain on disk untouched, referenced",
    "only by this register."),
  action = "checksummed and marked DIAGNOSTIC; nothing moved or deleted",
  cap_minutes_now = cap_minutes(),
  ladder_rungs_now = unname(ladder_rungs()),
  artifacts = lapply(seq_len(nrow(tbl)), function(i) as.list(tbl[i]))
)
write_json_atomic_local(register, file.path(DATA, "matrice",
                                            "DIAGNOSTIC-register.json"))

data.table::fwrite(tbl, file.path(OUTPUTS, "22-diagnostic-classification.csv"))
data.table::fwrite(tbl, file.path(DATA, "matrice", "DIAGNOSTIC-register.csv"))
write_json_atomic_local(register, file.path(OUTPUTS, "22-diagnostic-register.json"))
cat("register written: data/matrice/DIAGNOSTIC-register.json\n")
print(tbl[, .(path, classification, n_rows, max_tt_nearest)])
cat("phase F complete\n")
