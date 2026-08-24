library(testthat)
library(data.table)

# The durable run manifest (#21): ONE file at data/matrice/<run_label>/
# manifest.json, owned by the orchestrator alone (single writer), carrying an
# identity block, the frozen plan census, and ONE entry per (mode x chunk).
# Every update is atomic (write-temp + rename); all-complete is DERIVED from
# the entries — there is no aggregate run-complete flag. The resume unit is
# the chunk (CONTEXT.md Core): "batch" is retired vocabulary.

test_that("chunk_entry_id names entries exactly like their artifacts and receipts", {
  expect_identical(chunk_entry_id("walk", 3L), "walk_3")
  expect_identical(chunk_entry_id("transit", 12L), "transit_12")
})

test_that("a new manifest carries the identity block, frozen census and all-pending entries", {
  identity <- list(version = 1L, network_fingerprint = strrep("a", 64))
  census <- plan_census(chunk_size = 2L, n_origins = 5L, n_origin_coords = 4L,
                        n_destinations = 3L, n_dest_coords = 2L)
  m <- new_run_manifest("bretagne-2026", identity, census,
                        modes = c("walk", "car"), n_chunks = 2L)

  expect_identical(m$kind, "matrice-run-manifest")
  expect_identical(m$manifest_version, 1L)
  expect_identical(m$run_label, "bretagne-2026")
  # Identity block rides verbatim; census is frozen as given.
  expect_identical(m$identity$network_fingerprint, strrep("a", 64))
  expect_identical(m$plan_census$n_chunks, 2L)
  expect_identical(m$plan_census$chunk_size, 2L)
  # ONE entry per (mode x chunk) — the resume unit is the chunk, never a group.
  expect_setequal(names(m$entries), c("walk_1", "walk_2", "car_1", "car_2"))
  for (id in names(m$entries)) {
    e <- m$entries[[id]]
    expect_identical(e$status, "pending")
    expect_identical(e$mode, sub("_.*$", "", id))
    expect_identical(as.integer(e$chunk_id),
                     as.integer(sub("^.*_", "", id)))
    expect_identical(e$path, NULL)
    expect_identical(e$sha256, NULL)
  }
  # Timestamps are UTC ISO-8601 (house convention).
  expect_match(m$created_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
})

test_that("the census derives n_chunks over unique origin coordinates", {
  census <- plan_census(chunk_size = 50000L,
                        n_origins = 1664221L, n_origin_coords = 1424208L,
                        n_destinations = 154417L, n_dest_coords = 112073L)
  expect_identical(census$n_chunks,
                   as.integer(ceiling(1424208L / 50000L)))  # 29 chunks
  # The expected full-run census rides along for cross-checks (#22's gate).
  expect_equal(census$expected_full_run$origins$rows, 1664221L)
  expect_error(plan_census(chunk_size = 0L, n_origins = 1L, n_origin_coords = 1L,
                           n_destinations = 1L, n_dest_coords = 1L), "chunk_size")
})

test_that("save/load round-trips the manifest through versioned pretty JSON", {
  path <- file.path(tempdir(), "manifest-roundtrip", "manifest.json")
  on.exit(unlink(dirname(path), recursive = TRUE, force = TRUE), add = TRUE)
  census <- plan_census(2L, 5L, 4L, 3L, 2L)
  m <- new_run_manifest("rt", list(network_fingerprint = strrep("b", 64)),
                        census, modes = "walk", n_chunks = 2L)
  expect_identical(save_run_manifest(m, path), path)
  raw <- paste(readLines(path), collapse = "\n")
  expect_match(raw, '"manifest_version": 1')          # versioned JSON
  expect_match(raw, '"kind": "matrice-run-manifest"') # house sentinel conventions

  back <- load_run_manifest(path)
  expect_identical(back$kind, m$kind)
  expect_identical(back$manifest_version, 1L)
  expect_identical(back$plan_census$n_chunks, 2L)
  expect_identical(back$entries[["walk_1"]]$status, "pending")
  expect_identical(back$identity$network_fingerprint, strrep("b", 64))
  # A missing manifest is a hard error (resume needs the frozen identity).
  expect_error(load_run_manifest(file.path(tempdir(), "nope", "manifest.json")),
               "manifest")
})

test_that("every update is atomic: PID-tagged temp, rename, no temp leftovers", {
  dir <- file.path(tempdir(), "manifest-atomic")
  path <- file.path(dir, "manifest.json")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  m <- new_run_manifest("atom", list(f = "x"), plan_census(10L, 1L, 1L, 1L, 1L),
                        modes = "walk", n_chunks = 1L)
  save_run_manifest(m, path)
  before <- file.info(path)$mtime

  m$entries[["walk_1"]]$status <- "running"
  Sys.sleep(0.05)
  save_run_manifest(m, path)
  expect_true(file.info(path)$mtime > before)
  expect_identical(load_run_manifest(path)$entries[["walk_1"]]$status, "running")
  # No temp residue next to the manifest (PID-tagged temps are renamed away).
  leftovers <- list.files(dir, pattern = "[.]tmp")
  expect_length(leftovers, 0L)
})

test_that("status lifecycle moves pending -> running -> complete | failed", {
  m <- new_run_manifest("life", list(f = "x"), plan_census(10L, 1L, 1L, 1L, 1L),
                        modes = "walk", n_chunks = 1L)
  e <- m$entries[["walk_1"]]
  expect_identical(e$status, "pending")

  e <- claim_chunk_entry(m, "walk_1")$entries[["walk_1"]]
  expect_identical(e$status, "running")   # a CLAIM, not a fact
  expect_identical(e$attempts, 1L)

  done <- complete_chunk_entry(m, "walk_1",
                               path = "data/matrice/life/chunks/walk_1.parquet",
                               n_rows = 7L, sha256 = strrep("c", 64),
                               route_seconds = 1.5)$entries[["walk_1"]]
  expect_identical(done$status, "complete")
  expect_identical(done$n_rows, 7L)
  expect_identical(done$sha256, strrep("c", 64))
  expect_match(done$validated_at, "Z$")

  fail <- fail_chunk_entry(m, "walk_1", reason = "child exit 3")$entries[["walk_1"]]
  expect_identical(fail$status, "failed")
  expect_match(fail$error, "child exit 3")
})

test_that("all-complete is DERIVED from the entries — no aggregate flag exists", {
  m <- new_run_manifest("derived", list(f = "x"),
                        plan_census(10L, 1L, 1L, 1L, 1L),
                        modes = c("walk", "car"), n_chunks = 1L)
  expect_false(manifest_all_complete(m))

  m$entries[["walk_1"]]$status <- "complete"
  expect_false(manifest_all_complete(m))  # car still owed — absence IS the instruction
  m$entries[["car_1"]]$status <- "complete"
  expect_true(manifest_all_complete(m))

  dir <- file.path(tempdir(), "manifest-derived")
  path <- file.path(dir, "manifest.json")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  save_run_manifest(m, path)
  raw <- paste(readLines(path), collapse = "\n")
  # No aggregate run-complete flag may ever be serialized.
  expect_no_match(raw, "all_complete|run_complete|is_complete")
})
