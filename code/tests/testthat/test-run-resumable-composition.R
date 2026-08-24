library(testthat)
library(data.table)

# End-to-end COMPOSITION of the #21 seams at fixture scale — sibling of
# test-durable-cache-integration.R (#19):
#
#   sentinel'd durable root -> network identity -> orchestration -> all
#   complete (DERIVED) -> run_metadata.json assembled + portable -> scripted
#   damage -> SURGICAL resume of exactly the damaged entries -> guardrails.

test_that("create, complete, damage, resume: one composition drives the whole machine", {
  fx <- fixture_run_layout(chunk_size = 2L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)

  # 1. The durable root carries its sentinel; cleanup cannot take the run.
  expect_invisible(write_durable_root_sentinel(fx$data_dir))
  expect_error(safe_remove(fx$run_dir), "durable.*root|sentinel")

  # 2. Identity composed through the REAL seam (network_cache_identity).
  identity <- network_cache_identity(
    osm_pin = list(id = "osm_fixture", sha256 = strrep("a", 64)),
    transit_pins = list(list(id = "gtfsx_a", sha256 = strrep("1", 64))),
    elevation_pin = NULL)
  args <- fixture_run_args(fx)
  args$network_identity <- identity

  # 3. First orchestration: everything completes through sequential children.
  spy1 <- scripted_spawn()
  out1 <- do.call(run_resumable, c(args, list(spawn_child = spy1)))
  expect_true(out1$complete)
  expect_identical(spawn_calls(spy1)$chunks, c("1:ok", "2:ok"))

  # All-complete was DERIVED: run_metadata.json exists because every entry is
  # complete, and it is portable (#19) with the cap-and-ladder contract.
  meta_path <- file.path(fx$run_dir, "run_metadata.json")
  expect_true(file.exists(meta_path))
  meta <- jsonlite::fromJSON(meta_path, simplifyVector = FALSE)
  expect_match(meta$identity$network_fingerprint, identity$fingerprint,
               fixed = TRUE)
  expect_identical(as.integer(meta$cap_minutes), cap_minutes())
  raw_meta <- paste(readLines(meta_path), collapse = "\n")
  expect_no_match(raw_meta, "[A-Za-z]:[/\\\\]")
  expect_no_match(raw_meta, "all_complete|run_complete")

  # The manifest itself records only portable paths.
  raw_manifest <- paste(readLines(fx$manifest_path), collapse = "\n")
  expect_no_match(raw_manifest, "[A-Za-z]:[/\\\\]")

  # 4. Scripted damage, three causes at once:
  #    a) DELETE car_2's artifact (entry demoted at sweep);
  #    b) CORRUPT walk_2's bytes (sha drift, the trust boundary catches it);
  #    c) stale walk_1's claim while its bytes survive (salvage, no respawn).
  m <- load_run_manifest(fx$manifest_path)
  walk1_sha_before <- m$entries[["walk_1"]]$sha256
  walk1_attempts_before <- as.integer(m$entries[["walk_1"]]$attempts)
  file.remove(file.path(fx$root, m$entries[["car_2"]]$path))
  file.remove(file.path(fx$run_dir, "receipts", "car_2.json"))
  cat("CORRUPTED", file = file.path(fx$root, m$entries[["walk_2"]]$path),
      append = TRUE)
  m$entries[["walk_1"]]$status <- "running"
  save_run_manifest(m, fx$manifest_path)

  # 5. Resume: exactly three entries were owed; walk_1 salvages (its child's
  #    work re-validates), chunks 2 spawn for their damaged modes only.
  spy2 <- scripted_spawn()
  out2 <- do.call(run_resumable, c(args, list(spawn_child = spy2)))
  expect_true(out2$complete)
  expect_identical(spawn_calls(spy2)$chunks, "2:ok")
  req2 <- chunk_request_load(file.path(fx$run_dir, "requests", "chunk_2.json"))
  expect_setequal(req2$modes, c("walk", "car"))

  m2 <- load_run_manifest(fx$manifest_path)
  expect_true(manifest_all_complete(m2))
  # Salvage kept the ORIGINAL work: same bytes, no extra attempt charged.
  expect_identical(m2$entries[["walk_1"]]$sha256, walk1_sha_before)
  expect_identical(as.integer(m2$entries[["walk_1"]]$attempts),
                   walk1_attempts_before)
})

test_that("resume after partial completion finishes only what remains", {
  fx <- fixture_run_layout(chunk_size = 10L)   # one chunk, two modes
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fixture_run_args(fx)

  # First pass: the child dies before writing anything.
  out1 <- do.call(run_resumable, c(args, list(spawn_child =
    scripted_spawn(script = list(`1` = "crash-before-write")))))
  expect_false(out1$complete)
  expect_identical(out1$n_failed, 2L)

  # Second pass with healthy children: both modes rerun and complete.
  spy <- scripted_spawn()
  out2 <- do.call(run_resumable, c(args, list(spawn_child = spy)))
  expect_true(out2$complete)
  expect_length(spawn_calls(spy)$chunks, 1L)
  expect_identical(out2$n_complete, 2L)
})
