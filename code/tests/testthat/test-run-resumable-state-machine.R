library(testthat)
library(data.table)

# The ORCHESTRATOR state machine (#21): run_resumable() owns the manifest,
# spawns children STRICTLY sequentially, marks an entry complete only after
# the child validated AND the orchestrator independently re-validates +
# cross-checks sha256 against the receipt, and resumes exactly what is not
# complete. All children here are SCRIPTED FAKES executing the real worker
# pipeline in-process — the true Rscript boundary has its own gated smoke.

fx_run_args <- function(fx, modes = c("walk", "car"),
                        fingerprint = strrep("a", 64),
                        git_sha = strrep("7", 40), chunk_size = NULL) {
  list(
    run_label = basename(fx$run_dir),
    modes = modes,
    chunk_size = if (is.null(chunk_size)) fx$chunk_size else chunk_size,
    network_identity = list(fingerprint = fingerprint, components = NULL),
    network_dir = file.path(fx$data_dir, "networks", "current"),
    origins_provider = function() fx$origins,
    destinations_provider = function() list(
      destinations = fx$dests[, .(id, lon, lat)],
      dest_map = fx$dest_map,
      registry = fx$dests),
    git_sha = git_sha,
    data_dir = fx$data_dir,
    out_dir = file.path(fx$root, "data", "matrice"),
    code_dir = normalizePath(testthat::test_path("../../.."), winslash = "/")
  )
}

run_fx <- function(args, spawn_child) {
  do.call(run_resumable, c(args, list(spawn_child = spawn_child)))
}

test_that("a clean run completes every entry through sequential spawns", {
  fx <- fixture_run_layout(chunk_size = 2L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  spy <- scripted_spawn()

  out <- run_fx(fx_run_args(fx), spy)

  expect_true(out$complete)
  m <- load_run_manifest(fx$manifest_path)
  expect_true(manifest_all_complete(m))
  expect_setequal(names(m$entries),
                  c("walk_1", "walk_2", "car_1", "car_2"))
  # Strictly sequential: one live child at a time, chunks in plan order.
  expect_identical(spawn_calls(spy)$chunks, c("1:ok", "2:ok"))
  # Each completed entry carries its provenance; paths stay portable.
  e <- m$entries[["walk_1"]]
  expect_identical(e$status, "complete")
  expect_identical(e$sha256, sha256_file(file.path(fx$root, e$path)))
  expect_identical(as.integer(e$n_rows), 9L)
  expect_match(e$validated_at, "Z$")
  expect_identical(e$path,
                   sprintf("data/matrice/%s/chunks/walk_1.parquet",
                           basename(fx$run_dir)))
})

test_that("an exit-nonzero child fails its entries and the run continues", {
  fx <- fixture_run_layout(chunk_size = 2L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)

  out <- run_fx(fx_run_args(fx, modes = "walk"),
                scripted_spawn(script = list(`1` = "crash-before-write")))

  m <- load_run_manifest(fx$manifest_path)
  expect_identical(m$entries[["walk_1"]]$status, "failed")
  expect_match(m$entries[["walk_1"]]$error, "JVM death")
  expect_identical(as.integer(m$entries[["walk_1"]]$attempts), 1L)
  expect_identical(m$entries[["walk_2"]]$status, "complete")  # run continued
  expect_false(out$complete)
})

test_that("receipt-without-artifact and artifact-without-receipt both fail", {
  for (damage in c("receipt-without-artifact", "artifact-without-receipt")) {
    fx <- fixture_run_layout(chunk_size = 10L)
    on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
    run_fx(fx_run_args(fx, modes = "walk"),
           scripted_spawn(script = list(`1` = damage)))

    m <- load_run_manifest(fx$manifest_path)
    expect_identical(m$entries[["walk_1"]]$status, "failed")
    # The trust boundary names what was missing: no artifact to cross-check,
    # or no receipt to cross-check against.
    expected <- if (identical(damage, "receipt-without-artifact"))
      "artifact missing" else "receipt not found"
    expect_match(m$entries[["walk_1"]]$error, expected)
  }
})

test_that("corrupted bytes fail the damaged mode only — checksum trust boundary", {
  fx <- fixture_run_layout(chunk_size = 10L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  run_fx(fx_run_args(fx, modes = c("walk", "car")),
         scripted_spawn(script = list(`1` = "corrupted-bytes"),
                        corrupt_mode = "walk"))

  m <- load_run_manifest(fx$manifest_path)
  expect_identical(m$entries[["walk_1"]]$status, "failed")
  expect_match(m$entries[["walk_1"]]$error, "sha256 mismatch")
  expect_identical(m$entries[["car_1"]]$status, "complete")  # sibling survives
})

test_that("resume redoes exactly the damaged mode and never valid entries", {
  fx <- fixture_run_layout(chunk_size = 2L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fx_run_args(fx)

  run_fx(args, scripted_spawn())

  # Damage: orphan-overwrite car_2's artifact AFTER completion — the classic
  # stray-child corruption the startup sweep must catch via recorded sha.
  m <- load_run_manifest(fx$manifest_path)
  cat("ORPHAN-OVERWRITE",
      file = file.path(fx$root, m$entries[["car_2"]]$path), append = TRUE)
  walk1_before <- m$entries[["walk_1"]]$sha256

  spy <- scripted_spawn()
  run_fx(args, spy)

  # Exactly ONE respawn: chunk 2, only its damaged mode; walk_1 untouched.
  expect_identical(spawn_calls(spy)$chunks, "2:ok")
  req <- chunk_request_load(file.path(fx$run_dir, "requests", "chunk_2.json"))
  expect_identical(req$modes, "car")
  m2 <- load_run_manifest(fx$manifest_path)
  expect_true(manifest_all_complete(m2))
  expect_identical(m2$entries[["walk_1"]]$sha256, walk1_before)
})

test_that("a stale claim whose work survived is salvaged, never respawned", {
  fx <- fixture_run_layout(chunk_size = 10L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fx_run_args(fx, modes = c("walk", "car"))
  run_fx(args, scripted_spawn())

  # Simulate an orchestrator killed after the child finished but before the
  # completion was recorded: the claim went stale, the bytes survived.
  m <- load_run_manifest(fx$manifest_path)
  m$entries[["walk_1"]]$status <- "running"
  save_run_manifest(m, fx$manifest_path)

  spy <- scripted_spawn()
  run_fx(args, spy)

  # The sweep re-validates artifact + receipt and promotes — no child spawn.
  expect_identical(spawn_calls(spy)$chunks, character(0))
  expect_true(manifest_all_complete(load_run_manifest(fx$manifest_path)))
})

test_that("a stale claim whose bytes are gone is work to do", {
  fx <- fixture_run_layout(chunk_size = 10L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fx_run_args(fx, modes = c("walk", "car"))
  run_fx(args, scripted_spawn())

  m <- load_run_manifest(fx$manifest_path)
  m$entries[["walk_1"]]$status <- "running"
  save_run_manifest(m, fx$manifest_path)
  # ...and this time the child's outputs died with it.
  unlink(file.path(fx$run_dir, "chunks", "walk_1.parquet"))
  unlink(file.path(fx$run_dir, "receipts", "walk_1.json"))

  spy <- scripted_spawn()
  run_fx(args, spy)

  expect_identical(spawn_calls(spy)$chunks, "1:ok")   # only walk_1 was owed
  req <- chunk_request_load(file.path(fx$run_dir, "requests", "chunk_1.json"))
  expect_identical(req$modes, "walk")
  expect_true(manifest_all_complete(load_run_manifest(fx$manifest_path)))
})

test_that("resume refuses incompatible identity naming the first mismatch", {
  fx <- fixture_run_layout(chunk_size = 10L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fx_run_args(fx)
  run_fx(args, scripted_spawn())

  # Network fingerprint changed upstream -> hard refuse, both sides named.
  expect_error(
    run_fx(fx_run_args(fx, fingerprint = strrep("f", 64)), scripted_spawn()),
    "resume refused.*network cache fingerprint mismatch")

  # Chunk re-cut -> census drift refuses.
  expect_error(
    run_fx(fx_run_args(fx, chunk_size = 3L), scripted_spawn()),
    "resume refused.*plan_census[.]chunk_size")

  # Routing parameter change is refusal-grade identity (D5).
  args_speed <- fx_run_args(fx)
  orig_provider <- args_speed$origins_provider
  expect_error(
    do.call(run_resumable,
            c(args_speed, list(walk_speed = 5,
                               spawn_child = scripted_spawn()))),
    "resume refused.*routing_parameters[.]walk_speed")
})

test_that("allow_code_drift continues past a git-SHA change and records it", {
  fx <- fixture_run_layout(chunk_size = 2L)
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fx_run_args(fx)
  run_fx(args, scripted_spawn())

  spy <- scripted_spawn()
  out <- do.call(run_resumable, c(
    fx_run_args(fx, git_sha = strrep("d", 40)),
    list(spawn_child = spy, allow_code_drift = TRUE)))

  # Everything already complete: nothing respawns; drift documented.
  expect_identical(spawn_calls(spy)$chunks, character(0))
  expect_true(out$code_drift_allowed)
  m <- load_run_manifest(fx$manifest_path)
  expect_match(m$code_drift$note, strrep("d", 40))

  # ...and WITHOUT the flag the same state refuses.
  expect_error(
    do.call(run_resumable, c(fx_run_args(fx, git_sha = strrep("e", 40)),
                             list(spawn_child = scripted_spawn()))),
    "resume refused.*git_sha|code drift")
})
