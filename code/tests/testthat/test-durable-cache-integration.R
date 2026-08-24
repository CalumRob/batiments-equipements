# End-to-end composition of the #19 seams at fixture scale — the exact flow
# ticket 21's batch runner and ticket 22's gate will drive post-merge:
#
#   sentinel'd durable root -> regime staging -> integrity gate -> identity
#   -> build (fixture marker) -> commit marker -> probe HIT on re-invocation
#   -> portable run metadata -> guardrailed cleanup.

test_that("sentinel, staging, identity, probe, portability and guardrails compose", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)

  # 1. Formalize the durable root (data/ carries the sentinel).
  expect_invisible(write_durable_root_sentinel(fx$data_dir))
  expect_true(has_durable_root_sentinel(fx$data_dir))

  # 2. Stage the default regime into a network directory UNDER the durable
  #    root (where the expensive network must live to be protected).
  net_dir <- file.path(fx$data_dir, "networks", "current")
  block <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                               manifest_path = fx$manifest_path)
  expect_identical(block$n_feeds, 3L)

  # 3. Identity over the OSM crop pin + every staged feed.
  osm_bytes <- charToRaw("OSM-CROP-FIXTURE")
  osm_pin <- list(id = "osm_bretagne_crop_fixture",
                  sha256 = digest::digest(osm_bytes, algo = "sha256",
                                          serialize = FALSE))
  identity <- network_cache_identity(osm_pin, block$feeds,
                                     elevation_pin = NULL)

  # 4. First setup invocation: probe misses (no marker), "build", commit.
  expect_false(probe_network_cache(net_dir, identity)$cache_hit)
  writeLines("simulated built network.dat",
             file.path(net_dir, "network.dat"))
  marker <- commit_network_cache(net_dir, identity)
  expect_true(file.exists(marker))

  # 5. Second setup invocation with identical inputs: HIT, no rebuild.
  probe <- probe_network_cache(net_dir, identity)
  expect_true(probe$cache_hit)
  expect_identical(probe$found_fingerprint, identity$fingerprint)

  # 6. Drift ONE feed sha upstream (re-pinned deliberately): staging copies
  #    the new bytes, identity flips, the probe MISSES -> rebuild is owed.
  tampered <- file.path(fx$data_dir, "downloads", "gtfs-original-a.zip")
  writeBin(charToRaw("GTFS-FIXTURE-BYTES-REPINNED"), tampered)
  m <- manifest_load(fx$manifest_path)
  m$sources[["gtfs-original-a"]]$sha256 <-
    digest::digest(charToRaw("GTFS-FIXTURE-BYTES-REPINNED"),
                   algo = "sha256", serialize = FALSE)
  manifest_save(m, fx$manifest_path)
  block2 <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                                manifest_path = fx$manifest_path)
  identity2 <- network_cache_identity(osm_pin, block2$feeds,
                                      elevation_pin = NULL)
  expect_false(identical(identity$fingerprint, identity2$fingerprint))
  expect_false(probe_network_cache(net_dir, identity2)$cache_hit)

  # 7. Run metadata stays portable: rewrite relative to the durable root,
  #    then enforce the no-absolute-paths rule (#19).
  summary <- list(
    network_dir = normalizePath(net_dir, winslash = "/"),
    network_dat = file.path(normalizePath(net_dir, winslash = "/"),
                            "network.dat"),
    staged_files = vapply(block2$feeds, function(f)
      file.path(normalizePath(net_dir, winslash = "/"), f$staged_file), ""),
    transit_identity = block2,
    fingerprint = identity2$fingerprint
  )
  portable <- make_metadata_portable(summary, fx$root)
  expect_error(assert_no_absolute_paths(portable), NA)
  expect_identical(portable$network_dat, "data/networks/current/network.dat")

  # 8. Guardrails: the network directory sits under the sentinel'd data/,
  #    so cleanup cannot take it; ordinary temp dirs go untouched-by-rule.
  expect_error(safe_remove(net_dir), "durable.*root|sentinel")
  expect_true(file.exists(file.path(net_dir, "network.dat")))
  scratch <- tempfile("ordinary-cleanup-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  expect_true(safe_remove(scratch))

  # 9. Worktree-removal rehearsal at fixture scale: deleting a FAKE worktree
  #    directory leaves the durable root byte-identical.
  fake_wt <- tempfile("fake-worktree-")
  dir.create(fake_wt)
  writeLines("worktree junk", file.path(fake_wt, "scratch.txt"))
  on.exit(unlink(fake_wt, recursive = TRUE, force = TRUE), add = TRUE)
  before <- sort(list.files(file.path(fx$data_dir, "networks", "current"),
                            recursive = TRUE))
  expect_true(safe_remove(fake_wt))
  after <- sort(list.files(file.path(fx$data_dir, "networks", "current"),
                           recursive = TRUE))
  expect_identical(before, after)
})
