# The multi-feed staging seam (#19, ADR-0004 revision).
#
# Ticket 25 delivered the promoted manifest: 159 r5r-transit pins in
# pin_key_role groups plus the derived namespaced gtfsx_* set. The seam must
# select a window regime (current primaries + gtfsx_* by default; the D1
# archive group otherwise), gate EVERY selected pin on existence + sha256
# before anything touches r5r, stage all feeds into the network directory
# keeping their (already namespaced) filenames, and return the COMPLETE
# transit identity block for cache identity and run metadata.

test_that("transit_window_regimes names the two coherent window regimes", {
  expect_setequal(transit_window_regimes(), c("current", "D1-archive"))
})

test_that("select_transit_pins default regime = current primaries + the derived gtfsx_* set", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  m <- manifest_load(fx$manifest_path)

  sel <- select_transit_pins(m, regime = "current")
  ids <- names(sel)
  # The current primaries and BOTH derived feeds are in.
  expect_true("gtfs-original-a" %in% ids)
  expect_setequal(c("gtfsx_a", "gtfsx_b"), grep("^gtfsx_", ids, value = TRUE))
  # The rival vintage, the D1 archive group, auxiliary archives and every
  # non-transit reader stay OUT — staging a rival vintage would double-count
  # networks (ADR-0004: every network routes exactly once).
  expect_false("gtfs-d1-b" %in% ids)
  expect_false("reference-c" %in% ids)
  expect_false("archive-d" %in% ids)
  expect_false("bpe_2025" %in% ids)
  # Roles ride along for the identity block.
  expect_identical(sel[["gtfs-original-a"]]$pin_key_role, "primary-current")
  expect_null(sel[["gtfsx_a"]]$pin_key_role)
})

test_that("select_transit_pins D1-archive regime selects exactly the primary-D1 group", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  m <- manifest_load(fx$manifest_path)

  sel <- select_transit_pins(m, regime = "D1-archive")
  expect_identical(names(sel), "gtfs-d1-b")
  expect_identical(sel[["gtfs-d1-b"]]$pin_key_role, "primary-D1")
})

test_that("select_transit_pins refuses an empty selection and unknown regimes", {
  m <- list(manifest_version = 1L, sources = list())
  expect_error(select_transit_pins(m), "no.*transit|not promoted|manifest")
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(select_transit_pins(manifest_load(fx$manifest_path),
                                   regime = "vintage-x"), "regime")
})

test_that("resolve_cached_path finds root-relative cached_path entries", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)

  # Real-manifest style: "data/downloads/x.zip" relative to the durable root.
  p <- resolve_cached_path("data/downloads/gtfs-original-a.zip", fx$data_dir)
  expect_true(file.exists(p))
  expect_identical(normalizePath(p, winslash = "/"),
                   normalizePath(file.path(fx$data_dir, "downloads",
                                           "gtfs-original-a.zip"),
                                 winslash = "/"))
})

test_that("verify_transit_pins gates existence and sha256 before anything touches r5r", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  m <- manifest_load(fx$manifest_path)

  sel <- select_transit_pins(m, regime = "current")

  # Happy path: every selected pin verifies against its bytes on disk.
  v <- verify_transit_pins(sel, data_dir = fx$data_dir)
  expect_identical(v$n_verified, length(sel))

  # Missing cached file -> hard failure naming the feed.
  unlink(file.path(fx$data_dir, "downloads", "derived", "a__feed-a.zip"))
  err <- tryCatch(verify_transit_pins(sel, data_dir = fx$data_dir),
                  error = function(e) e)
  expect_match(conditionMessage(err), "gtfsx_a")
  expect_match(conditionMessage(err), "not found|missing")

  # Corrupted cache (sha mismatch) -> hard failure naming the feed.
  write_fake_feed(file.path(fx$data_dir, "downloads", "gtfs-original-a.zip"),
                  "TAMPERED")
  err2 <- tryCatch(verify_transit_pins(sel, data_dir = fx$data_dir),
                   error = function(e) e)
  expect_match(conditionMessage(err2), "gtfs-original-a")
  expect_match(conditionMessage(err2), "sha256|mismatch")
})

test_that("stage_transit_feeds stages all N feeds, keeping namespaced filenames", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  net_dir <- file.path(fx$root, "network-current")
  on.exit(unlink(net_dir, recursive = TRUE, force = TRUE), add = TRUE)

  block <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                               manifest_path = fx$manifest_path)

  expect_identical(block$regime, "current")
  expect_identical(block$n_feeds, 3L)
  staged_files <- sort(list.files(net_dir))
  expect_identical(staged_files,
                   c("a__feed-a.zip", "b__feed-b.zip", "gtfs-original-a.zip"))

  # The COMPLETE transit identity block: every pin id + sha256 + role,
  # byte-exact against the manifest pins.
  ids <- vapply(block$feeds, `[[`, "", "id")
  shas <- vapply(block$feeds, `[[`, "", "sha256")
  roles <- vapply(block$feeds, function(x) x$role %||% NA_character_, "")
  expect_setequal(ids, c("gtfs-original-a", "gtfsx_a", "gtfsx_b"))
  for (id in ids) {
    expect_identical(shas[[which(ids == id)]],
                     fx$entries[[which(vapply(fx$entries, `[[`, "", "id") == id)]]$sha256)
  }
  expect_identical(roles[[which(ids == "gtfs-original-a")]], "primary-current")
  # Derived feeds record their namespacing prefix and their derived role.
  a <- block$feeds[[which(ids == "gtfsx_a")]]
  expect_identical(a$prefix, "a")
  expect_identical(a$role, "derived-namespaced")
  # Staged paths are plain file NAMES relative to the network directory.
  expect_setequal(basename(unlist(lapply(block$feeds, `[[`, "staged_file"))), staged_files)

  # JSON-safe: the block serializes cleanly for run metadata.
  rt <- jsonlite::fromJSON(jsonlite::toJSON(block, auto_unbox = TRUE),
                           simplifyVector = FALSE)
  expect_length(rt$feeds, 3L)
})

test_that("stage_transit_feeds is idempotent on re-invocation", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  net_dir <- file.path(fx$root, "network-current")

  b1 <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                            manifest_path = fx$manifest_path)
  b2 <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                            manifest_path = fx$manifest_path)
  expect_identical(lapply(b1$feeds, `[[`, "sha256"),
                   lapply(b2$feeds, `[[`, "sha256"))
  expect_identical(sort(list.files(net_dir)), sort(list.files(net_dir)))
})

test_that("stage_transit_feeds honours the D1-archive regime", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  net_dir <- file.path(fx$root, "network-d1")

  block <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                               manifest_path = fx$manifest_path,
                               regime = "D1-archive")
  expect_identical(block$n_feeds, 1L)
  expect_identical(block$feeds[[1]]$id, "gtfs-d1-b")
  expect_identical(list.files(net_dir), "gtfs-d1-b.zip")
})

test_that("stage_transit_feeds refuses to hand r5r an unverified feed", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  # Corrupt one pinned feed AFTER promotion: the integrity gate must stop
  # the staging before any copy happens.
  unlink(file.path(fx$data_dir, "downloads", "derived", "b__feed-b.zip"))
  net_dir <- file.path(fx$root, "never-built")
  expect_error(stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                                   manifest_path = fx$manifest_path),
               "gtfsx_b")
  expect_false(dir.exists(net_dir))
})
