# Cache identity v2 and the cache-hit probe seam (#19).
#
# Identity = sha over [OSM crop pin] + [EVERY selected transit pin, order-
# independent canonical sort] + [DEM pin or elevation=NONE] + [r5r version,
# R5 version] + [W via border_width_m()] + [cap via cap_minutes()]. The
# fingerprint decides network reuse: change one feed sha and the Bretagne
# network rebuilds; keep every component byte-identical and the expensive
# build (post-merge orchestration, #22 kicks it) is reused.

test_that("r5r_runtime_versions reads both versions without starting a JVM", {
  v <- r5r_runtime_versions()
  expect_match(v$r5r, "^[0-9]+[.][0-9]+([.][0-9]+)?$")
  expect_match(v$r5, "^[0-9]+[.][0-9]+([.][0-9]+)?$")
  # r5r 2.x ships R5 v7.x — a coarse cross-check against the DESCRIPTION.
  expect_identical(v$r5r, as.character(utils::packageVersion("r5r")))
})

test_that("network_cache_identity returns the fingerprint plus structured components", {
  osm <- list(id = "osm_bretagne_crop_26.6km", sha256 = paste0(rep("a", 64), collapse = ""))
  feeds <- list(
    list(id = "gtfsx_star", sha256 = paste0(rep("1", 64), collapse = ""), role = "derived-namespaced"),
    list(id = "gtfsx_bibus", sha256 = paste0(rep("2", 64), collapse = ""), role = "derived-namespaced")
  )
  id1 <- network_cache_identity(osm_pin = osm, transit_pins = feeds,
                                elevation_pin = NULL)
  expect_type(id1$fingerprint, "character")
  expect_true(grepl("^[0-9a-f]{64}$", id1$fingerprint))
  expect_s3_class(id1$components, "network_identity_components")

  c <- id1$components
  expect_identical(c$osm_pin$id, osm$id)
  expect_length(c$transit_lines, 2L)
  # Canonical form is radix-sorted id=sha pairs: order-independent by
  # construction.
  expect_identical(c$transit_lines, sort(c(
    paste0("feed:gtfsx_star=", strrep("1", 64)),
    paste0("feed:gtfsx_bibus=", strrep("2", 64))
  ), method = "radix"))
  expect_null(c$elevation_pin)
  expect_identical(c$elevation_label, "NONE")
  expect_identical(c$r5r_version, as.character(utils::packageVersion("r5r")))
  expect_identical(c$W_m, border_width_m())
  expect_identical(c$cap_minutes, cap_minutes())
})

test_that("the fingerprint is deterministic under feed-order shuffling", {
  osm <- list(id = "osm", sha256 = strrep("a", 64))
  feeds <- list(
    list(id = "feed-a", sha256 = strrep("1", 64)),
    list(id = "feed-b", sha256 = strrep("2", 64)),
    list(id = "feed-c", sha256 = strrep("3", 64)),
    list(id = "feed-d", sha256 = strrep("4", 64))
  )
  id1 <- network_cache_identity(osm, feeds, NULL)
  id2 <- network_cache_identity(osm, feeds[c(3, 1, 4, 2)], NULL)
  id3 <- network_cache_identity(osm, rev(feeds), NULL)
  expect_identical(id1$fingerprint, id2$fingerprint)
  expect_identical(id1$fingerprint, id3$fingerprint)
})

test_that("the fingerprint is sensitive to every component it names", {
  osm <- list(id = "osm", sha256 = strrep("a", 64))
  feeds <- list(list(id = "feed-a", sha256 = strrep("1", 64)))
  base <- network_cache_identity(osm, feeds, NULL)

  swap_sha <- function(x) {
    x$components$osm_pin$sha256 <- strrep("b", 64)
    x
  }
  expect_false(identical(base$fingerprint,
                         network_fingerprint(swap_sha(base)$components)))

  # One transit feed's sha changes -> different network -> different identity.
  alt_feeds <- list(list(id = "feed-a", sha256 = strrep("9", 64)))
  expect_false(identical(
    base$fingerprint,
    network_cache_identity(osm, alt_feeds, NULL)$fingerprint))

  # A feed's IDENTITY (id) changes at constant sha -> still different.
  renamed <- list(list(id = "feed-z", sha256 = strrep("1", 64)))
  expect_false(identical(
    base$fingerprint,
    network_cache_identity(osm, renamed, NULL)$fingerprint))

  # Adding / removing a feed changes identity.
  more <- append(feeds, list(list(id = "feed-b", sha256 = strrep("2", 64))))
  expect_false(identical(
    base$fingerprint,
    network_cache_identity(osm, more, NULL)$fingerprint))

  # Elevation: NONE vs a DEM pin differ; two DIFFERENT DEM pins differ.
  dem <- list(id = "dem_srtm_gl1_v3_N48W002", sha256 = strrep("d", 64))
  with_dem <- network_cache_identity(osm, feeds, dem)
  expect_false(identical(base$fingerprint, with_dem$fingerprint))
  other_dem <- list(id = dem$id, sha256 = strrep("e", 64))
  expect_false(identical(
    with_dem$fingerprint,
    network_cache_identity(osm, feeds, other_dem)$fingerprint))

  # Engine versions, border width, cap.
  bump_r5r <- function(v) { v$components$r5r_version <- "99.0.0"; v }
  expect_false(identical(base$fingerprint,
                         network_fingerprint(bump_r5r(base)$components)))
  bump_r5 <- function(v) { v$components$r5_version <- "99.0"; v }
  expect_false(identical(base$fingerprint,
                         network_fingerprint(bump_r5(base)$components)))
  bump_w <- function(v) { v$components$W_m <- 15000L; v }
  expect_false(identical(base$fingerprint,
                         network_fingerprint(bump_w(base)$components)))
  bump_cap <- function(v) { v$components$cap_minutes <- 30L; v }
  expect_false(identical(base$fingerprint,
                         network_fingerprint(bump_cap(base)$components)))
})

test_that("the staging block plugs straight into cache identity", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  net_dir <- file.path(fx$root, "net")
  block <- stage_transit_feeds(net_dir, data_dir = fx$data_dir,
                               manifest_path = fx$manifest_path)
  osm <- list(id = "osm_fixture", sha256 = strrep("a", 64))
  id_block <- network_cache_identity(osm_pin = osm, transit_pins = block$feeds,
                                     elevation_pin = NULL)
  id_manual <- network_cache_identity(
    osm_pin = osm,
    transit_pins = lapply(block$feeds, function(f) list(id = f$id, sha256 = f$sha256)),
    elevation_pin = NULL)
  expect_identical(id_block$fingerprint, id_manual$fingerprint)
})

test_that("commit + probe detect an identity match and reuse without rebuild", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  net_dir <- file.path(fx$root, "net-built")
  dir.create(net_dir)
  # Fixture scale: a marker file stands in for the built network.dat.
  writeLines("simulated r5 network", file.path(net_dir, "network.dat"))

  osm <- list(id = "osm_fixture", sha256 = strrep("a", 64))
  feeds <- list(list(id = "gtfsx_a", sha256 = strrep("1", 64)))
  identity <- network_cache_identity(osm, feeds, NULL)

  # Before commit: no marker -> miss, reason says why.
  miss <- probe_network_cache(net_dir, identity)
  expect_false(miss$cache_hit)
  expect_match(miss$reason, "no identity marker|absent")

  commit_network_cache(net_dir, identity)
  hit <- probe_network_cache(net_dir, identity)
  expect_true(hit$cache_hit)
  expect_identical(hit$found_fingerprint, identity$fingerprint)

  # A second setup invocation with the SAME inputs reuses without rebuild:
  # the probe is what #21/#22 call before paying for setup_r5 again.
  again <- probe_network_cache(net_dir,
                               network_cache_identity(osm, rev(feeds), NULL))
  expect_true(again$cache_hit)  # order-independence holds through the probe

  # Any input drift -> miss, naming both fingerprints.
  drifted_osm <- list(id = osm$id, sha256 = strrep("f", 64))
  drifted_identity <- network_cache_identity(drifted_osm, feeds, NULL)
  drifted <- probe_network_cache(net_dir, drifted_identity)
  expect_false(drifted$cache_hit)
  expect_match(drifted$reason, "fingerprint mismatch")
  expect_identical(drifted$expected_fingerprint,
                   drifted_identity$fingerprint)
  # The marker still records what WAS built (the original identity) — the
  # mismatch is exactly requested-vs-built.
  expect_identical(drifted$found_fingerprint, identity$fingerprint)
  expect_false(identical(drifted$expected_fingerprint, identity$fingerprint))

  # Tampered marker (corrupt JSON) -> miss, never a false hit.
  writeLines("{not json", file.path(net_dir, ".network-identity.json"))
  broken <- probe_network_cache(net_dir, identity)
  expect_false(broken$cache_hit)

  # Missing network directory entirely -> miss.
  expect_false(probe_network_cache(file.path(fx$root, "nope"),
                                   identity)$cache_hit)
})

test_that("the committed marker carries no absolute paths (portable metadata)", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  net_dir <- file.path(fx$root, "net-marked")
  dir.create(net_dir)
  identity <- network_cache_identity(list(id = "o", sha256 = strrep("a", 64)),
                                     list(), NULL)
  marker <- commit_network_cache(net_dir, identity)
  raw <- paste(readLines(marker), collapse = "\n")
  expect_no_match(raw, "[A-Za-z]:[/\\\\]")
  expect_no_match(raw, "//")
})

test_that("the identity canonical lines pin W and cap to the named constants", {
  c <- network_identity_components(list(id = "o", sha256 = strrep("a", 64)),
                                   list(), NULL)
  lines <- network_identity_canonical_lines(c)
  expect_true(sprintf("W=%dm", border_width_m()) %in% lines)
  expect_true(sprintf("cap=%dmin", cap_minutes()) %in% lines)
})
