# Durable-root guardrails (#19, ADR-0004 revision context).
#
# The durable root is the main checkout's data/ tree: the pinned acquisition
# cache, the expensive r5r network caches, and resumable run state. It lives
# OUTSIDE every disposable worktree and must survive worktree cleanup.
# These tests prove the sentinel + cleanup-guardrail contract at fixture
# scale: a fake worktree layout built in tempdir(), never the real data/.

test_that("the sentinel is written once, with the warning payload", {
  root <- tempfile("durable-root-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  p <- write_durable_root_sentinel(root)
  expect_true(file.exists(p))
  expect_identical(basename(p), ".durable-root.json")

  j <- jsonlite::fromJSON(p)
  expect_identical(as.integer(j$version), 1L)
  expect_match(j$description, "durable", ignore.case = TRUE)
  expect_match(j$do_not_delete, "never", ignore.case = TRUE)
  expect_match(j$created_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")

  # Immutable by default: a second write refuses (the sentinel records WHEN
  # the root became durable — silently refreshing it defeats the audit).
  expect_error(write_durable_root_sentinel(root), "sentinel")
  # Deliberate overwrite allowed.
  expect_invisible(write_durable_root_sentinel(root, overwrite = TRUE))
})

test_that("has_durable_root_sentinel detects presence and absence", {
  root <- tempfile("durable-root-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_false(has_durable_root_sentinel(root))
  write_durable_root_sentinel(root)
  expect_true(has_durable_root_sentinel(root))
})

test_that("safe_remove refuses the durable root itself", {
  root <- tempfile("durable-root-")
  dir.create(file.path(root, "downloads"), recursive = TRUE)
  write_durable_root_sentinel(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(safe_remove(root), "durable.*root|sentinel")
  # Nothing was deleted.
  expect_true(dir.exists(root))
  expect_true(has_durable_root_sentinel(root))
})

test_that("safe_remove refuses directories nested under the durable root", {
  root <- tempfile("durable-root-")
  nested <- file.path(root, "acquired", "osm", "network_x")
  dir.create(nested, recursive = TRUE)
  write.file <- file.path(nested, "network.dat")
  writeLines("r5 network", write.file)
  write_durable_root_sentinel(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(safe_remove(nested), "durable.*root|sentinel")
  expect_error(safe_remove(file.path(root, "acquired")), "durable.*root|sentinel")
  expect_error(safe_remove(file.path(root, "acquired", "osm")),
               "durable.*root|sentinel")
  expect_true(file.exists(write.file))

  # assert_not_durable_root agrees with safe_remove's refusal.
  expect_error(assert_not_durable_root(nested), "durable.*root|sentinel")
})

test_that("safe_remove allows ordinary temp directories outside the root", {
  root <- tempfile("durable-root-")
  dir.create(file.path(root, "downloads"), recursive = TRUE)
  write_durable_root_sentinel(root)

  victim <- tempfile("ordinary-temp-")
  dir.create(file.path(victim, "sub"), recursive = TRUE)
  writeLines("scratch", file.path(victim, "sub", "junk.txt"))
  on.exit(unlink(c(root, victim), recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(assert_not_durable_root(victim), NA)
  expect_true(safe_remove(victim))
  expect_false(dir.exists(victim))
  # The durable root was never touched.
  expect_true(dir.exists(root))
})

test_that("safe_remove on an absent path is a harmless no-op", {
  ghost <- tempfile("does-not-exist-")
  expect_false(safe_remove(ghost))
  expect_error(assert_not_durable_root(ghost), NA)
})

test_that("a worktree-shaped cleanup cannot reach the durable root through a junction", {
  skip_on_cran()
  root <- tempfile("durable-root-")
  target <- file.path(root, "downloads", "gtfs")
  dir.create(target, recursive = TRUE)
  writeLines("pinned feed bytes", file.path(target, "feed.zip"))
  write_durable_root_sentinel(root)

  # A junction OUTSIDE the durable root aliasing INTO it — the classic
  # cleanup escape: deleting the alias would destroy durable content.
  outside <- tempfile("cleanup-zone-")
  dir.create(outside)
  alias <- file.path(outside, "alias")
  try(
    system2("cmd", c("/c", "mklink", "/J", shQuote(alias), shQuote(target)),
            stdout = FALSE, stderr = FALSE),
    silent = TRUE
  )
  # Existence through the alias is the only proof that counts.
  linked <- dir.exists(file.path(alias, ".")) &&
    file.exists(file.path(alias, "feed.zip"))
  if (!linked) {
    skip("directory junctions unavailable on this host")
  }
  # Junction cleanup MUST go through cmd rmdir (never recurses into target);
  # register before any expectation so a failure cannot leak the alias.
  on.exit({
    system2("cmd", c("/c", "rmdir", shQuote(alias)))
    unlink(c(root, outside), recursive = TRUE, force = TRUE)
  }, add = TRUE)

  expect_error(safe_remove(alias), "durable.*root|sentinel")
  # The durable bytes survived the refused deletion.
  expect_true(file.exists(file.path(target, "feed.zip")))

  # With the refusal honoured there is no legitimate way through: the
  # explicit link override still refuses because the RESOLVED path lands
  # under the durable root — escaping via an alias is refused, period.
  expect_error(safe_remove(alias, force_links = TRUE), "durable.*root|sentinel")
  expect_true(file.exists(file.path(target, "feed.zip")))
})

test_that("safe_remove strips a stray link outside any durable root only on explicit override", {
  skip_on_cran()
  outside <- tempfile("cleanup-zone-")
  dir.create(file.path(outside, "real"), recursive = TRUE)
  writeLines("scratch", file.path(outside, "real", "x.txt"))

  alias <- file.path(outside, "alias")
  system2("cmd", c("/c", "mklink", "/J", shQuote(alias),
                   shQuote(file.path(outside, "real"))))
  if (!file.exists(file.path(alias, "x.txt"))) {
    skip("directory junctions unavailable on this host")
  }
  on.exit({
    if (dir.exists(alias) || file.exists(alias)) {
      invisible(system2("cmd", c("/c", "rmdir", shQuote(alias)),
                        stdout = FALSE, stderr = FALSE))
    }
    unlink(outside, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Conservative default: recursive removal through a link is refused.
  expect_error(safe_remove(alias), "link|junction")
  expect_true(file.exists(file.path(outside, "real", "x.txt")))

  # Explicit override removes the LINK only; the target content survives.
  expect_true(safe_remove(alias, force_links = TRUE))
  expect_false(dir.exists(alias))
  expect_true(file.exists(file.path(outside, "real", "x.txt")))
})
