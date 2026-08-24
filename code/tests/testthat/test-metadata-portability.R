# Metadata portability (#19).
#
# The known defect of the failed full-run attempt: run metadata recorded
# absolute paths inside a disposable worktree — dead references the moment
# the worktree was removed. The rule from #19: wherever network setup or
# run metadata writes paths, they are written RELATIVE to the durable root.
# These tests pin the helper contract and the no-absolute-paths assertion.

test_that("is_absolute_path recognises drive, UNC, and posix forms only", {
  expect_true(is_absolute_path("C:/Users/calum/data/x.zip"))
  expect_true(is_absolute_path("C:\\data\\x.zip"))
  expect_true(is_absolute_path("//server/share/x.zip"))
  expect_true(is_absolute_path("/home/calum/x.zip"))
  expect_false(is_absolute_path("data/downloads/derived/feed.zip"))
  expect_false(is_absolute_path("./data"))
  expect_false(is_absolute_path("../data"))
  expect_false(is_absolute_path("-Xmx24G"))
  expect_false(is_absolute_path("2025-11-18T08:00:00Z"))
  expect_false(is_absolute_path(NA_character_))
})

test_that("portable_path relativises paths under the root and refuses strays", {
  root <- tempfile("durable-root-")
  dir.create(file.path(root, "downloads"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  p <- file.path(root, "downloads", "derived", "aleop__feed.zip")
  dir.create(dirname(p), recursive = TRUE)
  file.create(p)

  rel <- portable_path(p, root)
  expect_identical(rel, "downloads/derived/aleop__feed.zip")
  expect_false(is_absolute_path(rel))

  # The root itself maps to "" (a path relative to itself).
  expect_identical(portable_path(root, root), "")

  # A path outside the root is an error, not a silent leak.
  stray <- tempfile("elsewhere-")
  on.exit(unlink(stray, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(portable_path(file.path(stray, "x"), root), "not under")
})

test_that("make_metadata_portable rewrites nested absolute paths, nothing else", {
  root <- tempfile("durable-root-")
  dir.create(file.path(root, "acquired", "osm", "network_x"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  meta <- list(
    network_dir = file.path(root, "acquired", "osm", "network_x"),
    network_dat = paste0(normalizePath(file.path(root, "acquired"),
                                       mustWork = FALSE),
                         "/network.dat"),
    files = c(file.path(root, "matrice", "walk_1.parquet"),
              file.path(root, "matrice", "car_1.parquet")),
    cap_minutes = 20L,
    ladder_rungs = c(5L, 10L, 15L, 20L),
    routing_parameters = list(
      elevation = list(setting = "NONE", dem_path = NULL),
      transit = list(departure_datetime = "2025-11-18T08:00:00+0000",
                     note = "-Xmx24G is a heap flag, not a path"),
      W = 25000L,
      scope = "bretagne (departements 22/29/35/56)"
    ),
    gtfs = list(path = NULL, sha256 = "abc123")
  )

  out <- make_metadata_portable(meta, root)
  expect_identical(out$network_dir, "acquired/osm/network_x")
  expect_identical(out$network_dat, "acquired/network.dat")
  expect_identical(out$files,
                   c("matrice/walk_1.parquet", "matrice/car_1.parquet"))
  # Non-path fields pass through untouched.
  expect_identical(out$cap_minutes, 20L)
  expect_identical(out$routing_parameters$W, 25000L)
  expect_null(out$routing_parameters$elevation$dem_path)
  expect_identical(out$gtfs$path, NULL)
  expect_identical(out$gtfs$sha256, "abc123")

  # The produced structure carries no absolute path anywhere.
  expect_error(assert_no_absolute_paths(out), NA)
  # ...and the UNconverted original is exactly the defect the validator
  # exists to catch (the failed attempt's run metadata looked like this).
  expect_error(assert_no_absolute_paths(meta), "absolute path")
})

test_that("assert_no_absolute_paths catches leaks at any depth", {
  ok <- list(a = list(b = list(c = "data/x")))
  expect_error(assert_no_absolute_paths(ok), NA)

  leaky <- list(
    network_dir = "E:/some/worktree/data/acquired/osm/network_x",
    nested = list(pairs = c("relative.parquet",
                            "\\\\server\\share\\matrix.parquet"))
  )
  err <- tryCatch(assert_no_absolute_paths(leaky), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "absolute path")
  expect_match(conditionMessage(err), "network_dir")
  expect_match(conditionMessage(err), "matrix[.]parquet")
})
