# Wiring contracts for the #19 seams into the existing driver surface.
#
# The real-run path needs a JVM, so these follow the repo's deparse-contract
# convention (as in test-border-width-contract.R): pin that run_tracer's
# serialization goes through the portability seam, and unit-test the
# data-dir -> durable-root resolution directly.

test_that("durable_root_of_data_dir resolves relative and absolute data dirs", {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)

  expect_identical(
    normalizePath(durable_root_of_data_dir("data"), winslash = "/"),
    normalizePath(file.path(wd), winslash = "/")
  )
  abs <- file.path(tempdir(), "some-checkout", "data")
  dir.create(abs, recursive = TRUE)
  on.exit(unlink(file.path(tempdir(), "some-checkout"),
                 recursive = TRUE, force = TRUE), add = TRUE)
  expect_identical(
    normalizePath(durable_root_of_data_dir(abs), winslash = "/"),
    normalizePath(file.path(tempdir(), "some-checkout"), winslash = "/")
  )
})

test_that("run_tracer serializes its metadata through the portability seam", {
  body_text <- paste(deparse(body(run_tracer)), collapse = "\n")
  # The write must consume make_metadata_portable over the summary, with the
  # root derived from data_dir (#19: no absolute paths in produced metadata).
  expect_match(body_text, "make_metadata_portable\\(out, cache_root\\)",
               fixed = FALSE)
  expect_match(body_text, "cache_root <- durable_root_of_data_dir\\(data_dir\\)")
  expect_match(body_text,
               "portable_path\\(metadata_path, cache_root\\)")
})
