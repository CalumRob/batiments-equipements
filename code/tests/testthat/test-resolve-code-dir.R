# Regression for the #22 probe's first-launch failure: children silently
# sourced nothing when the orchestrator ran from a worktree ROOT (code_dir
# resolved to <root>/R, which does not exist). resolve_code_dir must find
# the package sources from BOTH invocation points and always answer with an
# absolute forward-slash path, so a child's own cwd can never matter.

test_that("resolve_code_dir finds the package from the repo root and from code/", {
  # testthat evaluates files from tests/testthat; derive the actual repository
  # root instead of assuming getwd() is the package directory.
  root <- normalizePath(testthat::test_path("../../.."), winslash = "/")
  code <- normalizePath(file.path(root, "code"), winslash = "/")

  from_root <- resolve_code_dir(root)
  expect_identical(from_root, code)
  expect_true(grepl("/", from_root, fixed = TRUE))   # forward slashes
  expect_false(grepl("\\\\", from_root))             # never backslashes

  # Invoked from the package directory itself: identity.
  expect_identical(resolve_code_dir(code), code)
})

test_that("resolve_code_dir refuses a cwd with neither R/ nor code/R", {
  empty <- file.path(tempdir(), paste0("nocode-", Sys.getpid()))
  unlink(empty, recursive = TRUE); dir.create(empty)
  on.exit(unlink(empty, recursive = TRUE), add = TRUE)
  expect_error(resolve_code_dir(empty), "cannot find|does not exist|exist")
})
