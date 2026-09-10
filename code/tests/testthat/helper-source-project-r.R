# Source implementation files when tests run from the source tree, while
# remaining compatible with R CMD check's installed-package test directory.
# The latter does not contain ../../R, but the package namespace does contain
# every implementation binding (including the unexported seams used here).
source_project_r <- function(path) {
  source_path <- testthat::test_path("../../R", path)
  target <- parent.frame()
  if (file.exists(source_path)) {
    return(source(source_path, local = target))
  }

  namespace <- asNamespace("batimentsequipements")
  for (name in ls(namespace, all.names = TRUE)) {
    assign(name, get(name, envir = namespace), envir = target)
  }
  invisible(NULL)
}
