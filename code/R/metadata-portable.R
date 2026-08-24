# S15 — metadata portability (#19).
#
# Rule: wherever network setup or run metadata writes paths, write them
# RELATIVE to the durable root. Absolute worktree paths in run_metadata.json
# were the known defect of the failed full-run attempt — dead references the
# moment the disposable worktree was removed. The helpers here are the
# enforcement seam; run_tracer applies them to its summary before it is
# serialized.

#' Is this string an absolute filesystem path?
#'
#' Recognises Windows drive-letter paths (C:/, C:\), UNC shares (\\\\server),
#' and POSIX absolute paths (/...). Everything else — relative paths,
#' "-Xmx24G"-style flags, timestamps — is not a path and is never rewritten.
is_absolute_path <- function(x) {
  if (!is.character(x)) return(rep(FALSE, length(x)))
  grepl("^[/\\\\]", x) | grepl("^[A-Za-z]:[/\\\\]", x)
}

#' Express a path relative to the durable root.
#'
#' Normalises both arguments to forward slashes and requires `path` to sit
#' under `root` (a stray path outside the durable root is an error, never a
#' silent leak). Returns the forward-slash relative form; "" for the root
#' itself.
portable_path <- function(path, root) {
  stopifnot(is.character(path), length(path) == 1L, !is.na(path), nzchar(path))
  stopifnot(is.character(root), length(root) == 1L, !is.na(root), nzchar(root))
  rp <- normalizePath(path, winslash = "/", mustWork = FALSE)
  rr <- normalizePath(root, winslash = "/", mustWork = TRUE)
  rr <- sub("/+$", "", rr)
  if (!identical(rp, rr)) {
    prefix <- paste0(rr, "/")
    if (!startsWith(rp, prefix)) {
      stop(sprintf("'%s' is not under the durable root '%s'", path, rr),
           call. = FALSE)
    }
    substring(rp, nchar(prefix) + 1L)
  } else {
    ""
  }
}

portable_path_maybe <- function(p, root) {
  tryCatch(portable_path(p, root), error = function(e) p)
}

under_root_ci <- function(rp, rr) {
  if (.Platform$OS.type == "windows") {
    startsWith(tolower(rp), paste0(tolower(rr), "/")) || identical(tolower(rp), tolower(rr))
  } else {
    startsWith(rp, paste0(rr, "/")) || identical(rp, rr)
  }
}

portable_path_lenient <- function(p, root) {
  rp <- gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
  rr <- sub("/+$", "", gsub("\\\\", "/",
                            normalizePath(root, winslash = "/", mustWork = TRUE)))
  if (under_root_ci(rp, rr)) portable_path(p, root) else p
}

#' Rewrite every absolute path in a metadata structure relative to `root`.
#'
#' Recurses lists and character vectors. A string that IS an absolute path
#' and lies under the durable root becomes relative (forward slashes); a
#' string outside the root is left verbatim — pairing with
#' assert_no_absolute_paths() is what turns that into an enforced contract
#' rather than a silent leak. Non-character values pass through untouched.
make_metadata_portable <- function(x, root) {
  if (is.list(x)) {
    return(lapply(x, make_metadata_portable, root = root))
  }
  if (is.character(x)) {
    abs <- is_absolute_path(x)
    x[abs] <- vapply(x[abs], portable_path_lenient, character(1L), root = root)
    return(x)
  }
  x
}

collect_absolute_strings <- function(x, where = "") {
  if (is.list(x)) {
    out <- character(0)
    for (nm in names(x)) {
      out <- c(out, collect_absolute_strings(x[[nm]],
                                             paste0(if (nzchar(where)) where else "metadata", "$", nm)))
    }
    return(out)
  }
  if (is.character(x)) {
    hits <- x[is_absolute_path(x)]
    if (length(hits)) return(paste0(where, ": ", hits))
  }
  character(0)
}

#' Assert a produced metadata structure contains no absolute paths.
#'
#' The #19 acceptance gate: no absolute path may appear anywhere in produced
#' metadata (they die with the disposable worktree). Errors naming each
#' offending field and value; invisible TRUE otherwise.
assert_no_absolute_paths <- function(x, label = "metadata") {
  leaks <- collect_absolute_strings(x, label)
  if (length(leaks)) {
    stop(sprintf(
      "%s leaks absolute path(s) — rewrite them relative to the durable root (#19): %s",
      label, paste(leaks, collapse = "; ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}
