# S14 — the durable run and network cache root (#19).
#
# The durable root is the main checkout's data/ tree — the pinned acquisition
# cache (downloads/, downloads/derived/), the acquired OSM crops, the r5r
# network caches, and resumable run state. It lives OUTSIDE every disposable
# git worktree (a worktree is a throwaway path; the expensive Bretagne network
# build must survive its removal), under the horloge lente rule: a rebuild is
# a release, never a cleanup casualty.
#
# Formalization = one sentinel file plus guardrails. The sentinel marks a
# directory as THE durable root; safe_remove refuses to delete anything that
# is, or resolves into, the marked directory — including through a filesystem
# junction/symlink alias (the classic cleanup escape). Ordinary temp
# directories are removed exactly as before.

#' The sentinel file name marking a durable cache root.
durable_root_sentinel_name <- function() ".durable-root.json"

#' The full path of a directory's durable-root sentinel.
#'
#' @param root Candidate durable root directory (default "data" — the main
#'   checkout's data/ tree).
durable_root_sentinel_path <- function(root = "data") {
  file.path(root, durable_root_sentinel_name())
}

#' Build the sentinel payload.
#'
#' version 1L records the schema; created_at is UTC ISO-8601 like every other
#' acquisition timestamp; description states what lives here; do_not_delete
#' is the instruction cleanup tooling must honour.
durable_root_sentinel_content <- function(created_at =
                                            format(Sys.time(),
                                                   "%Y-%m-%dT%H:%M:%SZ",
                                                   tz = "UTC")) {
  list(
    version = 1L,
    created_at = created_at,
    description = paste(
      "Durable once-run cache root: pinned acquisition manifest and cache",
      "(downloads/, derived/), acquired OSM crops, r5r network caches, and",
      "resumable run state. Lives outside every disposable git worktree",
      "(ticket #19)."
    ),
    do_not_delete = paste(
      "NEVER delete this directory or anything inside it during cleanup:",
      "it holds hours of expensive computation (the Bretagne network build)",
      "and the sha256-pinned inputs reproducibility depends on. Worktree",
      "removal must leave it untouched."
    )
  )
}

#' Mark a directory as THE durable cache root.
#'
#' Writes .durable-root.json into `root` (creating `root` if needed). The
#' write refuses to overwrite an existing sentinel unless `overwrite = TRUE`:
#' created_at records when the root became durable and must not be silently
#' refreshed. Returns the sentinel path invisibly. Idempotent on an existing,
#' matching sentinel.
write_durable_root_sentinel <- function(root = "data", overwrite = FALSE) {
  stopifnot(is.character(root), length(root) == 1L, !is.na(root), nzchar(root))
  stopifnot(is.logical(overwrite), length(overwrite) == 1L, !is.na(overwrite))
  if (!dir.exists(root)) dir.create(root, recursive = TRUE)
  p <- durable_root_sentinel_path(root)
  if (file.exists(p) && !isTRUE(overwrite)) {
    stop("a durable-root sentinel already exists at ", p,
         "; pass overwrite = TRUE to re-mark deliberately", call. = FALSE)
  }
  jsonlite::write_json(durable_root_sentinel_content(), p,
                       auto_unbox = TRUE, pretty = TRUE)
  invisible(p)
}

#' Does this directory carry the durable-root sentinel?
has_durable_root_sentinel <- function(path) {
  file.exists(durable_root_sentinel_path(path))
}

#' Find the nearest durable-root ancestor of a path, if any.
#'
#' Walks from `path` up through every ancestor of BOTH the literal path and
#' its fully-resolved form (normalizePath follows junctions/symlinks on
#' Windows where Sys.readlink cannot see them). Returns the deepest ancestor
#' directory carrying the sentinel, or NULL. This dual walk is what makes
#' alias escapes visible: a junction outside the root pointing INTO it
#' resolves to a path whose ancestors include the sentinel.
find_durable_ancestor <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, !is.na(path), nzchar(path))
  candidates <- unique(c(gsub("\\\\", "/", path),
                         gsub("\\\\", "/", normalizePath(path, mustWork = FALSE))))
  for (cand in candidates) {
    parts <- strsplit(sub("/+$", "", cand), "/", fixed = TRUE)[[1L]]
    for (k in seq_along(parts)) {
      dir <- paste(parts[seq_len(length(parts) - k + 1L)], collapse = "/")
      if (nzchar(dir) && dir.exists(dir) && has_durable_root_sentinel(dir)) {
        return(dir)
      }
    }
  }
  NULL
}

#' Refuse any destructive operation targeting the durable root.
#'
#' Errors when `path` is the durable root or sits anywhere beneath it (in
#' literal or resolved terms). Invisible TRUE otherwise. The guardrail every
#' cleanup entry point calls BEFORE touching bytes.
assert_not_durable_root <- function(path) {
  hit <- find_durable_ancestor(path)
  if (!is.null(hit)) {
    stop(sprintf(
      "refusing: '%s' is or sits under the durable cache root '%s' (%s sentinel, ticket #19) — worktree removal must leave durable inputs, networks, and run state untouched",
      path, hit, durable_root_sentinel_name()
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Detect a filesystem link (symlink or Windows junction) at a path.
#'
#' Sys.readlink() reports symbolic links but NOT NTFS junctions; on Windows a
#' second probe reads LinkType via PowerShell (-EncodedCommand keeps quoting
#' out of the picture). Returns "symbolic-link", "junction", or NULL. A NULL
#' here degrades safely: the resolved-path walk in find_durable_ancestor
#' still catches escapes into the durable root.
fs_link_type <- function(path) {
  sl <- suppressWarnings(Sys.readlink(path))
  if (length(sl) == 1L && !is.na(sl) && nzchar(sl)) return("symbolic-link")
  if (.Platform$OS.type == "windows" && file.exists(path)) {
    ps <- sprintf(
      "$i = Get-Item -LiteralPath '%s' -Force; if ($i.LinkType) { $i.LinkType }",
      gsub("'", "''", path)
    )
    b <- iconv(ps, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1L]]
    out <- suppressWarnings(system2(
      "powershell", c("-NoProfile", "-EncodedCommand", jsonlite::base64_enc(b)),
      stdout = TRUE, stderr = FALSE
    ))
    lt <- trimws(out)
    lt <- lt[nzchar(lt)]
    if (length(lt)) {
      t <- tolower(lt[length(lt)])
      if (t %in% c("symboliclink", "junction")) return(t)
    }
  }
  NULL
}

#' Remove a path — refusing anything durable.
#'
#' The cleanup-guardrail API (#19): deletion of the durable root, anything
#' nested under it, or any alias resolving into it is REFUSED with an error;
#' ordinary temp directories are removed normally.
#'
#' @param path Single path to remove.
#' @param recursive Passed to unlink() for ordinary directories.
#' @param force_links Override for a path that IS itself a symlink/junction
#'   OUTSIDE any durable root: the default (FALSE) refuses to recurse through
#'   a link; TRUE removes the link only (via cmd rmdir on Windows, which never
#'   follows junctions), leaving link targets intact. No override can reach
#'   the durable root — the resolved-path refusal above runs first and has no
#'   bypass.
#'
#' @return Invisible FALSE when the path does not exist (nothing to do),
#'   invisible TRUE on success.
safe_remove <- function(path, recursive = TRUE, force_links = FALSE) {
  stopifnot(is.character(path), length(path) == 1L, !is.na(path), nzchar(path))
  stopifnot(is.logical(recursive), length(recursive) == 1L, !is.na(recursive))
  stopifnot(is.logical(force_links), length(force_links) == 1L,
            !is.na(force_links))
  if (!file.exists(path) && !dir.exists(path)) return(invisible(FALSE))

  # Guardrail first, always: literal AND resolved ancestors (no bypass).
  assert_not_durable_root(path)

  # A path that is itself a link: refuse recursion by default. Deleting
  # THROUGH an alias is how guardrails get bypassed; removing the link alone
  # needs the explicit override.
  link <- fs_link_type(path)
  if (!is.null(link)) {
    if (!isTRUE(force_links)) {
      stop(sprintf(
        "refusing to delete '%s': it is a %s — remove it explicitly with force_links = TRUE (the target is left intact), after checking it aliases nothing durable",
        path, link
      ), call. = FALSE)
    }
    if (.Platform$OS.type == "windows") {
      st <- system2("cmd", c("/c", "rmdir", shQuote(path)))
      if (!identical(as.integer(st), 0L)) {
        stop("cmd rmdir failed for junction '", path, "'", call. = FALSE)
      }
      return(invisible(TRUE))
    }
    return(invisible(unlink(path, recursive = FALSE) == 0L))
  }

  unlink(path, recursive = recursive)
  if (file.exists(path) || dir.exists(path)) {
    stop("failed to remove '", path, "'", call. = FALSE)
  }
  invisible(TRUE)
}
