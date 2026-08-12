# S5 — the acquisition manifest + cache discipline: download → integrity
# check → cache, per source. Shared machinery for the three acquisition
# readers (BPE, BDNB, OSM); paths are always explicit parameters — the
# Rscript entry points run from the repo root, never from a fixed cwd.

#' Compute the lowercase hex sha256 of a file.
#'
#' The integrity primitive of the acquisition discipline: every downloaded
#' artifact is hashed before it is allowed into the cache, and the pin is
#' enforced on every later run (see acquire_source).
sha256_file <- function(path) {
  if (!file.exists(path)) {
    stop("file not found: ", path, call. = FALSE)
  }
  digest::digest(file = path, algo = "sha256")
}

#' The canonical empty manifest.
#'
#' manifest_version = 1L; `sources` is a named list (id -> entry) that grows
#' as register_source / acquire_source add entries.
empty_manifest <- function() {
  list(
    manifest_version = 1L,
    sources = list()
  )
}

#' Load the acquisition manifest from a JSON file.
#'
#' Reads a pretty-printed manifest written by manifest_save
#' (jsonlite::fromJSON, simplifyVector = TRUE). If `path` does not exist,
#' returns the canonical empty manifest — the loader doubles as the
#' initializer, so acquire_source never special-cases a first run. The
#' `sources` object is normalised back to a named list of lists regardless of
#' how jsonlite simplified it (a rectangular set of fully-acquired entries
#' comes back as a data.frame and is rebuilt row-wise).
manifest_load <- function(path) {
  if (!file.exists(path)) {
    return(empty_manifest())
  }
  m <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  if (is.null(m[["manifest_version"]])) {
    m$manifest_version <- 1L
  }
  src <- m[["sources"]]
  if (is.null(src)) {
    m$sources <- list()
  } else if (is.data.frame(src)) {
    ids <- rownames(src)
    entries <- lapply(seq_len(nrow(src)), function(i) as.list(src[i, , drop = FALSE]))
    names(entries) <- ids
    m$sources <- entries
  } else if (!is.list(src)) {
    stop("manifest sources must be an object of source entries", call. = FALSE)
  }
  m
}

#' Save the acquisition manifest as pretty JSON.
#'
#' Writes `manifest` to `path` (jsonlite::toJSON, pretty + auto_unbox) and
#' creates the parent directory if needed.
manifest_save <- function(manifest, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    jsonlite::toJSON(manifest, pretty = TRUE, auto_unbox = TRUE),
    path
  )
  invisible(path)
}

#' Register (or overwrite) a source entry in the manifest.
#'
#' Adds an entry for `id` with the source URL, the pinned millésime, and the
#' consuming `readers` (e.g. "bpe"); the acquisition fields (sha256,
#' size_bytes, acquired_at, cached_path) stay NULL until acquire_source fills
#' them. Returns the updated manifest — the input is not modified in place.
register_source <- function(manifest, id, source, millesime, readers) {
  manifest$sources[[id]] <- list(
    id = id,
    source = source,
    millesime = millesime,
    sha256 = NULL,
    size_bytes = NULL,
    acquired_at = NULL,
    cached_path = NULL,
    readers = readers
  )
  manifest
}

#' The pinned sha256 of a manifest entry, or NULL when unpinned.
#'
#' A placeholder pin (NULL after a fresh register_source, NA after a JSON
#' round-trip) is treated as "no pin yet".
pin_of <- function(manifest, id) {
  entry <- manifest$sources[[id]]
  if (is.null(entry)) {
    return(NULL)
  }
  p <- entry$sha256
  if (is.null(p) || length(p) != 1L || is.na(p) || !nzchar(p)) {
    return(NULL)
  }
  as.character(p)
}

#' Acquire one source: download → integrity check → cache, idempotently.
#'
#' The load-bearing function of the acquisition discipline, shared by the
#' three readers (BPE, BDNB, OSM). On a cache hit the cached file is returned
#' without touching the network; a corrupt/stale cache is deleted and
#' re-acquired; every download passes the integrity gate before promotion.
#' Hash enforcement: with a pinned manifest hash the observed hash MUST equal
#' it (upstream drift is a hard error — the source changed under the pinned
#' millésime and must be re-pinned deliberately); with `expected_sha256` and
#' no pin the observed hash MUST equal it; with neither (first acquisition)
#' the observed hash is accepted and pinned — self-pin at first acquisition,
#' enforce thereafter.
acquire_source <- function(id, source, millesime, dest_dir, manifest_path,
                           expected_sha256 = NULL, readers = character(0)) {
  manifest <- manifest_load(manifest_path)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  cache <- file.path(dest_dir, id)
  pin <- pin_of(manifest, id)

  # Pin vs expected agreement — evaluated on EVERY call, cache hit or not: a
  # caller passing an expected hash that contradicts the manifest pin must
  # fail loudly, never pass silently on a cache hit.
  if (!is.null(pin) && !is.null(expected_sha256) && !identical(pin, expected_sha256)) {
    stop(sprintf(
      "pin vs expected mismatch for %s: manifest pins %s but expected_sha256 is %s — the pin and the expected hash disagree and must be re-pinned deliberately",
      id, pin, expected_sha256
    ), call. = FALSE)
  }

  # Cache hit: the cached file already satisfies the pin (or the expected
  # hash when nothing is pinned yet) — nothing to do.
  if (file.exists(cache)) {
    cache_hash <- sha256_file(cache)
    hit <- identical(cache_hash, pin) ||
      (is.null(pin) && identical(cache_hash, expected_sha256))
    if (hit) {
      message(sprintf("cache hit for %s", id))
      return(invisible(cache))
    }
    # Corrupt/stale cache: its hash matches neither the pin nor the expected.
    if (!identical(cache_hash, pin) && !identical(cache_hash, expected_sha256)) {
      message(sprintf("removing corrupt cache for %s", id))
      unlink(cache)
    }
  }

  # Download to a temp file next to the cache target.
  tmp <- tempfile(tmpdir = dirname(cache))
  status <- tryCatch(
    utils::download.file(url = source, destfile = tmp, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(status, "error") || !identical(as.integer(status), 0L)) {
    unlink(tmp)
    msg <- if (inherits(status, "error")) {
      conditionMessage(status)
    } else {
      paste("exit code", status)
    }
    stop("download failed for ", id, ": ", msg, call. = FALSE)
  }
  observed <- sha256_file(tmp)

  # Integrity gate (the enforcement point). The pin vs expected agreement was
  # already checked up front; here, the observed hash must match the pin (or
  # the expected hash when nothing is pinned yet).
  if (!is.null(pin)) {
    if (!identical(observed, pin)) {
      unlink(tmp)
      stop(sprintf(
        "upstream drift detected for %s: pinned %s but observed %s — the source changed under the pinned millésime and must be re-pinned deliberately",
        id, pin, observed
      ), call. = FALSE)
    }
  } else if (!is.null(expected_sha256)) {
    if (!identical(observed, expected_sha256)) {
      unlink(tmp)
      stop(sprintf(
        "integrity check failed for %s: expected %s, got %s",
        id, expected_sha256, observed
      ), call. = FALSE)
    }
  }

  # Promote: move the temp file into the cache path.
  if (!isTRUE(file.rename(tmp, cache))) {
    if (!isTRUE(file.copy(tmp, cache, overwrite = TRUE))) {
      unlink(tmp)
      stop("failed to promote downloaded file into cache for ", id, call. = FALSE)
    }
    unlink(tmp)
  }

  # Record and save.
  manifest$sources[[id]] <- list(
    id = id,
    source = source,
    millesime = millesime,
    sha256 = observed,
    size_bytes = as.numeric(file.info(cache)$size),
    acquired_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    cached_path = cache,
    readers = readers
  )
  manifest_save(manifest, manifest_path)
  invisible(cache)
}
