# Shared fixtures for the #19 cache-seam tests: a miniature but
# structurally-faithful promoted manifest, mirroring the REAL data/manifest.json
# produced by ticket 25 — pin_key_role groups (primary-current / primary-D1 /
# reference-current / auxiliary-archive), derived namespaced feeds with ids
# starting gtfsx_ and a "prefix" field, cached_path recorded relative to the
# durable root ("data/..."), and sha256 pins over real bytes on disk.

fake_feed_bytes <- function(tag) charToRaw(paste0("GTFS-FIXTURE-BYTES-", tag))

write_fake_feed <- function(path, tag) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(fake_feed_bytes(tag), path)
  path
}

fixture_entry <- function(id, rel_path, tag, role = NULL,
                          readers = "r5r-transit", prefix = NULL) {
  e <- list(
    id = id,
    source = paste0("https://example.invalid/", id),
    millesime = "fixture",
    sha256 = digest::digest(fake_feed_bytes(tag), algo = "sha256", serialize = FALSE),
    size_bytes = length(fake_feed_bytes(tag)),
    acquired_at = "2026-08-23T00:00:00Z",
    cached_path = rel_path,
    readers = readers
  )
  if (!is.null(role)) e$pin_key_role <- role
  if (!is.null(prefix)) e$prefix <- prefix
  e
}

#' Build the miniature promoted manifest.
#'
#' Layout under `root` (the fake durable checkout root):
#'   root/data/manifest.json
#'   root/data/downloads/gtfs-original-a.zip          (primary-current)
#'   root/data/downloads/gtfs-d1-b.zip                (primary-D1)
#'   root/data/downloads/derived/a__feed-a.zip        (gtfsx_a, prefix a)
#'   root/data/downloads/derived/b__feed-b.zip        (gtfsx_b, prefix b)
#'   root/data/downloads/reference-c.zip              (reference-current rival)
#'   root/data/downloads/archive-d.zip                (auxiliary-archive)
#'   root/data/downloads/bpe-universe.parquet         (readers=bpe)
#' Returns list(root, data_dir, manifest_path, entries).
fixture_promoted_manifest <- function(root = tempfile("cache-root-")) {
  data_dir <- file.path(root, "data")
  dl <- file.path(data_dir, "downloads")

  write_fake_feed(file.path(dl, "gtfs-original-a.zip"), "orig-A")
  write_fake_feed(file.path(dl, "gtfs-d1-b.zip"), "d1-B")
  write_fake_feed(file.path(dl, "derived", "a__feed-a.zip"), "der-a")
  write_fake_feed(file.path(dl, "derived", "b__feed-b.zip"), "der-b")
  write_fake_feed(file.path(dl, "reference-c.zip"), "ref-C")
  write_fake_feed(file.path(dl, "archive-d.zip"), "aux-D")
  write_fake_feed(file.path(dl, "bpe-universe.parquet"), "bpe")

  # The real manifest keys `sources` by id — name the list or the JSON
  # round-trip degrades it to an array.
  entries <- list(
    fixture_entry("gtfs-original-a", "data/downloads/gtfs-original-a.zip",
                  "orig-A", role = "primary-current"),
    fixture_entry("gtfsx_a", "data/downloads/derived/a__feed-a.zip",
                  "der-a", prefix = "a"),
    fixture_entry("gtfsx_b", "data/downloads/derived/b__feed-b.zip",
                  "der-b", prefix = "b"),
    fixture_entry("gtfs-d1-b", "data/downloads/gtfs-d1-b.zip",
                  "d1-B", role = "primary-D1"),
    fixture_entry("reference-c", "data/downloads/reference-c.zip",
                  "ref-C", role = "reference-current"),
    fixture_entry("archive-d", "data/downloads/archive-d.zip",
                  "aux-D", role = "auxiliary-archive"),
    fixture_entry("bpe_2025", "data/downloads/bpe-universe.parquet",
                  "bpe", readers = "bpe", role = NULL)
  )
  names(entries) <- vapply(entries, `[[`, character(1L), "id")

  manifest <- list(manifest_version = 1L, sources = entries)
  manifest_path <- file.path(data_dir, "manifest.json")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  list(root = root, data_dir = data_dir, manifest_path = manifest_path,
       entries = entries)
}

fixture_with_excluded_feeds <- function(fx) {
  excluded <- c(
    gtfsx_nemus = "der-nemus",
    gtfsx_des = "der-des",
    gtfsx_bferry = "der-bferry",
    gtfsx_nomad = "der-nomad",
    gtfsx_norm = "der-norm",
    gtfsx_ponto = "der-ponto"
  )
  for (id in names(excluded)) {
    prefix <- sub("^gtfsx_", "", id)
    write_fake_feed(file.path(fx$data_dir, "downloads", "derived",
                              paste0(prefix, "__feed-", prefix, ".zip")),
                    excluded[[id]])
  }
  manifest <- jsonlite::fromJSON(fx$manifest_path, simplifyVector = FALSE)
  for (id in names(excluded)) {
    prefix <- sub("^gtfsx_", "", id)
    manifest$sources[[id]] <- fixture_entry(
      id,
      paste0("data/downloads/derived/", prefix, "__feed-", prefix, ".zip"),
      excluded[[id]], prefix = prefix
    )
  }
  jsonlite::write_json(manifest, fx$manifest_path, auto_unbox = TRUE,
                       pretty = TRUE)
  fx
}
