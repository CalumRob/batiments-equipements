# 22-apply-manifest-patch.R — surgically update the promoted manifest's 18
# repaired gtfsx_ pins: same ids, new sha256/size_bytes/acquired_at, a
# provenance suffix recording the #22-gate repair. Atomic write + backup;
# every other entry untouched.

MAIN <- "E:/batiments-equipements"
DATA <- file.path(MAIN, "data")
WT   <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
OUT  <- file.path(WT, ".scratch", "tracer-matrice", "research", "outputs")

patch <- jsonlite::fromJSON(file.path(OUT, "22-repair-manifest-patch.json"),
                            simplifyVector = FALSE)
mpath <- file.path(DATA, "manifest.json")
backup <- paste0(mpath, ".bak-22repair")
file.copy(mpath, backup, overwrite = TRUE)

m <- jsonlite::fromJSON(mpath, simplifyVector = FALSE)
applied <- 0L
for (id in names(patch)) {
  p <- patch[[id]]
  e <- m[["sources"]][[id]]
  stopifnot(!is.null(e))
  old_sha <- e[["sha256"]]
  e[["sha256"]] <- p$sha256
  e[["size_bytes"]] <- p$size_bytes
  e[["acquired_at"]] <- p$acquired_at
  e[["provenance"]] <- paste0(
    e[["provenance"]],
    sprintf(" | REPAIRED 2026-08-24 under ticket #22 gate: stops.txt referential closure restored (parent stations re-added from the raw source artifact; +%d stops) after r5r HIGH-priority parent_station integrity rejection — supersedes sha %s",
            p$added_stops, old_sha))
  m[["sources"]][[id]] <- e
  applied <- applied + 1L
}

tmp <- paste0(mpath, ".tmp.pid", Sys.getpid())
jsonlite::write_json(m, tmp, auto_unbox = TRUE, pretty = TRUE,
                     null = "null")
ok <- file.rename(tmp, mpath)
if (!ok) { file.copy(tmp, mpath, overwrite = TRUE); unlink(tmp) }
cat(sprintf("manifest updated: %d pins repinned (backup at %s)\n",
            applied, backup))

# verify the manifest still parses AND every patched sha matches disk now
m2 <- jsonlite::fromJSON(mpath, simplifyVector = FALSE)
for (id in names(patch)) {
  e <- m2[["sources"]][[id]]
  z <- file.path(MAIN, e[["cached_path"]])
  h <- digest::digest(file = z, algo = "sha256")
  stopifnot(identical(h, e[["sha256"]]))
}
cat("verification: all repinned shas match artifacts on disk\n")
