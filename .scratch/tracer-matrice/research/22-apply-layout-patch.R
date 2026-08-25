MAIN <- "E:/batiments-equipements"; DATA <- file.path(MAIN, "data")
patch <- jsonlite::fromJSON(
  "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22/.scratch/tracer-matrice/research/outputs/22-layout-manifest-patch.json",
  simplifyVector = FALSE)
mpath <- file.path(DATA, "manifest.json")
file.copy(mpath, paste0(mpath, ".bak-22layout"), overwrite = TRUE)
m <- jsonlite::fromJSON(mpath, simplifyVector = FALSE)
for (id in names(patch)) {
  p <- patch[[id]]; e <- m[["sources"]][[id]]
  e[["provenance"]] <- paste0(
    e[["provenance"]],
    " | REPACKED 2026-08-24 under ticket #22 gate: zip entry names flattened (leading ./ removed; R5 read it as a subdirectory and refused HIGH TableInSubdirectoryError) - content byte-identical, supersedes sha ",
    e[["sha256"]])
  e[["sha256"]] <- p$sha256
  e[["size_bytes"]] <- p$size_bytes
  e[["acquired_at"]] <- p$acquired_at
  m[["sources"]][[id]] <- e
}
tmp <- paste0(mpath, ".tmp")
jsonlite::write_json(m, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
ok <- file.rename(tmp, mpath)
if (!ok) { file.copy(tmp, mpath, overwrite = TRUE); unlink(tmp) }
m2 <- jsonlite::fromJSON(mpath, simplifyVector = FALSE)
n <- 0
for (id in names(patch)) {
  e <- m2[["sources"]][[id]]
  z <- file.path(MAIN, e[["cached_path"]])
  stopifnot(identical(digest::digest(file = z, algo = "sha256"), e[["sha256"]]))
  n <- n + 1L
}
cat("repinned", n, "pins; all shas verified on disk\n")
