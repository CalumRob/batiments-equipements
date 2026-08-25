# 22-repair-zip-layout.R — DEFECT C (the #22 gate, second residue):
# #25's derived gtfsx_ zips stored entries as "./<file>.txt"; R5's GTFS
# loader treats the leading "./" as a SUBDIRECTORY -> TableInSubdirectory
# (HIGH) + MissingTable (MEDIUM) for every table of every affected feed.
# Repair: repackage each affected zip with FLAT entry names — content
# byte-identical, entry names only — verify per-entry byte equality, commit,
# and emit the manifest sha patch proposal.

suppressMessages(library(data.table))
options(width = 200)

MAIN <- "E:/batiments-equipements"
DATA <- file.path(MAIN, "data")
WT   <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
OUT  <- file.path(WT, ".scratch", "tracer-matrice", "research", "outputs")

sha256 <- function(path) digest::digest(file = path, algo = "sha256")
utc_stamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

m <- jsonlite::fromJSON(file.path(DATA, "manifest.json"), simplifyVector = FALSE)
gtfsx_ids <- grep("^gtfsx_", names(m[["sources"]]), value = TRUE)

report <- list(); patched <- list()
for (id in gtfsx_ids) {
  e <- m[["sources"]][[id]]
  zpath <- file.path(MAIN, e[["cached_path"]])
  nm <- utils::unzip(zpath, list = TRUE)[["Name"]]
  nm <- nm[nm != "" & !grepl("/$", nm)]
  needs <- any(grepl("^[.]/", nm))
  if (!needs) {
    report[[length(report) + 1L]] <- data.frame(
      id = id, entries = length(nm), note = "already flat"); next
  }
  tmp <- file.path(tempdir(), paste0("flat-", id))
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(zpath, exdir = tmp, overwrite = TRUE)
  files <- setdiff(list.files(tmp, recursive = TRUE), "rebuilt.zip")
  stopifnot(length(files) == length(nm))  # extraction flattens ./: 1:1 with entries
  znew <- file.path(tmp, "rebuilt.zip")
  zip::zip(zipfile = znew, files = files, root = tmp, recurse = TRUE)

  # byte-for-byte content equality per entry (names flattened, bytes same)
  ok_all <- TRUE
  for (f in files) {
    a <- digest::digest(file = file.path(tmp, f), algo = "sha256")
    vtmp <- file.path(tmp, "v"); dir.create(vtmp)
    utils::unzip(znew, files = f, exdir = vtmp, overwrite = TRUE)
    b <- digest::digest(file = file.path(vtmp, f), algo = "sha256")
    unlink(vtmp, recursive = TRUE)
    if (!identical(a, b)) { ok_all <- FALSE; break }
  }
  if (!ok_all) {
    report[[length(report) + 1L]] <- data.frame(
      id = id, entries = length(nm), note = "BYTE MISMATCH - NOT COMMITTED")
    unlink(tmp, recursive = TRUE); next
  }
  stopifnot(file.copy(znew, zpath, overwrite = TRUE))
  unlink(tmp, recursive = TRUE)
  report[[length(report) + 1L]] <- data.frame(
    id = id, entries = length(nm), note = "FLATTENED")
  patched[[id]] <- list(
    sha256 = sha256(zpath),
    size_bytes = as.integer(file.size(zpath)),
    acquired_at = utc_stamp(),
    note = "entry names flattened (leading './' removed); content unchanged"
  )
}

rep <- rbindlist(report, use.names = TRUE, fill = TRUE)
fwrite(rep, file.path(OUT, "22-layout-report.csv"))
print(rep[note != "already flat"])
jsonlite::write_json(patched, file.path(OUT, "22-layout-manifest-patch.json"),
                     auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("flattened %d / %d gtfsx_ pins\n",
            sum(rep$note == "FLATTENED"), nrow(rep)))
