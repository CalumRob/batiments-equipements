# 22-diag-bisect.R — attribute r5r's HIGH-priority GTFS errors to specific
# staged feeds using r5r's own validator, at toy-network speed.
#
# Method: one fresh Rscript JVM per candidate feed (JVM cannot restart
# in-process). Each iteration: temp dir = tiny Fougeres OSM crop + ONE
# staged zip -> link_network -> read that dir's gtfs_errors.csv -> record.
# The toy crop keeps OSM parsing seconds-scale so GTFS validation dominates.

WT <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
NET_DIR <- "E:/batiments-equipements/data/networks/cap20-current"
TOY_PBF <- "E:/batiments-equipements/data/acquired/osm/fougeres_crop_26.6km_dd918758f50e.osm.pbf"
OUT <- file.path(WT, ".scratch", "tracer-matrice", "research", "outputs")

CANDIDATES <- c(
  "base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip",
  "kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip",
  "arrets-horaires-et-circuits-des-lignes-de-transports-en-commun-en-pays-de-la-loire-gtfs-destineo-reseaux-aom-aleop-1.zip",
  "des__arrets-horaires-et-circuits-des-lignes-de-transports-en-commun-en-pays-de-la-loire-gtfs-destineo-reseaux-aom-aleop-1.zip",
  "base-de-donnees-multimodale-des-reseaux-de-transport-public-normands.zip",
  "norm__base-de-donnees-multimodale-des-reseaux-de-transport-public-normands.zip",
  "arrets-horaires-et-circuits-des-lignes-de-transports-aleop-1.zip",
  "aleop__arrets-horaires-et-circuits-des-lignes-de-transports-aleop-1.zip",
  "donnees-theoriques-du-reseau-urbain-de-flers-agglo-au-format-gtfs.zip",
  "flers__donnees-theoriques-du-reseau-urbain-de-flers-agglo-au-format-gtfs.zip",
  "lineotim-morlaix-communaute.zip",
  "lineotim__lineotim-morlaix-communaute.zip",
  "reseau-urbain-kiceo.zip",
  "kiceo__reseau-urbain-kiceo.zip",
  "navette-a-la-voile-pour-les-glenan.zip"
)

one_probe <- function(zip_name) {
  tmp <- file.path(tempdir(), paste0("bisect-", format(Sys.time(), "%H%M%S")))
  unlink(tmp, recursive = TRUE); dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(TOY_PBF, file.path(tmp, "toy.osm.pbf"))
  file.copy(file.path(NET_DIR, zip_name), file.path(tmp, "feed.zip"))
  code <- sprintf(
    paste0(
      "options(java.parameters='-Xmx2G'); ",
      "for (f in sort(list.files('%s', pattern='[.]R$', full.names=TRUE))) source(f); ",
      "net <- tryCatch(link_network(data_path='%s', elevation='NONE', verbose=FALSE), ",
      "error=function(e) {cat('BUILDFAIL:', conditionMessage(e), '\\n'); NULL}); ",
      "if (!is.null(net)) r5r::stop_r5(net)"),
    gsub("\\\\", "/", file.path(WT, "code", "R")), gsub("\\\\", "/", tmp))
  errf <- file.path(tempdir(), paste0("bisect-err-", format(Sys.time(), "%H%M%S"), ".log"))
  st <- system2("Rscript", c("-e", shQuote(code)), stdout = FALSE,
                stderr = errf)
  if (!identical(as.integer(st), 0L)) {
    cat("  (stderr tail: ", tail(readLines(errf), 2), ")\n", sep = "\n")
  }
  err <- file.path(tmp, "gtfs_errors.csv")
  n_high <- NA_integer_; n_rows <- NA_integer_
  if (file.exists(err)) {
    e <- data.table::fread(err, showProgress = FALSE)
    n_rows <- nrow(e)
    n_high <- sum(e[["priority"]] == "HIGH")
  }
  data.frame(feed = zip_name, exit = st, error_rows = n_rows,
             high_priority = n_high)
}

results <- list()
for (z in CANDIDATES) {
  cat("probing:", z, "...\n")
  results[[length(results) + 1L]] <- tryCatch(one_probe(z),
    error = function(e) data.frame(feed = z, exit = -1L,
                                   error_rows = NA_integer_,
                                   high_priority = NA_integer_))
}
out <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
data.table::setorder(out, -high_priority)
data.table::fwrite(out, file.path(OUT, "22-diag-bisect.csv"))
cat("\n=== RESULT ===\n"); print(out)
