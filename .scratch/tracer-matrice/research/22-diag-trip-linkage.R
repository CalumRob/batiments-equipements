suppressMessages(library(data.table))
NET <- "E:/batiments-equipements/data/networks/cap20-current"
linkage <- function(zfile) {
  cat("\n==", substr(basename(zfile), 1, 30), "==\n")
  tmp <- file.path(tempdir(), "lnk"); unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(zfile, exdir = tmp)
  fl <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  rd <- function(b) {
    f <- fl[grepl(paste0("(^|/)", b, "[.]txt$"), fl)][1]
    if (is.na(f)) NULL else fread(f, colClasses = "character", na.strings = "",
                                  showProgress = FALSE)
  }
  tr <- rd("trips"); st <- rd("stop_times"); ro <- rd("routes")
  unlink(tmp, recursive = TRUE)
  if (is.null(tr) || is.null(st)) { cat("missing tables\n"); return(invisible()) }
  tids <- unique(tr$trip_id); stids <- unique(st$trip_id)
  cat(sprintf("trips %d | trip_ids %d | stop_times rows %d | st trip_ids %d\n",
              nrow(tr), length(tids), nrow(st), length(stids)))
  cat("trip_id linkage:", sum(tids %in% stids), "/", length(tids),
      "| orphans in stop_times:", sum(!(stids %in% tids)), "\n")
  if (!is.null(ro)) {
    rids <- unique(tr$route_id)
    cat("route_id linkage:", sum(rids %in% ro$route_id), "/", length(rids), "\n")
  }
  # prefix shape check
  pfx_t <- unique(sub(":.*", "", head(tids, 1000)))
  pfx_s <- unique(sub(":.*", "", head(stids, 2000)))
  cat("id prefixes trips:", paste(head(pfx_t, 3)),
      "| stop_times:", paste(head(pfx_s, 3)), "\n")
}
linkage(file.path(NET, "bibus__horaires-theoriques-et-temps-reel-des-bus-et-tramways-circulant-sur-le-territoire-de-brest-metropole.zip"))
linkage(file.path(NET, "kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip"))
linkage(file.path(NET, "des__arrets-horaires-et-circuits-des-lignes-de-transports-en-commun-en-pays-de-la-loire-gtfs-destineo-reseaux-aom-aleop-1.zip"))
