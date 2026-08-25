suppressMessages(library(data.table))
NET <- "E:/batiments-equipements/data/networks/cap20-current"

probe_zip <- function(z) {
  cat("\n==", substr(basename(z), 1, 30), "==\n")
  nm <- utils::unzip(z, list = TRUE)[["Name"]]
  cat("entries:", paste(head(nm, 12), collapse = " | "), "\n")
  tmp <- file.path(tempdir(), "svc2"); unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(z, exdir = tmp)
  fl <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  rd <- function(b) {
    f <- fl[grepl(paste0("/", b, ".txt$|^", b, ".txt$"), fl)][1]
    if (is.na(f)) return(NULL)
    fread(f, colClasses = "character", showProgress = FALSE, na.strings = "")
  }
  tr <- rd("trips"); cal <- rd("calendar"); cd <- rd("calendar_dates")
  if (is.null(tr)) { cat("NO trips.txt\n"); return(invisible()) }
  sids <- unique(tr$service_id)
  incal <- if (!is.null(cal)) sum(sids %in% cal$service_id) else 0L
  incd <- if (!is.null(cd)) sum(sids %in% cd$service_id) else 0L
  cat("trip rows:", nrow(tr), "| distinct service_ids:", length(sids),
      "| covered by calendar:", incal, "| by calendar_dates:", incd, "\n")
  cat("sample sid:", head(sids, 3), "\n")
  if (!is.null(cal) && nrow(cal)) {
    cat("calendar window:", min(cal$start_date), "->", max(cal$end_date), "\n")
  }
  unlink(tmp, recursive = TRUE)
}

probe_zip(file.path(NET, "star__versions-des-horaires-theoriques-des-lignes-de-bus-et-de-metro-du-reseau-star-dans-les-formats-gtfs-et-netex-ainsi-que-les-urls-dacces-au-gtfs-rt-1__83281.zip"))
probe_zip(file.path(NET, "kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip"))
probe_zip(file.path(NET, "bibus__horaires-theoriques-et-temps-reel-des-bus-et-tramways-circulant-sur-le-territoire-de-brest-metropole.zip"))

cat("\nr5r::check_transit_availability args:\n")
print(args(r5r::check_transit_availability))
