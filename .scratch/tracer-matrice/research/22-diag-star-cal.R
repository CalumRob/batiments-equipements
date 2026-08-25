suppressMessages(library(data.table))
NET <- "E:/batiments-equipements/data/networks/cap20-current"
svc <- function(zfile) {
  cat("\n==", substr(basename(zfile), 1, 44), "==\n")
  tmp <- file.path(tempdir(), "star"); unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(zfile, exdir = tmp)
  fl <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  rd <- function(b) {
    f <- fl[grepl(paste0("(^|/)", b, "[.]txt$"), fl)][1]
    if (is.na(f)) NULL else fread(f, colClasses = "character", na.strings = "",
                                  showProgress = FALSE)
  }
  tr <- rd("trips"); cal <- rd("calendar"); cd <- rd("calendar_dates")
  sids <- unique(tr$service_id)
  incal <- if (!is.null(cal)) sids[sids %in% cal$service_id] else character(0)
  cat("trips:", nrow(tr), "| sids:", length(sids),
      "| calendar rows:", if (is.null(cal)) 0 else nrow(cal),
      "| cal_dates rows:", if (is.null(cd)) 0 else nrow(cd), "\n")
  if (!is.null(cal) && nrow(cal)) {
    cat("calendar range:", min(cal$start_date), "->", max(cal$end_date), "\n")
    act <- cal[start_date <= "20260826" & end_date >= "20260826"]
    cat("rows covering 2026-08-26:", nrow(act),
        "| wed=1:", sum(substr(act$wednesday, 1, 1) == "1"), "\n")
  } else cat("NO calendar rows\n")
  if (!is.null(cd) && nrow(cd)) {
    a <- cd[date == "20260826" & exception_type == "1"]
    cat("cal_dates ADDED on 2026-08-26:", nrow(a), "sids ->",
        sum(a$service_id %in% sids), "of our sids\n")
  }
  unlink(tmp, recursive = TRUE)
}
svc(file.path(NET, "star__versions-des-horaires-theoriques-des-lignes-de-bus-et-de-metro-du-reseau-star-dans-les-formats-gtfs-et-netex-ainsi-que-les-urls-dacces-au-gtfs-rt-1__83281.zip"))
