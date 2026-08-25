suppressMessages(library(data.table))
DL <- "E:/batiments-equipements/data/downloads"
for (z in c("versions-des-horaires-theoriques-des-lignes-de-bus-et-de-metro-du-reseau-star-dans-les-formats-gtfs-et-netex-ainsi-que-les-urls-dacces-au-gtfs-rt-1__83281.zip",
            "versions-des-horaires-theoriques-des-lignes-de-bus-et-de-metro-du-reseau-star-dans-les-formats-gtfs-et-netex-ainsi-que-les-urls-dacces-au-gtfs-rt-1__83282.zip")) {
  cat("\n==", substr(z, nchar(z) - 12, nchar(z)), "==\n")
  tmp <- file.path(tempdir(), paste0("star-", substr(z, nchar(z) - 5, nchar(z) - 4)))
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  r <- tryCatch({ utils::unzip(file.path(DL, z), exdir = tmp); "R-extract OK" },
           error = function(e) paste("R-extract FAIL:", conditionMessage(e)),
           warning = function(w) paste("R-extract WARN:", conditionMessage(w)))
  cat(r, "\n")
  fl <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  rd <- function(b) {
    f <- fl[grepl(paste0("(^|/)", b, "[.]txt$"), fl)][1]
    if (is.na(f)) NULL else fread(f, colClasses = "character", na.strings = "",
                                  showProgress = FALSE)
  }
  cal <- rd("calendar"); cd <- rd("calendar_dates"); tr <- rd("trips")
  cat("trips:", if (is.null(tr)) NA else nrow(tr),
      "| calendar rows:", if (is.null(cal)) NA else nrow(cal),
      "| calendar_dates rows:", if (is.null(cd)) NA else nrow(cd), "\n")
  if (!is.null(cal) && nrow(cal))
    cat("calendar range:", min(cal$start_date), "->", max(cal$end_date),
        "| covering 20260826:",
        sum(cal$start_date <= "20260826" & cal$end_date >= "20260826"), "\n")
  unlink(tmp, recursive = TRUE)
}
