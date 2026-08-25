suppressMessages(library(data.table))
NET <- "E:/batiments-equipements/data/networks/cap20-current"
probe_zip <- function(zfile) {
  cat("\n==", substr(basename(zfile), 1, 34), "==\n")
  tmp <- file.path(tempdir(), "svc3"); unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(zfile, exdir = tmp)
  fl <- list.files(tmp, recursive = TRUE, full.names = TRUE)
  rd <- function(b) {
    f <- fl[grepl(paste0("(^|/)", b, "[.]txt$"), fl)][1]
    if (is.na(f)) NULL else fread(f, colClasses = "character", na.strings = "",
                                  showProgress = FALSE)
  }
  tr <- rd("trips"); cal <- rd("calendar"); cd <- rd("calendar_dates")
  if (is.null(tr) || !nrow(tr)) { cat("NO TRIP ROWS\n"); return(invisible()) }
  sids <- unique(tr$service_id)
  cov_c <- if (!is.null(cal)) sum(sids %in% cal$service_id) else 0L
  cov_d <- if (!is.null(cd)) sum(sids %in% cd$service_id) else 0L
  cat(sprintf("trips %d | sids %d | in calendar %d | in calendar_dates %d | UNCOVERED %d\n",
              nrow(tr), length(sids), cov_c, cov_d,
              length(sids) - sum(unique(c(if (!is.null(cal)) cal$service_id,
                                          if (!is.null(cd)) cd$service_id)) %in% sids)))
  uncov <- setdiff(sids, unique(c(if (!is.null(cal)) cal$service_id,
                                  if (!is.null(cd)) cd$service_id)))
  if (length(uncov)) cat("uncovered sample:", head(uncov, 3), "\n")
  if (!is.null(cal) && nrow(cal)) {
    cat("calendar sample start/end:",
        paste(head(cal$start_date, 2)), "|", paste(head(cal$end_date, 2)), "\n")
    # how many calendar rows are ACTIVE on 2026-08-26 (Wednesday)? dow mask pos 3
    act <- cal[start_date <= "20260826" & end_date >= "20260826"]
    n_wed <- sum(substr(act$wednesday, 1, 1) == "1")
    cat("calendar rows covering 2026-08-26:", nrow(act), "| wednesday=1:", n_wed, "\n")
  }
  if (!is.null(cd) && nrow(cd)) {
    cat("calendar_dates rows on 20260826:", sum(cd$date == "20260826"),
        "(added:", sum(cd$date == "20260826" & cd$exception_type == "1"), ")\n")
  }
  unlink(tmp, recursive = TRUE)
}
probe_zip(file.path(NET, "bibus__horaires-theoriques-et-temps-reel-des-bus-et-tramways-circulant-sur-le-territoire-de-brest-metropole.zip"))
probe_zip(file.path(NET, "kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip"))
probe_zip(file.path(NET, "des__arrets-horaires-et-circuits-des-lignes-de-transports-en-commun-en-pays-de-la-loire-gtfs-destineo-reseaux-aom-aleop-1.zip"))
probe_zip(file.path(NET, "tub__gtfs-du-reseau-de-transports-publics-de-saint-brieuc-armor-agglomeration.zip"))
