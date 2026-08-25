suppressMessages(library(data.table))
NET <- "E:/batiments-equipements/data/networks/cap20-current"

# 1. internal service_id consistency of a few derived zips
for (z in c("star__versions-des-horaires-theoriques-des-lignes-de-bus-et-de-metro-du-reseau-star-dans-les-formats-gtfs-et-netex-ainsi-que-les-urls-dacces-au-gtfs-rt-1__83281.zip",
            "kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip")) {
  tmp <- file.path(tempdir(), paste0("svc-", substr(z, 1, 8)))
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(file.path(NET, z), exdir = tmp)
  rd <- function(b) {
    f <- list.files(tmp, pattern = paste0("^", b, "[.]txt$"), recursive = TRUE,
                    full.names = TRUE)[1]
    if (is.na(f)) NULL else fread(f, colClasses = "character", showProgress = FALSE)
  }
  tr <- rd("trips.txt"); cal <- rd("calendar.txt"); cd <- rd("calendar_dates.txt")
  sids <- unique(tr$service_id)
  cat("\n==", substr(z, 1, 24), "==\n")
  cat("trips:", nrow(tr), "| distinct service_ids:", length(sids), "\n")
  cat("in calendar:", sum(sids %in% cal$service_id),
      "| in calendar_dates:", sum(sids %in% cd$service_id), "\n")
  cat("sample sid:", head(sids, 2), "\n")
  unlink(tmp, recursive = TRUE)
}

# 2. ask the built network: is transit available at the probe departure?
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE)))
  source(f)
net <- link_network(data_path = NET, elevation = "NONE", verbose = FALSE)
av <- tryCatch(
  r5r::check_transit_availability(net,
                                  departure_datetime = as.POSIXct("2026-09-15 06:00:00", tz = "UTC")),
  error = function(e) { cat("avail err:", conditionMessage(e), "\n"); NULL })
cat("\ncheck_transit_availability @ 2026-09-15T06:00Z:\n"); print(av)
av2 <- tryCatch(
  r5r::check_transit_availability(net,
                                  departure_datetime = as.POSIXct("2026-08-26 06:00:00", tz = "UTC")),
  error = function(e) { cat("avail err2:", conditionMessage(e), "\n"); NULL })
cat("\ncheck_transit_availability @ 2026-08-26T06:00Z:\n"); print(av2)
r5r::stop_r5(net)
