# 22-diag-dup.R — plain vs namespaced staged zips: same network twice?
# Compare agency/trip/stop identity content of each <plain>.zip against its
# <slug>__<plain>.zip sibling. Decides whether staging double-routes
# networks (bug to fix) or the two forms carry disjoint content.

NET_DIR <- "E:/batiments-equipements/data/networks/cap20-current"
OUT <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22/.scratch/tracer-matrice/research/outputs"

peek <- function(zip, files = c("agency.txt", "trips.txt", "stops.txt")) {
  tmp <- file.path(tempdir(), "dupx"); unlink(tmp, recursive = TRUE); dir.create(tmp)
  out <- tryCatch({
    utils::unzip(zip, exdir = tmp)
    vapply(files, function(f) {
      p <- file.path(tmp, f)
      if (!file.exists(p)) return(NA_integer_)
      nrow(data.table::fread(p, showProgress = FALSE))
    }, integer(1))
  }, error = function(e) setNames(rep(NA_integer_, length(files)), files))
  unlink(tmp, recursive = TRUE)
  out
}

zips <- list.files(NET_DIR, pattern = "[.]zip$")
plain <- zips[!grepl("^[a-z0-9]+__", zips)]
rows <- lapply(plain, function(p) {
  slug <- sub("__.*$", "", grep(paste0("^[a-z0-9]+__", p, "$"), zips, value = TRUE)[1])
  if (is.na(slug)) return(NULL)
  a <- peek(file.path(NET_DIR, p)); b <- peek(file.path(NET_DIR, paste0(slug, "__", p)))
  data.frame(plain_zip = p, slug = slug,
             plain_agency = a[[1]], ns_agency = b[[1]],
             plain_trips = a[[2]], ns_trips = b[[2]],
             plain_stops = a[[3]], ns_stops = b[[3]])
})
out <- data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
data.table::fwrite(out, file.path(OUT, "22-diag-dup.csv"))
print(out)
cat("\nidentical-content pairs:", sum(out$plain_agency == out$ns_agency &
      out$plain_trips == out$ns_trips, na.rm = TRUE), "/", nrow(out), "\n")
