# 22-diag-gtfs-errors.R — attribute r5r's HIGH-priority GTFS errors to the
# offending staged feed(s). r5r's gtfs_errors.csv aggregates across every
# feed in the network directory WITHOUT naming the source zip; the fix
# decision (derive-filter vs documented exclusion per the #25 union
# contract) depends on knowing WHO is broken.
#
# Probe: for every staged <slug>__<feed>.zip, read stops.txt and count
# parent_station references that dangle (non-empty but absent from stop_id).
# Also report per-feed row counts so relative scale is visible. Light,
# CPU-only, no JVM.

WT <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
NET_DIR <- "E:/batiments-equipements/data/networks/cap20-current"
OUT <- file.path(WT, ".scratch", "tracer-matrice", "research", "outputs")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

zips <- list.files(NET_DIR, pattern = "[.]zip$", full.names = TRUE)
cat(sprintf("probing %d staged feed zips\n", length(zips)))

res <- lapply(zips, function(z) {
  nm <- basename(z)
  tmp <- file.path(tempdir(), "diagx")
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  ok <- tryCatch({
    utils::unzip(z, files = c("stops.txt"), exdir = tmp, overwrite = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(file.path(tmp, "stops.txt"))) {
    return(data.frame(feed = nm, n_stops = NA_integer_, n_dangling = NA_integer_,
                      stringsAsFactors = FALSE))
  }
  s <- data.table::fread(file.path(tmp, "stops.txt"), encoding = "UTF-8",
                         showProgress = FALSE)
  sid <- s[["stop_id"]]
  ps <- s[["parent_station"]]
  n_dangling <- 0L
  if (!is.null(ps)) {
    np <- ps[!is.na(ps) & nzchar(ps)]
    if (length(np)) n_dangling <- sum(!np %in% sid)
  }
  unlink(tmp, recursive = TRUE)
  data.frame(feed = nm, n_stops = nrow(s), n_dangling = n_dangling,
             stringsAsFactors = FALSE)
})
out <- data.table::rbindlist(res, use.names = TRUE)
data.table::setorder(out, -n_dangling)
data.table::fwrite(out, file.path(OUT, "22-diag-parent-station.csv"))
cat("total dangling parent_station refs:", sum(out[["n_dangling"]], na.rm = TRUE), "\n")
print(out[n_dangling > 0])
cat("\ntop by size:\n"); print(out[order(-n_stops)][1:8])
