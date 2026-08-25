# 22-diag-gtfs-errors2.R — hypothesis 2: r5r's ReferentialIntegrityError on
# parent_station = references that exist as stop_id but point at a PLATFORM
# (location_type != 1), which the GTFS spec forbids (parent must be a
# Station). Naive id-existence passes; spec-typed lookup catches it.

WT <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22"
NET_DIR <- "E:/batiments-equipements/data/networks/cap20-current"
OUT <- file.path(WT, ".scratch", "tracer-matrice", "research", "outputs")

zips <- list.files(NET_DIR, pattern = "[.]zip$", full.names = TRUE)
res <- lapply(zips, function(z) {
  nm <- basename(z)
  tmp <- file.path(tempdir(), "diagx2")
  unlink(tmp, recursive = TRUE); dir.create(tmp)
  ok <- tryCatch({ utils::unzip(z, files = c("stops.txt"), exdir = tmp); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(file.path(tmp, "stops.txt"))) {
    unlink(tmp, recursive = TRUE)
    return(data.frame(feed = nm, n_stops = NA_integer_,
                      n_parent_to_nonstation = NA_integer_, stringsAsFactors = FALSE))
  }
  s <- data.table::fread(file.path(tmp, "stops.txt"), encoding = "UTF-8",
                         showProgress = FALSE)
  unlink(tmp, recursive = TRUE)
  n_bad <- 0L
  ps <- s[["parent_station"]]; sid <- s[["stop_id"]]
  if (!is.null(ps)) {
    idx <- which(!is.na(ps) & nzchar(ps))
    if (length(idx)) {
      lt <- s[["location_type"]]
      if (is.null(lt)) lt <- rep("0", nrow(s))
      lt[is.na(lt)] <- "0"
      m <- match(ps[idx], sid)
      hit <- !is.na(m)
      parent_lt <- rep(NA_character_, length(idx))
      parent_lt[hit] <- lt[m[hit]]
      n_bad <- sum(parent_lt %in% c("0", ""), na.rm = TRUE) +
               sum(is.na(m))
    }
  }
  data.frame(feed = nm, n_stops = nrow(s),
             n_parent_to_nonstation_or_missing = n_bad,
             stringsAsFactors = FALSE)
})
out <- data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
data.table::setorder(out, -n_parent_to_nonstation_or_missing)
data.table::fwrite(out, file.path(OUT, "22-diag-parent-station2.csv"))
cat("TOTAL bad parent_station refs:", sum(out[["n_parent_to_nonstation_or_missing"]], na.rm = TRUE),
    "(r5r reported 16445 HIGH across all types)\n")
print(out[n_parent_to_nonstation_or_missing > 0])
