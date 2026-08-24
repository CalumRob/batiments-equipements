suppressMessages(library(data.table))
DATA <- "E:/batiments-equipements/data"; pfx <- "kor"
uz <- function(zip, base, exdir) {
  nm <- utils::unzip(zip, list = TRUE)[["Name"]]
  hit <- grep(paste0("(^|[.]/)", base, "$"), nm, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) return(invisible(FALSE))
  utils::unzip(zip, files = hit[1], exdir = exdir, overwrite = TRUE)
  invisible(TRUE)
}
gt <- function(p) {
  o <- fread(p, colClasses = "character", encoding = "UTF-8",
             na.strings = "", showProgress = FALSE)
  setnames(o, names(o), trimws(gsub("^\ufeff", "", names(o)))); o
}
tmp <- file.path(tempdir(), "k1"); unlink(tmp, recursive = TRUE); dir.create(tmp)
uz(file.path(DATA, "downloads/derived/kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip"), "stops.txt", tmp)
d <- gt(file.path(tmp, "stops.txt"))
rtmp <- file.path(tempdir(), "k2"); unlink(rtmp, recursive = TRUE); dir.create(rtmp)
uz(file.path(DATA, "downloads/base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip"), "stops.txt", rtmp)
r <- gt(file.path(rtmp, "stops.txt"))
cat("derived rows:", nrow(d), " raw rows:", nrow(r), "\n")
strip <- function(x) sub(paste0("^", pfx, ":"), "", x)
ps <- d[["parent_station"]]; dang <- ps[!is.na(ps) & nzchar(ps) & !(ps %in% d[["stop_id"]])]
cat("dangling:", length(unique(dang)), " sample: ", head(unique(dang), 3), "\n")
need <- unique(strip(dang)); ridx <- setNames(seq_len(nrow(r)), r[["stop_id"]])
cat("needed raw ids found:", sum(need %in% names(ridx)), "/", length(need), "\n")
miss <- need[!need %in% names(ridx)]
if (length(miss)) {
  cat("missing sample:\n"); print(head(miss, 5))
  st <- r[location_type == "1"]
  cat("raw stations:", nrow(st), " raw location_types:\n"); print(r[, .N, by = location_type])
  m1 <- miss[1]
  cat("fuzzy search '", m1, "':\n")
  print(st[grepl(sub("_1$", "", m1), stop_id, fixed = TRUE)][1:3])
  # where DOES the stripped id appear in raw? maybe parents live in a DIFFERENT feed
  hit <- r[stop_id %in% need]
  cat("raw hits for needed ids:", nrow(hit), "\n")
  # check the OTHER aggregates: maybe kor children reference stations of the ORIGINAL korrigo (yes) vs something else
  alt <- c("E:/batiments-equipements/data/downloads/arrets-horaires-et-circuits-des-lignes-de-transports-en-commun-en-pays-de-la-loire-gtfs-destineo-reseaux-aom-aleop-1.zip")
}
