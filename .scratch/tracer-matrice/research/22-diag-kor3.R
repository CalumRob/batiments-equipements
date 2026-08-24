suppressMessages(library(data.table))
zp <- "E:/batiments-equipements/data/downloads/derived/kor__base-de-donnees-multimodale-transports-publics-en-bretagne-korrigo-gtfs.zip"
tmp <- file.path(tempdir(), "k3"); unlink(tmp, recursive = TRUE); dir.create(tmp)
nm <- utils::unzip(zp, list = TRUE)[["Name"]]
hit <- grep("(^|[.]/)stops[.]txt$", nm, value = TRUE)
cat("entry:", hit, "\n")
utils::unzip(zp, files = hit[1], exdir = tmp)
s <- fread(file.path(tmp, "stops.txt"), colClasses = "character",
           na.strings = "", showProgress = FALSE)
ps <- s[["parent_station"]]
cat("rows:", nrow(s), " non-empty parents:", sum(!is.na(ps) & nzchar(ps)), "\n")
cat("parents resolving:", sum(ps %in% s[["stop_id"]], na.rm = TRUE), "\n")
print(head(s[!is.na(parent_station) & parent_station != "" &
              !(parent_station %in% stop_id)][, .(stop_id, location_type, parent_station)], 4))
