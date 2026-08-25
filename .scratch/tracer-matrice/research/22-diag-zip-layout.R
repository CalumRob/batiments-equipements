d <- "E:/batiments-equipements/data/networks/cap20-current"
zips <- list.files(d, pattern = "[.]zip$")
rows <- lapply(zips, function(z) {
  nm <- utils::unzip(file.path(d, z), list = TRUE)[["Name"]]
  nm <- nm[nm != "" & !grepl("/$", nm)]
  data.frame(zip = z,
             entries = length(nm),
             nested = sum(grepl("/", nm)),
             dotflat = sum(grepl("^[.]/[^/]+$", nm)),
             plainflat = sum(grepl("^[^/.][^/]*$", nm)))
})
o <- data.table::rbindlist(rows)
cat("zips:", nrow(o),
    "| with nested entries:", sum(o$nested > 0),
    "| ./flat:", sum(o$dotflat > 0 & o$nested == 0),
    "| plain flat:", sum(o$plainflat > 0 & o$nested == 0 & o$dotflat == 0), "\n")
cat("\nNESTED (TableInSubdirectory suspects):\n")
print(o[nested > 0])
