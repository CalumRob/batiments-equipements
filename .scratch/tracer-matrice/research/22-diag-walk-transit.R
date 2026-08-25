suppressMessages(library(data.table))
d <- "E:/batiments-equipements/data/matrice/cap20-probe-1000/chunks"
fs <- list.files(d)
cat("chunk files:\n"); print(fs)
w <- as.data.table(arrow::read_parquet(
  file.path(d, grep("^walk_1[.]", fs, value = TRUE)[1])))
t <- as.data.table(arrow::read_parquet(
  file.path(d, grep("^transit_1[.]", fs, value = TRUE)[1])))
cat("\nwalk cols:", paste(names(w), collapse = ", "), "\n")
cat("transit cols:", paste(names(t), collapse = ", "), "\n")
kw <- w[, .(batiment_id, TYPEQU, tt_nearest)]
kt <- t[, .(batiment_id, TYPEQU, tt_nearest)]
setorder(kw, batiment_id, TYPEQU); setorder(kt, batiment_id, TYPEQU)
cat("\nkey sets identical:", identical(kw$batiment_id, kt$batiment_id) &&
      identical(kw$TYPEQU, kt$TYPEQU), "\n")
cat("tt vectors identical:", identical(kw$tt_nearest, kt$tt_nearest), "\n")
cat("tt corr:",
    cor(as.numeric(kw$tt_nearest), as.numeric(kt$tt_nearest)), "\n")
cat("walk tt summary:\n"); print(summary(as.numeric(kw$tt_nearest)))
cat("transit tt summary:\n"); print(summary(as.numeric(kt$tt_nearest)))
