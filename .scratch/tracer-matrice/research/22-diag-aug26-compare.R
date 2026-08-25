suppressMessages(library(data.table))
d <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26/chunks"
w <- as.data.table(arrow::read_parquet(file.path(d, "walk_1.parquet")))
t <- as.data.table(arrow::read_parquet(file.path(d, "transit_1.parquet")))
cat("walk rows:", nrow(w), "| transit rows:", nrow(t), "\n")
kw <- w[, .(batiment_id, TYPEQU, tt = tt_nearest)]
kt <- t[, .(batiment_id, TYPEQU, tt = tt_nearest)]
setorder(kw, batiment_id, TYPEQU); setorder(kt, batiment_id, TYPEQU)
cat("keys identical:", identical(kw$batiment_id, kt$batiment_id),
    "&&", identical(kw$TYPEQU, kt$TYPEQU), "\n")
cat("tt_nearest identical:", identical(as.numeric(kw$tt), as.numeric(kt$tt)), "\n")
cat("n differing tt:", sum(as.numeric(kw$tt) != as.numeric(kt$tt)), "\n")
# transit-only keys?
setkey(kw, batiment_id, TYPEQU); setkey(kt, batiment_id, TYPEQU)
only_t <- fsetdiff(kt[, .(batiment_id, TYPEQU)], kw[, .(batiment_id, TYPEQU)])
only_w <- fsetdiff(kw[, .(batiment_id, TYPEQU)], kt[, .(batiment_id, TYPEQU)])
cat("keys ONLY in transit:", nrow(only_t), "| ONLY in walk:", nrow(only_w), "\n")
if (nrow(only_t)) print(head(only_t))
