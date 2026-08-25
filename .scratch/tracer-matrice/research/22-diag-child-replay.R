suppressMessages(library(data.table))
options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
RD <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-aug26"

req <- jsonlite::fromJSON(file.path(RD, "requests/chunk_3.json"),
                          simplifyVector = TRUE)
cat("request chunk:", req$chunk_id, "| modes:",
    paste(req$modes, collapse = "+"), "\n")

# confirm 537 rides in this chunk
pts <- as.data.table(arrow::read_parquet(req$paths$origin_points))
slice <- chunk_point_slice(pts, as.integer(req$chunk_id),
                           as.integer(req$chunk_size))
cat("chunk origins:", nrow(slice),
    "| contains coord_o_000537:", any(slice$id == "coord_o_000537"), "\n")

res <- run_chunk_worker(req, network_loader = function() {
  link_network(data_path = req$network_dir, elevation = "NONE",
               verbose = FALSE)
})
tr <- as.data.table(arrow::read_parquet(
  file.path(RD, "chunks", sprintf("transit_%d.parquet", req$chunk_id))))
wk <- as.data.table(arrow::read_parquet(
  file.path(RD, "chunks", sprintf("walk_%d.parquet", req$chunk_id))))
cat("\nafter in-process child rerun:\n")
cat("transit rows:", nrow(tr), "| walk rows:", nrow(wk), "\n")
kt <- tr[, .(batiment_id, TYPEQU, tt = tt_nearest)]
kw <- wk[, .(batiment_id, TYPEQU, tt = tt_nearest)]
setorder(kt); setorder(kw)
only_t <- fsetdiff(kt[, .(batiment_id, TYPEQU)], kw[, .(batiment_id, TYPEQU)])
cat("keys ONLY in transit now:", nrow(only_t), "\n")
