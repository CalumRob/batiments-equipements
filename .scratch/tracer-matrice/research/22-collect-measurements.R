suppressMessages(library(data.table))
d <- "E:/batiments-equipements/data/matrice/cap20-probe-1000-b"
m <- jsonlite::fromJSON(file.path(d, "manifest.json"), simplifyVector = FALSE)
ch <- rbindlist(lapply(names(m$entries), function(k) {
  x <- m$entries[[k]]
  data.table(mode = x$mode, chunk = x$chunk_id, status = x$status,
             route_s = x$route_seconds, n_routed_pairs = x$n_routed_pairs,
             n_identity_pairs = x$n_identity_pairs, n_rows = x$n_rows,
             sha = substr(x$sha256, 1, 8))
}))
setorder(ch, mode, chunk)
print(ch)
cat("\n== per-mode totals ==\n")
tot <- ch[, .(chunks = .N, route_s_total = sum(route_s),
              routed_pairs = sum(n_routed_pairs),
              identity_pairs = sum(n_identity_pairs),
              matrix_rows = sum(n_rows)), by = mode]
print(tot)
meta <- jsonlite::fromJSON(file.path(d, "run_metadata.json"),
                           simplifyVector = FALSE)
cat("\nwall_seconds per mode:", paste(names(meta$per_mode), vapply(meta$per_mode, function(x) paste(unlist(x), collapse="/"), ""), sep="=", collapse=" | "), "\n")
# full-run extrapolation: probe chunks are 200 coords; release runs 100k
# coords/chunk -> scale per-coordinate route time linearly
coords_per_probe_chunk <- 200L
n_release_coords <- 1424208L
scale <- n_release_coords / (5 * coords_per_probe_chunk)
est <- tot[, .(mode,
               s_per_coord = route_s_total / (5 * coords_per_probe_chunk),
               est_release_hours = route_s_total / (5 * coords_per_probe_chunk)
                                     * n_release_coords / 3600)]
setorder(est, -est_release_hours)
cat("\n== full-run wall-clock estimate (linear in coordinates) ==\n")
print(est)
cat("SUM est hours:", sum(est$est_release_hours), "\n")
fwrite(ch, "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22/.scratch/tracer-matrice/research/outputs/22-probe-entries.csv")
fwrite(est, "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22/.scratch/tracer-matrice/research/outputs/22-wallclock-estimate.csv")
