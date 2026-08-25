options(java.parameters = "-Xmx24G")
for (f in sort(list.files("code/R", pattern = "[.]R$", full.names = TRUE))) source(f)
DATA <- "E:/batiments-equipements/data"
b <- stage_transit_feeds(file.path(tempdir(), "probe-net"), data_dir = DATA,
                         manifest_path = file.path(DATA, "manifest.json"))
cat("n_feeds:", b$n_feeds, " skipped:", length(b$skipped), "\n")
for (s in b$skipped) cat("  skipped:", s$id, "|", s$reason, "\n")
ids <- vapply(b$feeds, function(x) x$id, "")
cat("flers staged?", "gtfsx_flers" %in% ids, "\n")
