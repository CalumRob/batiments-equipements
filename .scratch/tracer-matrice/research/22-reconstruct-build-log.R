NET <- "E:/batiments-equipements/data/networks/cap20-current"
OUT <- "E:/Temp/opencode/pocock-workers/batiments-equipements/issue-22/.scratch/tracer-matrice/research/outputs"
mk <- jsonlite::fromJSON(file.path(NET, ".network-identity.json"),
                         simplifyVector = FALSE)
log <- list(
  recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  note = paste("build_seconds reconstructed from r5r JVM log timestamps:",
               "the wrapper crashed AFTER commit_network_cache while parsing",
               "r5r 2.4.0's network_settings.json - that file carries unescaped",
               "Windows backslashes and is invalid JSON written by r5r itself;",
               "known-issue note for the run metadata"),
  build_seconds = 87 * 60, build_hours = 87 / 60,
  marker_fingerprint = mk$fingerprint,
  network_dat_bytes = file.info(file.path(NET, "network.dat"))[["size"]],
  r5r_version = mk$components$r5r_version,
  r5_version = mk$components$r5_version,
  regime = "current", heap = "-Xmx24G")
jsonlite::write_json(log, file.path(OUT, "22-build-log.json"),
                     auto_unbox = TRUE, pretty = TRUE)
cat("build-log reconstructed; network.dat",
    round(file.info(file.path(NET, "network.dat"))[["size"]] / 1048576),
    "MB; fingerprint", substr(mk$fingerprint, 1, 12), "\n")
