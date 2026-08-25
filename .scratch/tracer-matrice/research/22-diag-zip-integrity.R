suppressMessages(library(data.table))
NET <- "E:/batiments-equipements/data/networks/cap20-current"
zips <- list.files(NET, pattern = "[.]zip$")
rows <- lapply(zips, function(z) {
  p <- file.path(NET, z)
  r <- tryCatch({
    nm <- utils::unzip(p, list = TRUE)
    n <- nrow(nm)
    # spot-check: extract every entry to tempdir and count failures
    tmp <- file.path(tempdir(), "int"); unlink(tmp, recursive = TRUE); dir.create(tmp)
    ex <- tryCatch({ utils::unzip(p, exdir = tmp); TRUE }, error = function(e) FALSE,
                   warning = function(w) FALSE)
    ok_entries <- if (ex) length(list.files(tmp, recursive = TRUE)) else NA_integer_
    unlink(tmp, recursive = TRUE)
    data.frame(zip = substr(z, 1, 40), entries = n, extracts = ok_entries,
               status = if (is.na(ok_entries)) "EXTRACT-FAIL"
                        else if (ok_entries != n) paste0("COUNT-MISMATCH ", ok_entries, "/", n)
                        else "OK")
  }, error = function(e) data.frame(zip = substr(z, 1, 40), entries = NA_integer_,
                                    extracts = NA_integer_, status = "OPEN-FAIL"))
})
o <- rbindlist(rows)
print(o[status != "OK"])
cat("\nOK:", sum(o$status == "OK"), "/", nrow(o), "\n")
