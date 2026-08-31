# The service-date gate is deliberately independent of r5r. It must catch a
# stale or misdated GTFS calendar before a network is built.

write_gtfs_calendar_fixture <- function(root, calendar, calendar_dates, trips) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(calendar)) {
    utils::write.csv(calendar, file.path(root, "calendar.txt"), row.names = FALSE,
                     quote = FALSE, na = "")
  }
  if (!is.null(calendar_dates)) {
    utils::write.csv(calendar_dates, file.path(root, "calendar_dates.txt"),
                     row.names = FALSE, quote = FALSE, na = "")
  }
  utils::write.csv(trips, file.path(root, "trips.txt"), row.names = FALSE,
                   quote = FALSE, na = "")
  zip_path <- file.path(dirname(root), paste0(basename(root), ".zip"))
  owd <- setwd(root)
  on.exit(setwd(owd), add = TRUE)
  files <- c(if (!is.null(calendar)) "calendar.txt" else character(),
             if (!is.null(calendar_dates)) "calendar_dates.txt" else character(),
             "trips.txt")
  zip::zip(zipfile = zip_path, files = files)
  setwd(owd)
  zip_path
}

test_that("gtfs_service_date_summary applies weekday calendars and exceptions", {
  root <- tempfile("gtfs-service-date-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  calendar <- data.frame(
    service_id = c("weekday", "saturday"),
    monday = c(1, 0), tuesday = c(1, 0), wednesday = c(1, 0),
    thursday = c(1, 0), friday = c(1, 0), saturday = c(0, 1),
    sunday = c(0, 0), start_date = c("20260901", "20260901"),
    end_date = c("20260930", "20260930"),
    stringsAsFactors = FALSE
  )
  calendar_dates <- data.frame(
    service_id = c("weekday", "saturday", "special"),
    date = c("20260916", "20260916", "20260916"),
    exception_type = c("2", "1", "1"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("tw", "ts", "tx"),
    service_id = c("weekday", "saturday", "special"),
    stringsAsFactors = FALSE
  )
  zip_path <- write_gtfs_calendar_fixture(root, calendar, calendar_dates, trips)

  summary <- gtfs_service_date_summary(zip_path, as.Date("2026-09-16"))

  expect_identical(summary$service_date, "2026-09-16")
  expect_setequal(summary$active_service_ids, c("saturday", "special"))
  expect_identical(as.integer(summary$n_active_services), 2L)
  expect_identical(as.integer(summary$n_active_trips), 2L)
  expect_identical(as.integer(summary$calendar_rows), 2L)
  expect_identical(as.integer(summary$calendar_date_rows), 3L)
})

test_that("gtfs_service_date_summary supports calendar_dates-only feeds", {
  root <- tempfile("gtfs-calendar-dates-only-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  calendar_dates <- data.frame(
    service_id = c("special", "other"),
    date = c("20260916", "20260917"),
    exception_type = c("1", "1"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("ts", "to"), service_id = c("special", "other"),
    stringsAsFactors = FALSE
  )
  zip_path <- write_gtfs_calendar_fixture(
    root, NULL, calendar_dates, trips
  )

  summary <- gtfs_service_date_summary(zip_path, "20260916")

  expect_setequal(summary$active_service_ids, "special")
  expect_identical(as.integer(summary$n_active_trips), 1L)
  expect_identical(as.integer(summary$calendar_rows), 0L)
})

test_that("the service-date validator does not project a 2034 sentinel onto September", {
  root <- tempfile("gtfs-sentinel-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  calendar <- data.frame(
    service_id = "winter-sentinel",
    monday = 1, tuesday = 1, wednesday = 1, thursday = 1, friday = 1,
    saturday = 1, sunday = 1, start_date = "20340116", end_date = "20340116",
    stringsAsFactors = FALSE
  )
  calendar_dates <- data.frame(
    service_id = character(), date = character(), exception_type = character(),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(trip_id = "t1", service_id = "winter-sentinel",
                      stringsAsFactors = FALSE)
  zip_path <- write_gtfs_calendar_fixture(root, calendar, calendar_dates, trips)

  expect_error(
    validate_gtfs_service_date(zip_path, as.Date("2026-09-16"),
                               feed_id = "coralie"),
    "coralie.*2026-09-16|2026-09-16.*coralie|no active trips"
  )
  expect_silent(validate_gtfs_service_date(
    zip_path, as.Date("2034-01-16"), feed_id = "coralie"))
})

test_that("the selection gate only requires explicitly required feeds", {
  root <- tempfile("gtfs-selection-gate-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  data_dir <- file.path(root, "data")
  dir.create(file.path(data_dir, "downloads"), recursive = TRUE,
             showWarnings = FALSE)
  inactive_calendar <- data.frame(
    service_id = "winter-sentinel",
    monday = 1, tuesday = 1, wednesday = 1, thursday = 1, friday = 1,
    saturday = 1, sunday = 1, start_date = "20340116", end_date = "20340116",
    stringsAsFactors = FALSE
  )
  active_calendar <- data.frame(
    service_id = "active-service",
    monday = 1, tuesday = 1, wednesday = 1, thursday = 1, friday = 1,
    saturday = 1, sunday = 1, start_date = "20260901", end_date = "20260930",
    stringsAsFactors = FALSE
  )
  calendar_dates <- data.frame(
    service_id = character(), date = character(), exception_type = character(),
    stringsAsFactors = FALSE
  )
  inactive_zip <- write_gtfs_calendar_fixture(
    file.path(root, "inactive"), inactive_calendar, calendar_dates,
    data.frame(trip_id = "t1", service_id = "winter-sentinel",
               stringsAsFactors = FALSE)
  )
  active_zip <- write_gtfs_calendar_fixture(
    file.path(root, "active"), active_calendar, calendar_dates,
    data.frame(trip_id = "t2", service_id = "active-service",
               stringsAsFactors = FALSE)
  )
  inactive_cached <- file.path(data_dir, "downloads", "coralie.zip")
  active_cached <- file.path(data_dir, "downloads", "active.zip")
  file.copy(inactive_zip, inactive_cached)
  file.copy(active_zip, active_cached)

  entries <- list(
    list(id = "gtfsx_coralie", sha256 = sha256_file(inactive_cached),
         cached_path = "data/downloads/coralie.zip"),
    list(id = "gtfsx_active", sha256 = sha256_file(active_cached),
         cached_path = "data/downloads/active.zip")
  )
  selection <- entries
  names(selection) <- vapply(entries, `[[`, character(1L), "id")

  expect_error(
    validate_transit_selection_service_date(
      selection, as.Date("2026-09-16"), data_dir = data_dir,
      required_ids = "gtfsx_coralie"
    ),
    "gtfsx_coralie.*2026-09-16|2026-09-16.*gtfsx_coralie|no active trips"
  )
  expect_silent(validate_transit_selection_service_date(
    selection, as.Date("2026-09-16"), data_dir = data_dir,
    required_ids = "gtfsx_active"
  ))
})

test_that("stage_transit_feeds can enforce the service-date gate before copying", {
  root <- tempfile("gtfs-stage-date-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  data_dir <- file.path(root, "data")
  download_dir <- file.path(data_dir, "downloads")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  calendar <- data.frame(
    service_id = "winter-sentinel",
    monday = 1, tuesday = 1, wednesday = 1, thursday = 1, friday = 1,
    saturday = 1, sunday = 1, start_date = "20340116", end_date = "20340116",
    stringsAsFactors = FALSE
  )
  calendar_dates <- data.frame(
    service_id = character(), date = character(), exception_type = character(),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(trip_id = "t1", service_id = "winter-sentinel",
                      stringsAsFactors = FALSE)
  fixture_zip <- write_gtfs_calendar_fixture(
    file.path(root, "fixture"), calendar, calendar_dates, trips
  )
  cached <- file.path(download_dir, "coralie.zip")
  file.copy(fixture_zip, cached)
  id <- "gtfsx_coralie"
  entry <- list(id = id, source = "https://example.invalid/coralie",
                sha256 = sha256_file(cached), cached_path = "data/downloads/coralie.zip",
                readers = "r5r-transit")
  manifest_path <- file.path(data_dir, "manifest.json")
  jsonlite::write_json(
    list(manifest_version = 1L, sources = setNames(list(entry), id)),
    manifest_path, auto_unbox = TRUE, pretty = TRUE
  )
  net_dir <- file.path(root, "network")

  expect_error(
    stage_transit_feeds(
      net_dir, data_dir = data_dir, manifest_path = manifest_path,
      service_date = as.Date("2026-09-16"), required_ids = id
    ),
    "service-date gate.*2026-09-16|2026-09-16.*service-date gate"
  )
  expect_false(dir.exists(net_dir))

  block <- stage_transit_feeds(
    net_dir, data_dir = data_dir, manifest_path = manifest_path,
    service_date = as.Date("2034-01-16"), required_ids = id
  )
  expect_true(file.exists(file.path(net_dir, "coralie.zip")))
  expect_identical(block$service_coverage$service_date, "2034-01-16")
  expect_identical(
    as.integer(block$service_coverage$feeds[[id]]$n_active_trips), 1L
  )
})
