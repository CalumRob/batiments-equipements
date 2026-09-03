# The service-date gate is deliberately independent of r5r. It must catch a
# stale or misdated GTFS calendar before a network is built.

write_gtfs_calendar_fixture <- function(root, calendar, calendar_dates, trips,
                                        stop_times = NULL) {
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
  if (!is.null(stop_times)) {
    utils::write.csv(stop_times, file.path(root, "stop_times.txt"),
                     row.names = FALSE, quote = FALSE, na = "")
  }
  zip_path <- file.path(dirname(root), paste0(basename(root), ".zip"))
  owd <- setwd(root)
  on.exit(setwd(owd), add = TRUE)
  files <- c(if (!is.null(calendar)) "calendar.txt" else character(),
             if (!is.null(calendar_dates)) "calendar_dates.txt" else character(),
             "trips.txt",
             if (!is.null(stop_times)) "stop_times.txt" else character())
  zip::zip(zipfile = zip_path, files = files)
  setwd(owd)
  zip_path
}

test_that("staging applies the confirmed 10:00-11:00 any-stop activity gate", {
  root <- tempfile("gtfs-activity-window-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  data_dir <- file.path(root, "data")
  dir.create(file.path(data_dir, "downloads"), recursive = TRUE,
             showWarnings = FALSE)
  calendar <- data.frame(
    service_id = "weekday", monday = 1, tuesday = 1, wednesday = 1,
    thursday = 1, friday = 1, saturday = 0, sunday = 0,
    start_date = "20260901", end_date = "20260930",
    stringsAsFactors = FALSE
  )
  no_exceptions <- data.frame(
    service_id = character(), date = character(), exception_type = character(),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("inside-trip", "outside-trip"),
    service_id = c("weekday", "weekday"), stringsAsFactors = FALSE
  )
  stop_times <- data.frame(
    trip_id = c("inside-trip", "inside-trip", "outside-trip", "outside-trip"),
    arrival_time = c("09:59:00", "10:00:00", "09:59:00", "11:00:00"),
    departure_time = c("09:59:00", "10:00:00", "09:59:00", "11:00:00"),
    stop_id = c("s1", "s2", "s1", "s2"), stop_sequence = c(1, 2, 1, 2),
    stringsAsFactors = FALSE
  )
  inside <- write_gtfs_calendar_fixture(
    file.path(root, "inside"), calendar, no_exceptions, trips[1, , drop = FALSE],
    stop_times[1:2, , drop = FALSE]
  )
  outside <- write_gtfs_calendar_fixture(
    file.path(root, "outside"), calendar, no_exceptions, trips[2, , drop = FALSE],
    stop_times[3:4, , drop = FALSE]
  )
  inside_cached <- file.path(data_dir, "downloads", "inside.zip")
  outside_cached <- file.path(data_dir, "downloads", "outside.zip")
  file.copy(inside, inside_cached)
  file.copy(outside, outside_cached)
  entries <- list(
    gtfsx_inside = list(id = "gtfsx_inside", sha256 = sha256_file(inside_cached),
                        cached_path = "data/downloads/inside.zip",
                        readers = "r5r-transit"),
    gtfsx_outside = list(id = "gtfsx_outside", sha256 = sha256_file(outside_cached),
                         cached_path = "data/downloads/outside.zip",
                         readers = "r5r-transit")
  )
  manifest_path <- file.path(data_dir, "manifest.json")
  jsonlite::write_json(
    list(manifest_version = 1L, sources = entries), manifest_path,
    auto_unbox = TRUE, pretty = TRUE
  )

  block <- stage_transit_feeds(
    file.path(root, "network"), data_dir = data_dir,
    manifest_path = manifest_path, service_date = "2026-09-16",
    required_ids = "gtfsx_inside",
    activity_window = full_run_transit_activity_window()
  )

  expect_identical(block$n_feeds, 1L)
  expect_identical(block$feeds[[1]]$id, "gtfsx_inside")
  expect_identical(block$skipped[[1]]$id, "gtfsx_outside")
  expect_match(block$skipped[[1]]$reason, "10:00:00.*11:00:00")
  expect_identical(block$service_coverage$activity_window$start, "10:00:00")
  expect_identical(block$service_coverage$feeds$gtfsx_inside$n_window_trips, 1L)
  expect_identical(block$service_coverage$feeds$gtfsx_inside$n_active_stops, 2L)
  expect_error(
    stage_transit_feeds(
      file.path(root, "required-outside"), data_dir = data_dir,
      manifest_path = manifest_path, service_date = "2026-09-16",
      required_ids = "gtfsx_outside",
      activity_window = full_run_transit_activity_window()
    ),
    "activity/coverage gate"
  )
})

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

test_that("gtfs_service_date_summary accepts publisher column casing and aliases", {
  root <- tempfile("gtfs-service-date-columns-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  calendar <- data.frame(
    service_id = c("weekday", "special"),
    Monday = c("1", "0"), Tuesday = c("1", "0"),
    Wednesday = c("1", "0"), Thursday = c("1", "0"),
    Friday = c("1", "0"), Saturday = c("0", "0"), Sunday = c("0", "0"),
    start_date = c("20260901", "20260901"),
    end_date = c("20260930", "20260930"), stringsAsFactors = FALSE
  )
  calendar_dates <- data.frame(
    service_id = c("weekday", "special"), date = c("20260916", "20260916"),
    exception_date = c("2", "1"), stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("weekday-trip", "special-trip"),
    service_id = c("weekday", "special"), stringsAsFactors = FALSE
  )
  zip_path <- write_gtfs_calendar_fixture(
    root, calendar, calendar_dates, trips
  )

  summary <- gtfs_service_date_summary(zip_path, "20260916")

  expect_identical(summary$active_service_ids, "special")
  expect_identical(as.integer(summary$n_active_trips), 1L)
})

test_that("historical projection preserves publisher calendar-date aliases", {
  root <- tempfile("gtfs-service-date-projection-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  calendar <- data.frame(
    service_id = "weekday",
    Monday = "1", Tuesday = "1", Wednesday = "1", Thursday = "1",
    Friday = "1", Saturday = "0", Sunday = "0",
    start_date = "20250901", end_date = "20250930",
    stringsAsFactors = FALSE
  )
  calendar_dates <- data.frame(
    service_id = character(), date = character(), exception_date = character(),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = "weekday-trip", service_id = "weekday", stringsAsFactors = FALSE
  )
  source_zip <- write_gtfs_calendar_fixture(root, calendar, calendar_dates, trips)
  output_zip <- file.path(root, "projection.zip")

  projected <- project_gtfs_service_date(
    source_zip, "2025-09-17", "2026-09-16", output_zip
  )

  expect_identical(as.integer(projected$target_summary$n_active_trips), 1L)
  projected_dates <- .gtfs_read_member(output_zip, "calendar_dates.txt", TRUE)
  expect_identical(names(projected_dates), names(calendar_dates))
  expect_true(any(projected_dates$exception_date == "1"))
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
