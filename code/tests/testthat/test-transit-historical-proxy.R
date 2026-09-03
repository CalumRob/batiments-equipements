# A historical proxy is a run-only copy: the pinned source must remain
# byte-identical while one source service date is made active on the global
# candidate date.

write_proxy_gtfs_fixture <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(
      service_id = c("weekday", "saturday"),
      monday = c("1", "0"), tuesday = c("1", "0"),
      wednesday = c("1", "0"), thursday = c("1", "0"),
      friday = c("1", "0"), saturday = c("0", "1"),
      sunday = c("0", "0"), start_date = c("20250901", "20250901"),
      end_date = c("20260703", "20260703"), stringsAsFactors = FALSE
    ), file.path(root, "calendar.txt"), row.names = FALSE, quote = FALSE
  )
  utils::write.csv(
    data.frame(
      service_id = "saturday", date = "20260916", exception_type = "1",
      stringsAsFactors = FALSE
    ), file.path(root, "calendar_dates.txt"), row.names = FALSE,
    quote = FALSE
  )
  utils::write.csv(
    data.frame(
      trip_id = c("weekday-trip", "saturday-trip"),
      service_id = c("weekday", "saturday"), stringsAsFactors = FALSE
    ), file.path(root, "trips.txt"), row.names = FALSE, quote = FALSE
  )
  zip_path <- file.path(dirname(root), paste0(basename(root), ".zip"))
  owd <- setwd(root)
  on.exit(setwd(owd), add = TRUE)
  zip::zip(zipfile = zip_path,
           files = c("calendar.txt", "calendar_dates.txt", "trips.txt"))
  setwd(owd)
  zip_path
}

test_that("historical proxy projects one service date without changing its source", {
  root <- tempfile("gtfs-historical-proxy-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source_zip <- write_proxy_gtfs_fixture(file.path(root, "source"))
  output_zip <- file.path(root, "proxy.zip")
  source_sha <- sha256_file(source_zip)

  projected <- project_gtfs_service_date(
    source_zip,
    source_service_date = as.Date("2025-09-17"),
    target_service_date = as.Date("2026-09-16"),
    output_path = output_zip
  )

  expect_identical(projected$policy, "historical-proxy")
  expect_identical(projected$source_service_date, "2025-09-17")
  expect_identical(projected$target_service_date, "2026-09-16")
  expect_identical(as.integer(projected$source_summary$n_active_trips), 1L)
  expect_identical(as.integer(projected$target_summary$n_active_trips), 1L)
  expect_identical(projected$target_summary$active_service_ids, "weekday")
  expect_identical(sha256_file(source_zip), source_sha)
  expect_true(file.exists(output_zip))
  expect_identical(
    as.integer(gtfs_service_date_summary(output_zip, "2026-09-16")$n_active_trips),
    1L
  )

  projected_again <- project_gtfs_service_date(
    source_zip,
    source_service_date = as.Date("2025-09-17"),
    target_service_date = as.Date("2026-09-16"),
    output_path = file.path(root, "proxy-again.zip")
  )
  expect_identical(projected_again$sha256, projected$sha256)
})

test_that("staging records a historical proxy override for the selected feed", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  source_zip <- write_proxy_gtfs_fixture(file.path(fx$root, "proxy-source"))
  proxy <- project_gtfs_service_date(
    source_zip,
    source_service_date = as.Date("2025-09-17"),
    target_service_date = as.Date("2026-09-16"),
    output_path = file.path(fx$root, "vitre-historical-proxy.zip")
  )
  proxy$source_resource_id <- "83276"

  block <- stage_transit_feeds(
    file.path(fx$root, "network"),
    data_dir = fx$data_dir,
    manifest_path = fx$manifest_path,
    feed_overrides = list(gtfsx_a = proxy)
  )

  expect_setequal(
    list.files(file.path(fx$root, "network")),
    c("b__feed-b.zip", "vitre-historical-proxy.zip")
  )
  ids <- vapply(block$feeds, `[[`, "", "id")
  a <- block$feeds[[which(ids == "gtfsx_a")]]
  expect_identical(a$role, "historical-proxy")
  expect_identical(a$override$source_service_date, "2025-09-17")
  expect_identical(a$override$target_service_date, "2026-09-16")
  expect_identical(a$override$source_sha256, proxy$source_sha256)
  expect_identical(a$override$source_resource_id, "83276")
  expect_identical(a$override$sha256, proxy$sha256)
})

test_that("Vit'obus proxy resolves the selected 83276-derived source", {
  root <- tempfile("vitobus-proxy-")
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  data_dir <- file.path(root, "data")
  source_dir <- file.path(data_dir, "downloads", "derived")
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  source_zip <- write_proxy_gtfs_fixture(file.path(root, "source"))
  cached <- file.path(source_dir, "vitre__urban-83276.zip")
  file.copy(source_zip, cached)
  manifest_path <- file.path(data_dir, "manifest.json")
  jsonlite::write_json(
    list(
      manifest_version = 1L,
      sources = list(gtfsx_vitre = list(
        id = "gtfsx_vitre", sha256 = sha256_file(cached),
        cached_path = "data/downloads/derived/vitre__urban-83276.zip",
        readers = "r5r-transit"
      ))
    ), manifest_path, auto_unbox = TRUE, pretty = TRUE
  )

  proxy <- vitobus_historical_proxy(
    data_dir = data_dir,
    manifest_path = manifest_path,
    source_service_date = as.Date("2025-09-17"),
    target_service_date = as.Date("2026-09-16")
  )

  expect_identical(proxy$feed_id, "gtfsx_vitre")
  expect_identical(proxy$source_resource_id, "83276")
  expect_identical(proxy$policy, "historical-proxy")
  expect_true(file.exists(proxy$output_path))
  expect_identical(
    as.integer(proxy$target_summary$n_active_trips), 1L
  )

  staged <- stage_transit_feeds(
    file.path(root, "network"), data_dir = data_dir,
    manifest_path = manifest_path, service_date = "2026-09-16",
    required_ids = "gtfsx_vitre"
  )
  expect_identical(staged$feeds[[1]]$id, "gtfsx_vitre")
  expect_identical(staged$feeds[[1]]$role, "historical-proxy")
  expect_identical(staged$feeds[[1]]$override$source_resource_id, "83276")
  expect_identical(staged$feeds[[1]]$override$target_service_date, "2026-09-16")
})
