# Regression for the #22 probe's transit collapse: the request file stored
# "2026-08-26T05:00:00+0000", but jsonlite::fromJSON auto-coerced it to a
# Date (time dropped), as_chunk_request rebuilt MIDNIGHT UTC, and every
# child routed transit at 02:00 Paris — no buses anywhere, pure-walk
# results, byte-stable across dates. The fix: departure_epoch rides beside
# the string, and the reader prefers it.

test_that("departure survives the request JSON round-trip with time-of-day intact", {
  dep <- as.POSIXct("2026-08-26 05:00:00", tz = "UTC")   # 07:00 Paris

  manifest <- list(
    run_label = "rt-departure",
    plan_census = list(chunk_size = 10L, n_origin_coords = 10L),
    identity = list(routing_parameters = list(
      walk_speed = 4, bike_speed = 12, max_trip_duration = cap_minutes(),
      elevation = "NONE",
      departure_datetime = as_canonical_datetime_string(dep),
      time_window = 60L, percentiles = c(1L, 50L)
    ))
  )
  req <- build_chunk_request(manifest, chunk_id = 1L, modes = c("transit"),
                             run_dir = tempdir(), network_dir = "net",
                             code_dir = getwd(), heap = "-Xmx24G")

  # the epoch field exists and is exact BEFORE serialization
  expect_equal(req$routing$departure_epoch, as.numeric(dep))

  # ... and AFTER the full JSON boundary that ate the probe's timestamp
  path <- chunk_request_save(req, file.path(tempdir(), "req-rt.json"))
  back <- as_chunk_request(jsonlite::fromJSON(path, simplifyVector = TRUE))
  expect_s3_class(back$routing$departure_datetime, "POSIXct")
  expect_identical(
    format(back$routing$departure_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-08-26 05:00:00")
})

test_that("a date-only departure string is refused, never silently midnight", {
  req <- list(kind = "matrice-chunk-request", chunk_id = 1L,
              chunk_size = 10L, n_origin_coords = 10L,
              modes = c("walk"), paths = list(),
              routing = list(departure_datetime = "2026-08-26"))
  # fromJSON-style damage reproduced literally: a Date carries no time
  req$routing$departure_datetime <- as.Date("2026-08-26")
  expect_error(as_chunk_request(req), "date-only|departure_epoch")
})
