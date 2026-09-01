test_that("school-only transit policy removes the complete Némus school feed", {
  routes <- data.frame(
    route_id = c("1", "26", "27"),
    route_type = c("3", "3", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("urban", "lfe1", "lfe2"),
    route_id = c("1", "26", "27"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "nemus")

  expect_equal(nrow(filtered$routes), 0L)
  expect_equal(nrow(filtered$trips), 0L)
  expect_setequal(filtered$removed_route_ids, c("1", "26", "27"))
  expect_true(filtered$feed_removed)
})

test_that("school-only transit policy removes the complete Destineo feed", {
  routes <- data.frame(
    route_id = c("airport-1", "airport-2"),
    route_type = c("3", "1100"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("flight-1", "flight-2"),
    route_id = c("airport-1", "airport-2"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "des")

  expect_equal(nrow(filtered$routes), 0L)
  expect_equal(nrow(filtered$trips), 0L)
  expect_setequal(filtered$removed_route_ids, c("airport-1", "airport-2"))
  expect_true(filtered$feed_removed)
})

test_that("school-only transit policy removes explicit routes but preserves public routes", {
  routes <- data.frame(
    route_id = c("39", "40", "1"),
    route_short_name = c("S702", "S703", "1"),
    route_type = c("3", "3", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("s702", "s703", "public"),
    route_id = c("39", "40", "1"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "tub")

  expect_identical(filtered$routes$route_id, "1")
  expect_identical(filtered$trips$trip_id, "public")
  expect_setequal(filtered$removed_route_ids, c("39", "40"))
  expect_false(filtered$feed_removed)
})

test_that("school-only transit policy removes the full TUB P/S family", {
  routes <- data.frame(
    route_id = c("p-route", "s-route", "urban"),
    route_short_name = c("P501", "S110", "1"),
    route_type = c("3", "3", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("school-p", "school-s", "public"),
    route_id = c("p-route", "s-route", "urban"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "tub")

  expect_identical(filtered$routes$route_id, "urban")
  expect_identical(filtered$trips$trip_id, "public")
  expect_setequal(filtered$removed_route_ids, c("p-route", "s-route"))
})

test_that("school-only transit policy removes the full MAT S family", {
  routes <- data.frame(
    route_id = c("s10", "s511", "urban"),
    route_short_name = c("S10", "S511", "1"),
    route_type = c("3", "200", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("school-10", "school-511", "public"),
    route_id = c("s10", "s511", "urban"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "mat")

  expect_identical(filtered$routes$route_id, "urban")
  expect_identical(filtered$trips$trip_id, "public")
  expect_setequal(filtered$removed_route_ids, c("s10", "s511"))
})

test_that("school-only transit policy removes the full Kicéo S family", {
  routes <- data.frame(
    route_id = c("70", "383", "urban"),
    route_short_name = c("S315_COLLEGE", "SXLL_304", "1"),
    route_type = c("3", "3", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("school-315", "school-304", "public"),
    route_id = c("70", "383", "urban"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "kiceo")

  expect_identical(filtered$routes$route_id, "urban")
  expect_identical(filtered$trips$trip_id, "public")
  expect_setequal(filtered$removed_route_ids, c("70", "383"))
})

test_that("school-only transit policy refuses to classify a non-bus route as school service", {
  routes <- data.frame(
    route_id = "26",
    route_type = "2",
    stringsAsFactors = FALSE
  )
  trips <- data.frame(route_id = "26", stringsAsFactors = FALSE)

  expect_error(
    filter_school_only_transit_routes(routes, trips, "nemus"),
    "non-bus"
  )
})

test_that("school-only transit policy rejects unknown feed prefixes", {
  routes <- data.frame(route_id = "1", route_type = "3",
                       stringsAsFactors = FALSE)
  trips <- data.frame(route_id = "1", stringsAsFactors = FALSE)

  expect_error(
    filter_school_only_transit_routes(routes, trips, "unknown"),
    "unknown transit school-service feed"
  )
})

test_that("school-only transit policy removes explicitly identified STAR routes", {
  routes <- data.frame(
    route_id = c("7-0401", "7-0001"),
    route_short_name = c("Ts1", "1"),
    route_type = c("3", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("school", "public"),
    route_id = c("7-0401", "7-0001"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "star")

  expect_identical(filtered$routes$route_id, "7-0001")
  expect_identical(filtered$trips$trip_id, "public")
  expect_identical(filtered$removed_route_ids, "7-0401")
})

test_that("school-only transit policy removes explicit school trips without deleting their route", {
  routes <- data.frame(
    route_id = "RIV:12599", route_type = "3",
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("school", "public"),
    route_id = c("RIV:12599", "RIV:12599"),
    trip_headsign = c("MAURON - MAIRIE Restaurant scolaire", "PLOERMEL"),
    trip_short_name = c("morning", "PLOERMEL"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "kor")

  expect_identical(filtered$routes$route_id, "RIV:12599")
  expect_identical(filtered$trips$trip_id, "public")
  expect_identical(filtered$removed_route_ids, character(0))
  expect_identical(filtered$removed_trip_ids, "school")
})

test_that("reviewed TBK school-period routes remain publicly accessible", {
  routes <- data.frame(
    route_id = c("212", "241"),
    route_type = c("3", "3"),
    stringsAsFactors = FALSE
  )
  trips <- data.frame(
    trip_id = c("212-trip", "241-trip"),
    route_id = c("212", "241"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_school_only_transit_routes(routes, trips, "tbk")

  expect_identical(filtered$routes$route_id, c("212", "241"))
  expect_identical(filtered$trips$trip_id, c("212-trip", "241-trip"))
  expect_length(filtered$removed_route_ids, 0L)
  expect_length(filtered$removed_trip_ids, 0L)
})

test_that("staging excludes route type 715 globally and records the rewrite", {
  fx <- fixture_promoted_manifest()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  source_dir <- file.path(fx$root, "on-demand")
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(route_id = c("a:715", "a:3"), route_type = c("715", "3"),
               stringsAsFactors = FALSE),
    file.path(source_dir, "routes.txt"), row.names = FALSE, quote = FALSE
  )
  utils::write.csv(
    data.frame(trip_id = c("on-demand", "public"),
               route_id = c("a:715", "a:3"), stringsAsFactors = FALSE),
    file.path(source_dir, "trips.txt"), row.names = FALSE, quote = FALSE
  )
  zip_path <- file.path(fx$root, "on-demand.zip")
  old <- setwd(source_dir)
  on.exit(setwd(old), add = TRUE)
  zip::zip(zip_path, files = c("routes.txt", "trips.txt"))
  setwd(old)
  cached <- file.path(fx$data_dir, "downloads", "derived", "a__feed-a.zip")
  file.copy(zip_path, cached, overwrite = TRUE)
  manifest <- jsonlite::fromJSON(fx$manifest_path, simplifyVector = FALSE)
  manifest$sources$gtfsx_a$sha256 <- sha256_file(cached)
  jsonlite::write_json(manifest, fx$manifest_path, auto_unbox = TRUE,
                       pretty = TRUE)

  block <- stage_transit_feeds(
    file.path(fx$root, "network"), data_dir = fx$data_dir,
    manifest_path = fx$manifest_path
  )
  ids <- vapply(block$feeds, `[[`, "", "id")
  record <- block$feeds[[which(ids == "gtfsx_a")]]
  staged <- file.path(fx$root, "network", "a__feed-a.zip")
  routes <- .gtfs_read_member(staged, "routes.txt", required = TRUE)
  trips <- .gtfs_read_member(staged, "trips.txt", required = TRUE)

  expect_identical(routes$route_id, "a:3")
  expect_identical(trips$trip_id, "public")
  expect_identical(record$source_sha256, manifest$sources$gtfsx_a$sha256)
  expect_identical(record$route_type_filter$excluded_route_types, "715")
  expect_identical(record$route_type_filter$removed_route_ids, "a:715")
  expect_identical(record$route_type_filter$removed_trip_ids, "on-demand")
})
