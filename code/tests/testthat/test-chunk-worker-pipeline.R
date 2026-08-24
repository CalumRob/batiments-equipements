library(testthat)
library(data.table)

# The chunk CHILD (#21): one worker invocation routes exactly ONE chunk
# across ALL its modes and reports one receipt per (mode x chunk). With a
# router injected (stub_route_pairs precedent) the ENTIRE pipeline runs
# headless: plan slice -> route -> expand -> derive -> temp-write -> rename ->
# validate -> sha256 -> receipt. No JVM, no network build, no manifest access.

test_that("chunk requests round-trip through versioned JSON", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  req <- fixture_chunk_request(fx, 2L, modes = c("walk", "transit"))
  p <- file.path(fx$requests_dir, "chunk_2.json")
  chunk_request_save(req, p)
  back <- chunk_request_load(p)
  expect_identical(back$kind, "matrice-chunk-request")
  expect_identical(as.integer(back$chunk_id), 2L)
  expect_identical(back$modes, c("walk", "transit"))
  expect_identical(back$paths$artifacts_dir, req$paths$artifacts_dir)
  # NULL n_threads loads as r5r's default (Inf) via as_chunk_request.
  expect_identical(back$routing$n_threads, Inf)
  expect_error(chunk_request_load(file.path(tempdir(), "no-such-request.json")),
               "request")
})

test_that("the chunk slice arithmetic matches the orchestrator's census exactly", {
  fx <- fixture_run_layout()   # 4 unique origin coords, chunk_size 2
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  pts <- fx$origin_plan$points
  s1 <- chunk_point_slice(pts, 1L, 2L)
  s2 <- chunk_point_slice(pts, 2L, 2L)
  expect_identical(s1$id, pts$id[1:2])
  expect_identical(s2$id, pts$id[3:4])   # last partial chunk takes the tail
  expect_error(chunk_point_slice(pts, 3L, 2L), "chunk")
  expect_error(chunk_point_slice(pts, 0L, 2L), "chunk")
})

test_that("an injected router drives the full child pipeline headless", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  req <- fixture_chunk_request(fx, 1L, modes = c("walk", "car"))

  seen <- new.env(parent = emptyenv())
  seen$modes <- character(0); seen$slices <- list(); seen$networks <- list()
  spy_router <- function(network, origins, destinations, mode) {
    seen$modes <- c(seen$modes, mode)
    seen$slices[[length(seen$slices) + 1L]] <- origins$id
    seen$networks[[length(seen$networks) + 1L]] <- network
    stub_route_pairs(origins, destinations)
  }

  out <- run_chunk_worker(req, router = spy_router, network = NULL)

  # One route call per mode, each with ONLY this chunk's plan slice.
  expect_setequal(seen$modes, c("walk", "car"))
  for (s in seen$slices) {
    expect_identical(s, chunk_point_slice(fx$origin_plan$points, 1L, 2L)$id)
  }
  expect_true(all(vapply(seen$networks, is.null, logical(1))))

  # Artifacts at the deterministic per-(mode x chunk) final paths.
  walk_path <- file.path(fx$chunks_dir, "walk_1.parquet")
  car_path <- file.path(fx$chunks_dir, "car_1.parquet")
  expect_true(file.exists(walk_path))
  m <- read_matrix(walk_path)
  validate_matrix(m)
  expect_true(all(m$mode == "walk"))
  # Paper-verifiable (stub geometry): chunk 1 holds coord_o_1 (-1.35,48.11 ->
  # b1a+b1b) and coord_o_2 (-1.38,48.12 -> bx); both reach all three listings
  # within the cap -> 3 buildings x 3 reachable TYPEQU = 9 sparse rows;
  # b2/b3 belong to later chunks and must be absent.
  expect_equal(nrow(m), 9L)
  expect_setequal(unique(m$batiment_id), c("b1a", "b1b", "bx"))

  # Receipts: per-(mode x chunk), sha256 over the artifact bytes, UTC stamp.
  rec <- read_chunk_receipt(chunk_receipt_path(fx$receipts_dir, "walk", 1L))
  expect_identical(rec$status, "complete")
  expect_identical(rec$sha256, sha256_file(walk_path))
  expect_identical(as.integer(rec$n_rows), nrow(m))
  expect_match(rec$validated_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
  expect_gte(rec$route_seconds, 0)

  # The orchestrator's independent re-validation + checksum cross-check.
  expect_invisible(validate_chunk_artifact(walk_path, rec))
  tampered <- rec; tampered$sha256 <- strrep("0", 64)
  expect_error(validate_chunk_artifact(walk_path, tampered), "sha256")

  # Atomic write discipline: no PID-tagged temp residue anywhere.
  expect_length(list.files(fx$chunks_dir, pattern = "[.]tmp"), 0L)
  expect_length(list.files(fx$receipts_dir, pattern = "[.]tmp"), 0L)

  # Children NEVER touch the manifest — no manifest exists in this layout.
  expect_false(file.exists(fx$manifest_path))
})

test_that("transit rides the same pipeline with its percentile axis intact", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  req <- fixture_chunk_request(fx, 1L, modes = "transit")
  out <- run_chunk_worker(req, router = stub_mode_dispatch(), network = NULL)
  p <- out$artifacts[["transit_1"]]
  m <- read_matrix(p)
  validate_matrix(m)   # transit is an axis extension: p1 primary readings
  expect_true(all(c("travel_time_p1", "travel_time_p50",
                    "count_5_p50") %in% names(m)))
})

test_that("a fully unreachable chunk still writes a valid empty artifact", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  req <- fixture_chunk_request(fx, 2L, modes = "walk")
  empty_router <- function(network, origins, destinations, mode) {
    data.table::data.table(from_id = character(0), to_id = character(0),
                           travel_time = numeric(0))
  }
  out <- run_chunk_worker(req, router = empty_router, network = NULL)
  m <- read_matrix(out$artifacts[["walk_2"]])
  expect_equal(nrow(m), 0L)
  validate_matrix(m)   # sparse contract: zero reachable rows validates
  rec <- read_chunk_receipt(chunk_receipt_path(fx$receipts_dir, "walk", 2L))
  expect_identical(as.integer(rec$n_rows), 0L)
})
