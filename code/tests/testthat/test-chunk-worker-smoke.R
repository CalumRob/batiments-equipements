library(testthat)
library(data.table)

# The TRUE process boundary (#21): a real Rscript child. Everything else in
# the #21 suite is JVM-free by injection; this file alone crosses into real
# processes and is GATED on the durable fixtures — worktrees carry no data/
# (gitignored), so it skips cleanly there; the orchestrator exercises it
# post-merge in the main checkout where the durable root lives.
#
# What it proves, per the acceptance box:
#   * CLI contract — spawn_chunk_child -> worker_bootstrap.R -> request.json,
#     exit codes 0 / 3;
#   * heap-before-rJava ordering — LIVE: the bootstrap logs its heap and
#     whether Java was already loaded at that instant, before any package
#     source is pulled in.

test_that("the generated bootstrap sets the heap before any source load (static)", {
  s <- chunk_worker_bootstrap_script()
  heap_line <- grep("options[(]java[.]parameters", s)
  log_line <- grep("rjava_loaded", s)
  src_line <- grep("source[(]f[)]", s)
  main_line <- grep("chunk_worker_main", s)
  expect_length(heap_line, 1L)
  expect_true(heap_line < src_line)   # D6: heap BEFORE anything that loads rJava
  expect_true(heap_line < main_line)
  expect_true(log_line < src_line)    # the ordering proof is emitted pre-load
})

run_dir_has_committed_network <- function(data_dir) {
  nets <- file.path(data_dir, "networks")
  if (!dir.exists(nets)) return(character(0))
  candidates <- list.dirs(nets, recursive = TRUE)
  candidates[vapply(candidates, function(d) {
    file.exists(file.path(d, "network.dat")) &&
      file.exists(file.path(d, ".network-identity.json"))
  }, logical(1L))]
}

test_that("a real Rscript child fails with exit code 3 on an unreadable request", {
  skip_if(!dir.exists(file.path("..", "data")),
          "durable fixtures absent (data/ lives only in the main checkout)")
  run_dir <- file.path("..", "data", "matrice", "_smoke21-cli")
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(run_dir, recursive = TRUE, force = TRUE), add = TRUE)
  bootstrap <- write_worker_bootstrap(run_dir)

  # A request whose plan parquets do not exist: the worker must die loudly
  # and the bootstrap must map that to a nonzero exit.
  req_path <- file.path(run_dir, "requests", "chunk_1.json")
  dir.create(dirname(req_path), showWarnings = FALSE)
  chunk_request_save(
    list(
      kind = "matrice-chunk-request",
      request_version = 1L,
      run_label = "_smoke21-cli",
      code_dir = normalizePath(getwd(), winslash = "/"),
      heap = "-Xmx24G",
      network_dir = file.path("..", "data", "networks", "does-not-exist"),
      chunk_id = 1L,
      chunk_size = 10L,
      n_origin_coords = 1L,
      modes = "walk",
      paths = list(
        origin_points = file.path(run_dir, "plan", "origin_points.parquet"),
        origin_link = file.path(run_dir, "plan", "origin_link.parquet"),
        destination_points = file.path(run_dir, "plan",
                                       "destination_points.parquet"),
        destination_link = file.path(run_dir, "plan",
                                     "destination_link.parquet"),
        destination_map = file.path(run_dir, "plan", "destination_map.parquet"),
        artifacts_dir = file.path(run_dir, "chunks"),
        receipts_dir = file.path(run_dir, "receipts")),
      routing = list(walk_speed = 4, elevation = "NONE", n_threads = NULL)),
    req_path)

  res <- spawn_chunk_child(bootstrap, normalizePath(req_path))
  expect_identical(as.integer(res$status), 3L)
  expect_match(res$stderr, "chunk worker failed|not found|missing")
})

test_that("the true boundary end-to-end: real Rscript + committed network routes one chunk", {
  skip_if(!dir.exists(file.path("..", "data")),
          "durable fixtures absent (data/ lives only in the main checkout)")
  networks <- run_dir_has_committed_network(file.path("..", "data"))
  skip_if(length(networks) == 0L,
          "no committed network cache yet (#22 kicks the Bretagne build)")

  data_dir <- file.path("..", "data")
  run_dir <- file.path(data_dir, "matrice",
                       sprintf("_smoke21-%d", Sys.getpid()))
  # Test-owned scratch under the durable root: plain unlink cleanup, NOT
  # safe_remove (the guardrail rightly refuses everything in there).
  on.exit(unlink(normalizePath(run_dir, winslash = "/", mustWork = FALSE),
                 recursive = TRUE, force = TRUE), add = TRUE)

  # A tiny synthetic universe near Fougeres: 2 routing coordinates x 2.
  origins <- data.table::data.table(
    id = c("smoke_b1", "smoke_b2"),
    lon = c(-1.35, -1.352), lat = c(48.35, 48.351))
  dests <- data.table::data.table(
    id = sprintf("bpe_listing_%06d", 1:2),
    lon = c(-1.3505, -1.3510), lat = c(48.3504, 48.3508),
    TYPEQU = c("B104", "D265"))

  fx <- fixture_run_layout(label = basename(run_dir),
                           root = dirname(dirname(file.path(data_dir))),
                           chunk_size = 10L)
  # Rebuild the plan files inside the durable-root run dir instead of the
  # fixture's temp layout (the child reads them from the request paths).
  plan_dir <- file.path(run_dir, "plan")
  dir.create(plan_dir, recursive = TRUE, showWarnings = FALSE)
  op <- coordinate_routing_plan(origins, prefix = "coord_o")
  dp <- coordinate_routing_plan(dests[, .(id, lon, lat)], prefix = "coord_d")
  arrow::write_parquet(op$points, file.path(plan_dir, "origin_points.parquet"))
  arrow::write_parquet(op$link, file.path(plan_dir, "origin_link.parquet"))
  arrow::write_parquet(dp$points, file.path(plan_dir, "destination_points.parquet"))
  arrow::write_parquet(dp$link, file.path(plan_dir, "destination_link.parquet"))
  arrow::write_parquet(dests[, .(id, TYPEQU)],
                       file.path(plan_dir, "destination_map.parquet"))

  req <- list(
    kind = "matrice-chunk-request",
    request_version = 1L,
    run_label = basename(run_dir),
    code_dir = normalizePath(getwd(), winslash = "/"),
    heap = "-Xmx24G",
    network_dir = normalizePath(networks[[1L]], winslash = "/"),
    chunk_id = 1L,
    chunk_size = 10L,
    n_origin_coords = nrow(op$points),
    modes = "walk",
    paths = list(
      origin_points = file.path(plan_dir, "origin_points.parquet"),
      origin_link = file.path(plan_dir, "origin_link.parquet"),
      destination_points = file.path(plan_dir, "destination_points.parquet"),
      destination_link = file.path(plan_dir, "destination_link.parquet"),
      destination_map = file.path(plan_dir, "destination_map.parquet"),
      artifacts_dir = file.path(run_dir, "chunks"),
      receipts_dir = file.path(run_dir, "receipts")),
    routing = list(walk_speed = 4, max_trip_duration = cap_minutes(),
                   elevation = "NONE", n_threads = NULL))

  req_path <- file.path(run_dir, "requests", "chunk_1.json")
  chunk_request_save(req, req_path)
  bootstrap <- write_worker_bootstrap(run_dir)

  res <- spawn_chunk_child(bootstrap, normalizePath(req_path, mustWork = TRUE))

  # CLI contract: success exits 0; the FIRST stdout line is the bootstrap's
  # heap-ordering proof (emitted before any source load).
  expect_identical(as.integer(res$status), 0L,
                   info = paste("stderr:", res$stderr))
  first_line <- strsplit(res$stdout, "\n", fixed = TRUE)[[1L]][[1L]]
  boot <- jsonlite::fromJSON(first_line)
  expect_identical(boot$bootstrap, "matrice-chunk-worker")
  expect_identical(as.character(boot$heap), "-Xmx24G")
  expect_false(isTRUE(boot$rjava_loaded))   # heap was set BEFORE rJava existed

  # The receipt + artifact exist, and BOTH validation layers agree.
  rec <- read_chunk_receipt(chunk_receipt_path(file.path(run_dir, "receipts"),
                                               "walk", 1L))
  expect_identical(rec$status, "complete")
  artifact <- file.path(run_dir, "chunks", "walk_1.parquet")
  validate_chunk_artifact(artifact, rec)
})
