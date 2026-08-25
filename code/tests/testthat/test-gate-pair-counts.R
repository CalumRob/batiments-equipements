library(testthat)
library(data.table)

# Pair-count profile (#22 gate deliverable): every (mode x chunk) receipt and
# manifest entry records the coordinate-level pairs the router returned and
# what expansion restored them to. The release operator's throughput evidence
# rides in the same trust boundary as n_rows — not in a side channel.

test_that("the child receipt records routed and identity pair counts", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  req <- fixture_chunk_request(fx, 1L, modes = c("walk", "transit"))

  out <- run_chunk_worker(req, router = stub_mode_dispatch(), network = NULL)

  # Paper-verifiable against the stub geometry: chunk 1 routes coord_o_1 +
  # coord_o_2 (2 coordinates) x 2 destination coordinates, all four pairs
  # within the cap -> 4 routed pairs; expansion restores
  # (2 origin ids x 2 dest ids) + (2 x 1) + (1 x 2) + (1 x 1) = 9 identity
  # pairs; derivation aggregates those to 9 sparse rows.
  for (mode in c("walk", "transit")) {
    rec <- read_chunk_receipt(chunk_receipt_path(fx$receipts_dir, mode, 1L))
    expect_identical(as.integer(rec$n_routed_pairs), 4L)
    expect_identical(as.integer(rec$n_identity_pairs), 9L)
    m <- read_matrix(out$artifacts[[paste0(mode, "_1")]])
    expect_identical(as.integer(rec$n_rows), nrow(m))
  }
})

test_that("a fully unreachable chunk records zero on both pair counts", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  req <- fixture_chunk_request(fx, 1L, modes = "walk")
  empty_router <- function(network, origins, destinations, mode) {
    data.table::data.table(from_id = character(0), to_id = character(0),
                           travel_time = numeric(0))
  }
  run_chunk_worker(req, router = empty_router, network = NULL)
  rec <- read_chunk_receipt(chunk_receipt_path(fx$receipts_dir, "walk", 1L))
  expect_identical(as.integer(rec$n_routed_pairs), 0L)
  expect_identical(as.integer(rec$n_identity_pairs), 0L)
})

test_that("the orchestrator's completed entries carry the pair counts", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fixture_run_args(fx)
  args$spawn_child <- scripted_spawn()
  res <- do.call(run_resumable, args)
  expect_true(res$complete)

  m <- load_run_manifest(fx$manifest_path)
  # Chunk 1 routes coord_o_1 + coord_o_2 -> all four coordinate pairs within
  # the cap (4 routed); expansion restores 9 identity pairs. Chunk 2 routes
  # coord_o_3 + coord_o_4, of which only coord_o_3 reaches the two destination
  # coordinates (2 routed -> 1 x (2+1) = 3 identity pairs).
  expected <- list("1" = c(4L, 9L), "2" = c(2L, 3L))
  for (id in names(m$entries)) {
    e <- m$entries[[id]]
    expect_identical(e$status, "complete")
    exp <- expected[[sub("^.*_", "", id)]]
    expect_identical(as.integer(e$n_routed_pairs), exp[[1L]],
                     info = paste(id, "routed"))
    expect_identical(as.integer(e$n_identity_pairs), exp[[2L]],
                     info = paste(id, "identity"))
  }
})

test_that("run_metadata.json sums the pair-count profile per mode", {
  fx <- fixture_run_layout()
  on.exit(unlink(fx$root, recursive = TRUE, force = TRUE), add = TRUE)
  args <- fixture_run_args(fx)
  args$spawn_child <- scripted_spawn()
  res <- do.call(run_resumable, args)
  expect_true(res$complete)

  meta <- jsonlite::fromJSON(file.path(fx$run_dir, "run_metadata.json"),
                             simplifyVector = FALSE)
  # Per mode: 2 chunks x (4 + 2 routed) = 6 routed; (9 + 3) = 12 identity.
  for (md in c("walk", "car")) {
    pm <- meta$per_mode[[md]]
    expect_identical(as.integer(pm$n_routed_pairs[[1]]), 6L)
    expect_identical(as.integer(pm$n_identity_pairs[[1]]), 12L)
  }
})
