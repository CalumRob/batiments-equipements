# S2 — make_fixture(): the deterministic synthetic fixture for the tracer.
# A tiny Breton dataset: 4 buildings, 2 communes, 2 EPCI, 2 déps, 8 TYPEQU
# (7 kept + 1 non-kept), modes walk + car. Every value hand-picked so each
# derivation in S3/S4 is hand-checkable (see the worked examples there).

test_that("make_fixture returns matrix, crosswalk and kept list", {
  fx <- make_fixture()
  expect_type(fx, "list")
  expect_named(fx, c("matrix", "crosswalk", "kept"))
  expect_true(is.data.frame(fx$matrix))
  expect_true(is.data.frame(fx$crosswalk))
  expect_type(fx$kept, "character")
})

test_that("the fixture is deterministic", {
  fx1 <- make_fixture()
  fx2 <- make_fixture()
  expect_identical(fx1$matrix, fx2$matrix)
  expect_identical(fx1$crosswalk, fx2$crosswalk)
})

test_that("the fixture matrix is a conforming artifact", {
  expect_true(validate_matrix(make_fixture()$matrix))
})

test_that("the fixture is tiny and hand-checkable in shape", {
  m <- make_fixture()$matrix
  # 4 buildings x 2 modes; sparse rows: 38 in total (see make_fixture docs)
  expect_equal(unique(m$batiment_id), c("b1", "b2", "b3", "b4"))
  expect_setequal(unique(m$mode), c("walk", "car"))
  expect_equal(nrow(m), 38L)
  # b4 has NO walk rows (deep rural — nothing within the 30-min cap)
  expect_equal(nrow(m[batiment_id == "b4" & mode == "walk"]), 0L)
  # b3 has exactly one walk row (only B104 within the cap)
  expect_equal(nrow(m[batiment_id == "b3" & mode == "walk"]), 1L)
  # every row is consistent (the validator's own invariant, re-stated)
  for (r in ladder_rungs()) {
    col <- paste0("count_", r)
    expect_true(all((m[[col]] >= 1) == (m$tt_nearest <= r)))
  }
})

test_that("spot-checked rows carry the hand-picked values", {
  m <- make_fixture()$matrix
  row <- function(b, t, mo) m[batiment_id == b & TYPEQU == t & mode == mo]
  expect_equal(row("b1", "B104", "walk")$tt_nearest, 8)
  expect_equal(row("b1", "B104", "walk")$count_20, 2L)
  expect_equal(row("b1", "B204", "walk")$count_20, 0L)   # reachable at 25, not at 20
  expect_equal(row("b3", "A203", "car")$count_20, 0L)    # tt 21, just past the rung
  expect_equal(row("b4", "B104", "car")$tt_nearest, 18)
})

test_that("the crosswalk links every building to its territory", {
  xw <- make_fixture()$crosswalk
  expect_named(xw, c("batiment_id", "code_insee", "nom_commune", "epci", "code_departement", "region"))
  expect_equal(nrow(xw), 4L)
  expect_setequal(xw$batiment_id, c("b1", "b2", "b3", "b4"))
  # b1/b2 in commune 35101 (dép 35), b3/b4 in 56101 (dép 56)
  expect_equal(xw$code_insee[xw$batiment_id %in% c("b1", "b2")], c("35101", "35101"))
  expect_equal(xw$code_insee[xw$batiment_id %in% c("b3", "b4")], c("56101", "56101"))
  expect_true(all(xw$region == "Bretagne"))
})

test_that("the fixture kept list keeps 7 of the 8 types (F999 is non-kept)", {
  fx <- make_fixture()
  expect_length(fx$kept, 7L)
  expect_false("F999" %in% fx$kept)
  expect_true(all(fx$kept %in% fx$matrix$TYPEQU))
  expect_true(all(fx$kept %in% kept_list_bpe2024()))  # realistic codes, BPE-2024 vintage
})
