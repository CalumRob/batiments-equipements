# S3 — derive_building_metrics(): kept-list selection, the div_loss family,
# and per-cluster access flags, at a ladder-rung threshold, vs a reference mode.
# Expected values are hand-worked literals from the fixture (see below).

test_that("kept is required — no default kept-list is baked in (#198 decides it)", {
  fx <- make_fixture()
  expect_error(derive_building_metrics(fx$matrix), "kept")
})

test_that("threshold must be a ladder rung", {
  fx <- make_fixture()
  expect_error(derive_building_metrics(fx$matrix, fx$kept, threshold = 12), "ladder")
})

# Worked example (T = 20, ref = car), read off the fixture by hand:
#   diversity = # kept types with count_20 >= 1;  total = sum(count_20)
#   div_loss = car_diversity - walk_diversity;    tot_loss = car_total - walk_total
#   b1 urban: walk 6/7, car 7/8  -> loss 1/1
#   b2 walk-poor: walk 0/0, car 7/8 -> loss 7/8 (the car-dependent building)
#   b3 rural: walk 0/0, car 5/5 (A203 tt21 past the rung) -> loss 5/5
#   b4 deep rural: no walk rows at all, car 2/2 -> loss 2/2
expected_building_metrics <- data.table::setorder(
  data.table::data.table(
    batiment_id = c("b1", "b1", "b2", "b2", "b3", "b3", "b4", "b4"),
    mode = c("car", "walk", "car", "walk", "car", "walk", "car", "walk"),
    diversity = c(7L, 6L, 7L, 0L, 5L, 0L, 2L, 0L),
    total = c(8L, 7L, 8L, 0L, 5L, 0L, 2L, 0L),
    div_loss = c(0L, 1L, 0L, 7L, 0L, 5L, 0L, 2L),
    tot_loss = c(0L, 1L, 0L, 8L, 0L, 5L, 0L, 2L),
    has_alimentation = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    has_sante = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    has_administration = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
    has_ecole = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
    has_banque = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  ),
  batiment_id, mode
)

test_that("the full building reading table matches the hand-worked values", {
  fx <- make_fixture()
  got <- derive_building_metrics(fx$matrix, fx$kept)
  # compare as data.frames: the sorted-key attribute is an implementation detail
  expect_equal(as.data.frame(got), as.data.frame(expected_building_metrics))
})

test_that("non-kept types are excluded (the kept-list selection works)", {
  fx <- make_fixture()
  got <- derive_building_metrics(fx$matrix, fx$kept)
  # b1 walk would be diversity 7 (total 8) if F999 leaked in; it is 6/7
  expect_equal(got[batiment_id == "b1" & mode == "walk", diversity], 6L)
  expect_equal(got[batiment_id == "b1" & mode == "walk", total], 7L)
})

test_that("the kept list is a parameter, not baked in (#198's decoupling)", {
  fx <- make_fixture()
  # keeping only the food cluster's two fixture types
  got <- derive_building_metrics(fx$matrix, c("B104", "B105"))
  expect_equal(got[batiment_id == "b1" & mode == "walk", diversity], 2L)
  expect_equal(got[batiment_id == "b1" & mode == "walk", total], 3L)  # 2 + 1
  # a building with no kept access in a mode still gets a zero row
  expect_equal(got[batiment_id == "b4" & mode == "walk", diversity], 0L)
})

test_that("a building with no rows in a mode gets an all-zero reading (sparse contract)", {
  fx <- make_fixture()
  got <- derive_building_metrics(fx$matrix, fx$kept)
  b4w <- got[batiment_id == "b4" & mode == "walk"]
  expect_equal(nrow(b4w), 1L)
  expect_equal(b4w$diversity, 0L)
  expect_false(any(as.logical(b4w[, cluster_flag_cols(), with = FALSE])))
})

test_that("the threshold parameter changes the readings (T = 15 worked example)", {
  fx <- make_fixture()
  got <- derive_building_metrics(fx$matrix, fx$kept, threshold = 15)
  # b1 walk at 15: B104, B105, D265, A129 (C108 tt20, A203 tt18 drop out)
  expect_equal(got[batiment_id == "b1" & mode == "walk", diversity], 4L)
  expect_equal(got[batiment_id == "b1" & mode == "walk", total], 4L)
  # car unchanged at 15: 7/8 -> loss 3/4
  expect_equal(got[batiment_id == "b1" & mode == "walk", div_loss], 3L)
  expect_equal(got[batiment_id == "b1" & mode == "walk", tot_loss], 4L)
})
