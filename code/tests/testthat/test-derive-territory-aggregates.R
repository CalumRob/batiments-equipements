# S4 — derive_territory_aggregates(): per-commune/EPCI/dép/région aggregates —
# access shares, isolation shares, and the div_loss/tot_loss stats.
# Expected values are hand-worked from the S3 building readings (T = 20, ref = car).

test_that("level must be a known territory level", {
  fx <- make_fixture()
  m <- derive_building_metrics(fx$matrix, fx$kept)
  expect_error(derive_territory_aggregates(m, fx$crosswalk, level = "pays"), "level")
})

test_that("the crosswalk must carry the grouping column", {
  fx <- make_fixture()
  m <- derive_building_metrics(fx$matrix, fx$kept)
  bad <- data.table::copy(fx$crosswalk)
  bad[, code_insee := NULL]
  expect_error(derive_territory_aggregates(m, bad, level = "commune"), "code_insee")
})

# Worked example, commune level (c1 = b1+b2, c2 = b3+b4):
#   c1 walk: b1 has everything (6/7, all clusters), b2 has nothing (0/0)
#     -> shares 0.5, pct_iso 0.5 (b2 car-only), pct_iso_full 0.5, avg_div_loss 4
#   c1 car: both 7/8, all clusters -> shares 1, pct_iso 0
#   c2 walk: neither building reaches anything by walk
#     -> shares 0, pct_iso food/sante 1, admin/ecole 0.5 (b4 has no car admin),
#        banque 0 (b3's car bank is past the rung, b4 has none), pct_iso_full 1
#   c2 car: b3 5/5 (bank F), b4 2/2 (admin/ecole/banque F)
#     -> shares food/sante 1, admin/ecole 0.5, banque 0, avg_diversity 3.5
expected_commune <- data.table::setorder(
  data.table::data.table(
    code_insee = c("35101", "35101", "56101", "56101"),
    mode = c("car", "walk", "car", "walk"),
    nb_buildings = c(2L, 2L, 2L, 2L),
    share_alimentation = c(1, 0.5, 1, 0),
    share_sante = c(1, 0.5, 1, 0),
    share_administration = c(1, 0.5, 0.5, 0),
    share_ecole = c(1, 0.5, 0.5, 0),
    share_banque = c(1, 0.5, 0, 0),
    pct_iso_alimentation = c(0, 0.5, 0, 1),
    pct_iso_sante = c(0, 0.5, 0, 1),
    pct_iso_administration = c(0, 0.5, 0, 0.5),
    pct_iso_ecole = c(0, 0.5, 0, 0.5),
    pct_iso_banque = c(0, 0.5, 0, 0),
    pct_iso_full = c(0, 0.5, 0, 1),
    avg_diversity = c(7, 3, 3.5, 0),
    avg_total = c(8, 3.5, 3.5, 0),
    avg_div_loss = c(0, 4, 0, 3.5),
    med_div_loss = c(0, 4, 0, 3.5),
    avg_tot_loss = c(0, 4.5, 0, 3.5),
    med_tot_loss = c(0, 4.5, 0, 3.5)
  ),
  code_insee, mode
)

test_that("commune aggregates match the hand-worked values", {
  fx <- make_fixture()
  m <- derive_building_metrics(fx$matrix, fx$kept)
  got <- derive_territory_aggregates(m, fx$crosswalk, level = "commune")
  expect_equal(as.data.frame(got), as.data.frame(expected_commune))
})

test_that("departement and epci levels group the same buildings", {
  fx <- make_fixture()
  m <- derive_building_metrics(fx$matrix, fx$kept)
  dep <- derive_territory_aggregates(m, fx$crosswalk, level = "departement")
  # one dép per commune here: dép 35 = c1, dép 56 = c2
  expect_equal(dep$code_departement, c("35", "35", "56", "56"))
  expect_equal(dep$avg_div_loss[dep$mode == "walk"], c(4, 3.5))
  epci <- derive_territory_aggregates(m, fx$crosswalk, level = "epci")
  expect_equal(epci$epci, c("EPCI Centre", "EPCI Centre", "EPCI Rural", "EPCI Rural"))
})

test_that("the region level aggregates all four buildings", {
  fx <- make_fixture()
  m <- derive_building_metrics(fx$matrix, fx$kept)
  reg <- derive_territory_aggregates(m, fx$crosswalk, level = "region")
  expect_equal(nrow(reg), 2L)  # one row per mode
  expect_true(all(reg$region == "Bretagne"))
  expect_equal(reg$nb_buildings[reg$mode == "walk"], 4L)
  # walk isolation across Bretagne: b2, b3, b4 isolated from food by walk (b1 not)
  expect_equal(reg$pct_iso_alimentation[reg$mode == "walk"], 0.75)
})
