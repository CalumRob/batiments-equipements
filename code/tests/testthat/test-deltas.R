library(testthat)
library(data.table)

# The package did not previously have a test harness.  Source the module here
# so these tests exercise its exported contract without loading optional
# routing dependencies or any external snapshot artifacts.
source(testthat::test_path("../../R/deltas.R"), local = TRUE)

test_that("the legacy kept-list contract is explicit", {
  expect_length(legacy_routed_types(), 53)
  expect_true(all(grepl("^[A-Z][0-9]{3}$", legacy_routed_types())))

  divergence <- legacy_kept_list_divergence()
  expect_setequal(divergence$recorded_but_not_routed, c("C304", "C305"))
  expect_identical(divergence$routed_but_not_recorded, "F101")
})

test_that("the snapshot map has nine car comparisons and no walk comparisons", {
  mapping <- legacy_snapshot_map()

  expect_equal(nrow(mapping), 18)
  expect_equal(sum(mapping$comparable), 9)
  expect_equal(sum(!mapping$comparable), 9)
  expect_true(all(!is.na(mapping$snapshot[mapping$comparable])))
  expect_true(all(is.na(mapping$snapshot[!mapping$comparable])))

  aggregate <- data.table(
    code_insee = "35001", mode = "car", nb_buildings = 1,
    share_alimentation = 1, share_sante = 1, share_administration = 1,
    share_ecole = 1, share_banque = 1, avg_diversity = 1, avg_total = 1,
    pct_iso_full = 0
  )
  snapshot <- data.table(
    code_insee = "35001", nom_commune = "Test", nb_buildings = 1,
    share_food_c = 1, share_health_c = 1, share_admin_c = 1,
    share_school_c = 1, share_bank_c = 1, avg_div_car = 1,
    avg_tot_car = 1, pct_iso_full_c = 0
  )
  deltas <- derive_deltas(aggregate, snapshot)
  expect_equal(sort(unique(deltas$metric)), sort(mapping$metric[mapping$comparable]))
  expect_false(any(deltas$metric %in% c("share_food_t", "avg_div_t", "pct_iso_full_t")))
})

test_that("the legacy snapshot reader normalises, filters, and types CSV values", {
  path <- file.path(tempdir(), "legacy-snapshot-test.csv")
  on.exit(unlink(path), add = TRUE)
  snapshot <- data.table(
    code_insee = c(123, 35001), nom_commune = c("Fougères", "Rennes"),
    nb_buildings = c("10", "20"), share_food_c = c("0.2", "0.4"),
    share_health_c = c("0.3", "0.5"), share_admin_c = c("0.4", "0.6"),
    share_school_c = c("0.5", "0.7"), share_bank_c = c("0.6", "0.8"),
    avg_div_car = c("2.5", "3.5"), avg_tot_car = c("8", "9"),
    pct_iso_full_c = c("0", "0.1")
  )
  fwrite(snapshot, path, bom = TRUE)

  actual <- read_legacy_snapshot(path, code_insee = "00123")
  expect_equal(actual$code_insee, "00123")
  expect_equal(actual$nom_commune, "Fougères")
  numeric_columns <- setdiff(names(actual), c("code_insee", "nom_commune"))
  expect_true(all(vapply(actual, is.numeric, logical(1))[numeric_columns]))
  expect_true(data.table::haskey(actual))
  expect_identical(key(actual), "code_insee")
})

make_delta_fixture <- function() {
  metrics <- c(
    "nb_buildings", "share_alimentation", "share_sante", "share_administration",
    "share_ecole", "share_banque", "avg_diversity", "avg_total", "pct_iso_full"
  )
  agg <- data.table(code_insee = c("35001", "35002", "35003"), mode = "car")
  snap <- data.table(code_insee = c("35001", "35002"), nom_commune = c("Centre", "Nord"))
  for (metric in metrics) {
    agg[[metric]] <- c(12, if (metric == "nb_buildings") 30 else if (metric %in% c("avg_diversity", "avg_total")) 2 else 1, 12)
    snap_metric <- c("nb_buildings" = "nb_buildings", "share_alimentation" = "share_food_c",
                     "share_sante" = "share_health_c", "share_administration" = "share_admin_c",
                     "share_ecole" = "share_school_c", "share_banque" = "share_bank_c",
                     "avg_diversity" = "avg_div_car", "avg_total" = "avg_tot_car",
                     "pct_iso_full" = "pct_iso_full_c")[[metric]]
    snap[[snap_metric]] <- if (metric == "nb_buildings") c(10, 10) else if (metric %in% c("avg_diversity", "avg_total")) c(1, 1) else c(1, 1)
  }
  list(agg = agg, snap = snap)
}

test_that("derive_deltas classifies expected, flagged, missing, and border deltas", {
  fixture <- make_delta_fixture()
  deltas <- derive_deltas(fixture$agg, fixture$snap, nb_prior = 1.2, border_communes = "35001")

  expect_equal(nrow(deltas), 27)
  expect_equal(length(unique(deltas$metric)), 9)
  expect_equal(deltas[code_insee == "35003", unique(classification)], "missing")
  expect_equal(deltas[code_insee == "35002" & metric == "nb_buildings", classification], "flag")
  expect_equal(deltas[code_insee == "35001" & metric == "nb_buildings", classification], "expected")
  expect_equal(deltas[code_insee == "35001" & metric == "avg_diversity", classification], "expected")
  expect_match(deltas[code_insee == "35001" & metric == "avg_diversity", reason], "border widening 15->25 km")
  expect_equal(deltas[code_insee == "35002" & metric == "avg_diversity", classification], "flag")
  expect_match(deltas[code_insee == "35002" & metric == "avg_diversity", reason], "verify at TYPEQU level")
})

test_that("the rendered report exposes method, classifications, and walk scope", {
  fixture <- make_delta_fixture()
  deltas <- derive_deltas(fixture$agg, fixture$snap, nb_prior = 1.2, border_communes = "35001")
  walk <- fixture$agg[mode == "car"][, mode := "walk"]
  report <- paste(render_deltas_report(
    deltas, walk_agg = walk, nb_prior = 1.2, snapshot_path = "toy.csv",
    date = as.Date("2026-08-18")
  ), collapse = "\n")

  expect_match(report, "## Method")
  expect_match(report, "53 routed vs 54 recorded")
  expect_match(report, "## Per-commune deltas")
  expect_match(report, "**flag**", fixed = TRUE)
  expect_match(report, "## Flagged anomalies")
  expect_match(report, "## Walk axis: NON-COMPARABLE")
  expect_match(report, "no pure-walk axis")
  expect_match(report, "toy.csv")
  expect_match(report, "35001")
})
