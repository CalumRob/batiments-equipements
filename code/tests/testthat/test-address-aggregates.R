library(testthat)
library(data.table)

source_project_r("constants.R")
source_project_r("address-aggregates.R")

test_that("address/type aggregation preserves the full address denominator", {
  spine <- data.table(
    address_id = c("a1", "a2", "a3"),
    code_insee = c("35001", "35001", "35002"),
    nom_commune = c("Centre", "Centre", "Rural")
  )
  metrics <- data.table(
    address_id = c("a1", "a2"),
    TYPEQU = c("A", "A"),
    count_5_walk = c(2L, 0L)
  )

  got <- aggregate_address_type_metrics(
    metrics = metrics,
    address_spine = spine,
    type_catalogue = c("A", "B"),
    value_columns = "count_5_walk"
  )

  expect_equal(nrow(got), 4L)
  centre_a <- got[code_insee == "35001" & TYPEQU == "A"]
  expect_equal(centre_a$n_addresses, 2L)
  expect_equal(centre_a$n_observed, 2L)
  expect_equal(centre_a$count_5_walk_mean, 1)
  expect_equal(centre_a$count_5_walk_share, 0.5)
  expect_equal(centre_a$count_5_walk_p50, 1)

  centre_b <- got[code_insee == "35001" & TYPEQU == "B"]
  expect_equal(centre_b$n_addresses, 2L)
  expect_equal(centre_b$n_observed, 0L)
  expect_equal(centre_b$count_5_walk_mean, 0)
  expect_equal(centre_b$count_5_walk_p90, 0)
  expect_equal(centre_b$count_5_walk_max, 0)
})

test_that("address/type aggregation refuses duplicate sparse keys", {
  expect_error(
    aggregate_address_type_metrics(
      metrics = data.table(
        address_id = c("a1", "a1"), TYPEQU = c("A", "A"),
        count_5_walk = c(1L, 1L)
      ),
      address_spine = data.table(
        address_id = "a1", code_insee = "35001", nom_commune = "Centre"
      ),
      type_catalogue = "A",
      value_columns = "count_5_walk"
    ),
    "duplicate"
  )
})

test_that("address/type aggregation supports arbitrary territory keys", {
  spine <- data.table(
    address_id = c("a1", "a2"),
    epci = c("E1", "E1"),
    code_departement = c("35", "35")
  )
  metrics <- data.table(
    address_id = "a1", TYPEQU = "A", count_5_walk = 3L
  )

  got <- aggregate_address_type_metrics(
    metrics, spine, type_catalogue = "A",
    territory_columns = c("epci", "code_departement"),
    value_columns = "count_5_walk"
  )

  expect_equal(got[, .(epci, code_departement, TYPEQU, n_addresses,
                       count_5_walk_mean)],
               data.table(epci = "E1", code_departement = "35", TYPEQU = "A",
                          n_addresses = 2L, count_5_walk_mean = 1.5))
})

test_that("address/type aggregation zero-fills an entirely absent metric table", {
  got <- aggregate_address_type_metrics(
    metrics = data.table(
      address_id = character(), TYPEQU = character(), count_5_walk = integer()
    ),
    address_spine = data.table(
      address_id = c("a1", "a2"), code_insee = c("35001", "35001"),
      nom_commune = c("Centre", "Centre")
    ),
    type_catalogue = c("A", "B"),
    value_columns = "count_5_walk"
  )

  expect_equal(nrow(got), 2L)
  expect_true(all(got$n_addresses == 2L))
  expect_true(all(got$n_observed == 0L))
  expect_true(all(got$count_5_walk_mean == 0))
  expect_true(all(got$count_5_walk_share == 0))
})

test_that("implicit-zero quantiles match an explicitly expanded vector", {
  values <- c(4, 1, 0)
  probs <- c(0.1, 0.5, 0.9)
  explicit <- quantile(c(values, 0, 0), probs = probs, names = FALSE)

  expect_equal(
    address_aggregate_implicit_zero_quantile(values, 5L, probs),
    as.numeric(explicit)
  )
})

test_that("weighted implicit-zero quantiles match an expanded histogram", {
  values <- c(4, 1, 0)
  weights <- c(1, 2, 1)
  probs <- c(0.1, 0.5, 0.9)
  explicit <- rep(values, weights)
  explicit <- c(explicit, rep(0, 6L - length(explicit)))

  expect_equal(
    address_aggregate_implicit_zero_quantile_weighted(
      values, weights, 6L, probs
    ),
    as.numeric(quantile(explicit, probs = probs, names = FALSE))
  )
})

test_that("Arrow aggregation collects compact histograms and zero-fills", {
  dataset <- arrow::arrow_table(data.frame(
    address_id = c("a1", "a1", "a2"),
    TYPEQU = c("A", "B", "A"),
    count_5_walk = c(2L, 1L, 0L),
    transit_gain_count_5_p1 = c(NA_integer_, 1L, 0L)
  ))
  spine <- data.table(
    address_id = c("a1", "a2"), code_insee = c("35001", "35001"),
    nom_commune = c("Centre", "Centre")
  )

  got <- aggregate_address_type_metrics_arrow(
    dataset, spine, type_catalogue = c("A", "B"),
    value_columns = c("count_5_walk", "transit_gain_count_5_p1"),
    probs = c(0.5)
  )

  expect_equal(nrow(got), 2L)
  expect_equal(got[TYPEQU == "A", count_5_walk_mean], 1)
  expect_equal(got[TYPEQU == "B", count_5_walk_mean], 0.5)
  expect_equal(got[TYPEQU == "A", transit_gain_count_5_p1_mean], 0)
  expect_equal(got[TYPEQU == "B", transit_gain_count_5_p1_mean], 0.5)
  expect_true(all(got$n_addresses == 2L))
})

test_that("Arrow aggregation rolls one histogram up to all territory levels", {
  dataset <- arrow::arrow_table(data.frame(
    address_id = c("a1", "a1", "a2"),
    TYPEQU = c("A", "B", "A"),
    count_5_walk = c(2L, 1L, 0L)
  ))
  spine <- data.table(
    address_id = c("a1", "a2"),
    code_insee = c("35001", "35002"),
    code_departement = c("35", "35"),
    nom_commune = c("Centre", "Rural"), code_region = c("53", "53"),
    epci = c("E1", "E1")
  )

  got <- aggregate_address_type_metrics_arrow_levels(
    dataset, spine, type_catalogue = c("A", "B"),
    territory_levels = list(
      commune = c("code_insee", "nom_commune"),
      epci = "epci"
    ),
    value_columns = "count_5_walk", probs = c(0.5)
  )

  expect_equal(names(got), c("commune", "epci"))
  expect_equal(nrow(got$commune), 4L)
  expect_equal(nrow(got$epci), 2L)
  expect_equal(got$epci[TYPEQU == "A", count_5_walk_mean], 1)
  expect_equal(got$epci[TYPEQU == "B", count_5_walk_mean], 0.5)
  expect_true(all(got$epci$n_addresses == 2L))
})

test_that("commune aggregation keeps addresses with no EPCI assignment", {
  dataset <- arrow::arrow_table(data.frame(
    address_id = c("a1", "a2"),
    TYPEQU = c("A", "A"),
    count_5_walk = c(2L, 1L)
  ))
  spine <- data.table(
    address_id = c("a1", "a2"),
    code_insee = c("35001", "35002"),
    code_departement = c("35", "35"),
    nom_commune = c("Attached", "No EPCI"),
    code_region = c("53", "53"),
    epci_code = c("E1", NA_character_)
  )

  got <- aggregate_address_type_metrics_arrow_levels(
    dataset, spine, type_catalogue = "A",
    value_columns = "count_5_walk", probs = c(0.5)
  )

  expect_equal(nrow(got$commune), 2L)
  expect_equal(nrow(got$epci), 1L)
  expect_equal(got$commune[code_insee == "35002", n_addresses], 1L)
  expect_equal(got$epci$epci_code, "E1")
})
