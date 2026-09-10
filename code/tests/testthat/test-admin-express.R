library(testthat)
library(data.table)

source_project_r("read-admin-express.R")

test_that("ADMIN EXPRESS commune attributes become a stable COG crosswalk", {
  got <- admin_express_commune_crosswalk(
    data.table(
      code_insee = c("35001", "22001"),
      nom_officiel = c("Centre", "Armor"),
      code_insee_du_departement = c("35", "22"),
      code_insee_de_la_region = c("53", "53"),
      codes_siren_des_epci = c("200000001", "200000002")
    ),
    departements = "35"
  )

  expect_equal(
    got,
    data.table(
      code_insee = "35001", nom_commune = "Centre",
      code_departement = "35", code_region = "53", epci = "200000001"
    )
  )
})

test_that("duplicate COG commune codes fail loudly", {
  communes <- data.table(
    code_insee = c("35001", "35001"),
    nom_officiel = c("Centre", "Centre bis"),
    code_insee_du_departement = c("35", "35"),
    code_insee_de_la_region = c("53", "53"),
    codes_siren_des_epci = c("200000001", "200000001")
  )

  expect_error(
    admin_express_commune_crosswalk(communes),
    "duplicate code_insee"
  )
})
