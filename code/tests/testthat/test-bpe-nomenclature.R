library(testthat)
library(data.table)

source_project_r("read-bpe.R")

test_that("BPE type nomenclature exposes official labels", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  fwrite(
    data.table(
      TYPEQU = c("A104", "B202"),
      LIB = c("GENDARMERIE", "ÉPICERIE")
    ),
    path
  )

  got <- read_bpe_type_nomenclature(path = path)

  expect_equal(
    got,
    data.table(
      TYPEQU = c("A104", "B202"),
      nom_typequ = c("GENDARMERIE", "ÉPICERIE")
    )
  )
})
