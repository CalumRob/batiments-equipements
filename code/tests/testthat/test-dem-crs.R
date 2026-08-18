library(testthat)

source(testthat::test_path("../../R/full-run-inputs.R"), local = TRUE)

test_that("the preserved full-run DEM accepts semantic WGS84 CRS", {
  dem <- testthat::test_path(
    "../../../data/acquired/osm/network_bretagne_plus_strip_merged.osm.pbf",
    "srtm_bretagne.tif"
  )
  skip_if_not(file.exists(dem))

  expect_silent(validate_dem_raster(dem))
})

test_that("DEM validation still rejects a different or absent CRS", {
  skip_if_not_installed("terra")
  path <- tempfile(fileext = ".tif")
  on.exit(unlink(path), add = TRUE)
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 0.00108,
                   ymin = 0, ymax = 0.00108, vals = 1:16)

  terra::crs(r) <- sf::st_crs(3857)[["wkt"]]
  terra::writeRaster(r, path, overwrite = TRUE)
  expect_error(validate_dem_raster(path, bbox = c(xmin = 0, ymin = 0,
                                                  xmax = 0.00108, ymax = 0.00108)),
               "DEM CRS must be EPSG:4326")

})
