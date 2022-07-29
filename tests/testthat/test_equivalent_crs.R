context("equivalent_crs")

test_that("checks CRS match", {
  TEST_DIRECTORY <- test_path("test_inputs")
  albers_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  lonlat_rast <- terra::rast(file.path(TEST_DIRECTORY,
                                       "greater_melb_wgs84.tif"))
  expect_false(equivalent_crs(albers_rast, lonlat_rast))
  new_rast <- terra::rast(matrix(), crs = "EPSG:3577")
  expect_true(equivalent_crs(new_rast, albers_rast))
  new_rast <- terra::rast(matrix(), crs = "EPSG:4326")
  expect_true(equivalent_crs(new_rast, lonlat_rast))
})
