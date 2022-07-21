context("conform_layer")

test_that("conforms CRS", {
  TEST_DIRECTORY <- test_path("test_inputs")
  albers_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  lonlat_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb_wgs84.tif"))
  expect_silent(new_layer <- suppressMessages(
    conform_layer(lonlat_rast, albers_rast*0)))
  expect_true(terra::crs(new_layer) == terra::crs(albers_rast))
  expect_silent(new_layer <- suppressMessages(
    conform_layer(albers_rast, lonlat_rast*0)))
  expect_true(terra::crs(new_layer) == terra::crs(lonlat_rast))
  expect_equal(new_layer[][,1], lonlat_rast[][,1])
})

test_that("conforms extent and resolution", {
  TEST_DIRECTORY <- test_path("test_inputs")
  aclum_rast <- terra::rast(file.path(TEST_DIRECTORY, "aclum.tif"))
  suit_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(suit_rast, aclum_rast*0)))
  expect_true(terra::ext(new_layer) == terra::ext(aclum_rast))
  expect_true(all(terra::res(new_layer) == terra::res(aclum_rast)))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(aclum_rast, suit_rast*0)))
  expect_true(terra::ext(new_layer) == terra::ext(suit_rast))
  expect_true(all(terra::res(new_layer) == terra::res(suit_rast)))
})

test_that("normalizes and binarizes raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  suit_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(suit_rast, suit_rast*0, normalize = TRUE)))
  expect_equal(min(new_layer[][,1], na.rm = TRUE), 0)
  expect_equal(max(new_layer[][,1], na.rm = TRUE), 1)
  new_layer <- expect_silent(suppressMessages(
    conform_layer(suit_rast, suit_rast*0, binarize = TRUE)))
  expect_true(all(terra::unique(new_layer)[,1] %in% c(0, 1)))
  expect_true(all(which(new_layer[][,1] == 0) == which(suit_rast[][,1] == 0)))
  expect_true(all(which(new_layer[][,1] == 1) == which(suit_rast[][,1] > 0)))
})
