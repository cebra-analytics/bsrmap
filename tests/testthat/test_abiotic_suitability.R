context("abiotic_suitability")

test_that("conforms layer to template", {
  TEST_DIRECTORY <- test_path("test_inputs")
  abiotic_rast <- terra::rast(file.path(TEST_DIRECTORY, "abiotic.tif"))
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  expect_silent(new_layer <- suppressMessages(
    abiotic_suitability(abiotic_rast, template_rast)))
  expect_true(terra::crs(new_layer) == terra::crs(template_rast))
  expect_true(terra::ext(new_layer) == terra::ext(template_rast))
  expect_true(all(terra::res(new_layer) == terra::res(template_rast)))
  expect_true(round(mean(new_layer[], na.rm = TRUE), 2) ==
                round(mean(terra::crop(abiotic_rast,
                                       c(144.5, 145.5, -38.5, -37.5))[],
                           na.rm = TRUE), 2))
})
