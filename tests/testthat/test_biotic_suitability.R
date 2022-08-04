context("biotic_suitability")

test_that("combines layers via product by default", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  stack_rast <- terra::rast(list(popn_rast, veg_rast))
  expect_silent(comb_rast <- suppressMessages(
    biotic_suitability(stack_rast)))
  expect_equal(comb_rast[][,1], popn_rast[][,1]*veg_rast[][,1])
})
