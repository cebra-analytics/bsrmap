context("establishment_likelihood")

test_that("combines layers via product by default", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  expect_silent(comb_rast <- suppressMessages(
    establishment_likelihood(popn_rast, veg_rast)))
  expect_equal(comb_rast[][,1], popn_rast[][,1]*veg_rast[][,1])
})
