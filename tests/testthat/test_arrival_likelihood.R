context("arrival_likelihood")

test_that("combines layers via union ignoring NAs by default", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))/22000
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  na_rast <- terra::rast(popn_rast)
  stack_rast <- terra::rast(list(popn_rast, veg_rast, na_rast))
  expect_silent(comb_rast <- suppressMessages(arrival_likelihood(stack_rast)))
  expect_equal(comb_rast[][,1], 1 - (1 - popn_rast[][,1])*(1 - veg_rast[][,1]))
})
