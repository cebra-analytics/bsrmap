context("combine_layers")

test_that("combines layers via product, sum, and union", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  stack_rast <- terra::rast(list(popn_rast, veg_rast))
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "prod")))
  expect_equal(terra::nlyr(comb_rast), 1)
  expect_equal(comb_rast[][,1], popn_rast[][,1]*veg_rast[][,1])
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "sum")))
  expect_equal(comb_rast[][,1], popn_rast[][,1] + veg_rast[][,1])
  stack_rast[[1]] <- stack_rast[[1]]/22000
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "union")))
  expect_equal(comb_rast[][,1],
               1 - (1 - stack_rast[[1]][][,1])*(1 - veg_rast[][,1]))
})

test_that("combines layers via product and sum with weights", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  stack_rast <- terra::rast(list(popn_rast, veg_rast))
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "prod", weights = c(2, 3))))
  expect_equal(terra::nlyr(comb_rast), 1)
  expect_equal(comb_rast[][,1], 2*3*popn_rast[][,1]*veg_rast[][,1])
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "sum", weights = c(2, 3))))
  expect_equal(comb_rast[][,1], 2*popn_rast[][,1] + 3*veg_rast[][,1])
})

test_that("combines layers with binarize and NA removal", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  stack_rast <- terra::rast(list(popn_rast, veg_rast))
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "prod", binarize = TRUE)))
  expect_equal(comb_rast[][,1], +(popn_rast[][,1]*veg_rast[][,1] > 0))
  stack_rast[[1]][] <- NA
  expect_silent(comb_rast <- suppressMessages(
    combine_layers(stack_rast, use_fun = "sum", na.rm = TRUE)))
  expect_equal(comb_rast[][,1], veg_rast[][,1])
})
