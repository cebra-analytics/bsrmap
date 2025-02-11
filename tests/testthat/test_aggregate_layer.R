context("aggregate_layer")

test_that("aggregates and re-samples layer to match template", {
  TEST_DIRECTORY <- test_path("test_inputs")
  aclum_rast <- terra::rast(file.path(TEST_DIRECTORY, "aclum.tif"))
  cropped_ext <- terra::ext(aclum_rast) - c(0, 5000, 0, 5000)
  template_rast <- terra::crop(terra::rast(file.path(TEST_DIRECTORY,
                                                     "greater_melb.tif"))*0,
                               cropped_ext)
  expect_silent(aggr_rast <- suppressMessages(
    aggregate_layer(aclum_rast, template_rast, use_fun = "max")))
  expect_true(terra::ext(aggr_rast) == terra::ext(template_rast))
  expect_true(terra::crs(aggr_rast, proj = TRUE) ==
                terra::crs(template_rast, proj = TRUE))
  expect_true(all(terra::res(aggr_rast) == terra::res(template_rast)))
  value_matrix <- matrix(terra::crop(aclum_rast, cropped_ext)[],
                         nrow = 100, byrow = TRUE)
  expected_values <- array(sapply(as.data.frame(
    t(expand.grid(0:4, 0:4))), function(a) {
      max(value_matrix[(1:20) + a[1]*20, (1:20) + a[2]*20])
    }), c(5, 5))
  expect_true(all(aggr_rast[][,1] == t(expected_values)))
})

test_that("aggregates via union: 1 - prod(1 - x)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  region_aggr <- terra::rast(file.path(TEST_DIRECTORY, "region_aggr.tif"))
  occur_pr <- terra::rast(file.path(TEST_DIRECTORY, "occur_pr.tif"))
  expect_silent(aggr_rast <- suppressMessages(
    aggregate_layer(occur_pr, region_aggr, use_fun = "union")))
  expect_equal(round(1 - prod(1 - occur_pr[][,1], na.rm = T), 3),
               round(1 - prod(1 - aggr_rast[][,1], na.rm = T), 3))
})
