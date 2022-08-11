context("aggregate_categories")

test_that("aggregates category layer via selected categories", {
  TEST_DIRECTORY <- test_path("test_inputs")
  aclum_rast <- terra::rast(file.path(TEST_DIRECTORY, "aclum.tif"))
  cropped_ext <- terra::ext(aclum_rast) - c(0, 5000, 0, 5000)
  template_rast <- terra::crop(terra::rast(file.path(TEST_DIRECTORY,
                                                     "greater_melb.tif"))*0,
                               cropped_ext)
  expect_silent(aggr_rast <- suppressMessages(
    aggregate_categories(aclum_rast, template_rast,
                         selected = c(220, 330, 560, 620))))
  expect_true(terra::ext(aggr_rast) == terra::ext(template_rast))
  expect_true(terra::crs(aggr_rast) == terra::crs(template_rast))
  expect_true(all(terra::res(aggr_rast) == terra::res(template_rast)))
  value_matrix <- matrix(terra::crop(aclum_rast, cropped_ext)[],
                         nrow = 100, byrow = TRUE)
  expected_values <- array(sapply(
    as.data.frame(t(expand.grid(0:4, 0:4))), function(a) {
      max(value_matrix[(1:20) + a[1]*20, (1:20) + a[2]*20]
          %in% c(220, 330, 560, 620))
    }), c(5, 5))
  expect_true(all(aggr_rast[][,1] == t(expected_values)))
  expect_silent(aggr_rast <- suppressMessages(
    aggregate_categories(aclum_rast, template_rast,
                         selected = c(220, 330, 560, 620),
                         binarize = FALSE)))
  expected_values <- array(sapply(
    as.data.frame(t(expand.grid(0:4, 0:4))), function(a) {
      mean(value_matrix[(1:20) + a[1]*20, (1:20) + a[2]*20] %in%
             c(220, 330, 560, 620))
    }), c(5, 5))
  expect_true(all(round(aggr_rast[][,1], 6) == round(t(expected_values), 6)))
})
