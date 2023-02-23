context("transform_layer")

test_that("various transformations", {
  TEST_DIRECTORY <- test_path("test_inputs")
  suit_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  idx <- which(!is.na(suit_rast[]))
  suit_values <- suit_rast[idx][,1]
  expect_error(transform_layer(suit_rast, a = "1"),
               "Parameter 'a' should be a single value numeric.")
  expect_error(transform_layer(suit_rast, b = 1:2),
               "Parameter 'b' should be a single value numeric.")
  expect_error(transform_layer(suit_rast, b = NA),
               "Parameter 'b' should be a single value numeric.")
  expect_error(transform_layer(suit_rast, type = "lower", b = "1"),
               "Parameter 'b' should be a single value numeric or NA.")
  expect_silent(trans_rast <- transform_layer(suit_rast))
  expect_is(trans_rast, "SpatRaster")
  expect_equal(trans_rast[idx][,1], suit_values)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "linear",
                                              a = 2, b = 3))
  expect_equal(trans_rast[idx][,1], 2*suit_values + 3)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "exponential"))
  expect_equal(trans_rast[idx][,1], suit_values*0 + 1)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "exponential",
                                              a = 2))
  expect_equal(trans_rast[idx][,1], 2^suit_values)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "exponential",
                                              b = 3))
  expect_equal(trans_rast[idx][,1], suit_values^3)
  expect_silent(trans_rast <- transform_layer(suit_rast + 1,
                                              type = "logarithmic"))
  expect_equal(trans_rast[idx][,1], log(suit_values + 1))
  expect_silent(trans_rast <- transform_layer(suit_rast + 1,
                                              type = "logarithmic", b = 10))
  expect_equal(trans_rast[idx][,1], log10(suit_values + 1))
  expect_silent(trans_rast <- transform_layer(suit_rast - 0.1, type = "lower"))
  expect_equal(trans_rast[idx][,1], (suit_values >= 0.1)*(suit_values - 0.1))
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "lower",
                                              a = 0.2, b = 0.1))
  expect_equal(trans_rast[idx][,1],
               (suit_values < 0.2)*0.1 + (suit_values >= 0.2)*suit_values)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "lower",
                                              a = 0.2, b = NA))
  expect_equal(trans_rast[idx][,1], ((suit_values >= 0.2) | NA)*suit_values)
  expect_silent(trans_rast <- transform_layer(suit_rast*2, type = "upper"))
  expect_equal(trans_rast[idx][,1],
               (suit_values > 0.5)*1 + (suit_values <= 0.5)*suit_values*2)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "upper",
                                              a = 0.5, b = 0.5))
  expect_equal(trans_rast[idx][,1],
               (suit_values > 0.5)*0.5 + (suit_values <= 0.5)*suit_values)
  expect_silent(trans_rast <- transform_layer(suit_rast, type = "upper",
                                              a = 0.5, b = NA))
  expect_equal(trans_rast[idx][,1], ((suit_values <= 0.5) | NA)*suit_values)
})
