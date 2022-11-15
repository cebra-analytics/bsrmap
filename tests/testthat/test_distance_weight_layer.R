context("distance_weight_layer")

test_that("calculates distance weight layer", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  expect_error(distance_weight_layer(template_rast, 1:5),
               "Point (features) y data should have at least 2 columns.",
               fixed = TRUE)
  expect_error(distance_weight_layer(template_rast,
                                     data.frame(a = 1:5, b = 1:5)),
               paste("Point (features) y data coordinates should be 'lon'",
                     "and 'lat'."), fixed = TRUE)
  example_pts <- data.frame(lon = c(144.3, 144.3, 144.4, 144.5, 144.5),
                            lat = c(-38.0, -37.6, -37.8, -37.9, -37.8))
  dist_rast <- (terra::distance(template_rast, terra::project(
    terra::vect(example_pts, crs = "EPSG:4326"), "EPSG:3577"))*
      (template_rast + 1))
  expect_rast <- exp(dist_rast/(1000/-0.03))
  expect_silent(dist_w_rast <- distance_weight_layer(
    template_rast, example_pts, beta = -0.03))
  expect_equal(dist_w_rast[][,1], expect_rast[][,1])
  expect_silent(dist_w_rast <- distance_weight_layer(
    dist_rast, beta = -0.03))
  expect_equal(dist_w_rast[][,1], expect_rast[][,1])
})

test_that("calculates distance weight layer with weights", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  example_pts <- data.frame(lon = c(144.3, 144.3),
                            lat = c(-38.0, -37.6),
                            weights = c(1, 2))
  expect_error(distance_weight_layer(template_rast, example_pts,
                                     weights = 1:3),
               "Weights should have the same number of rows as y.")
  expect_error(distance_weight_layer(template_rast, example_pts,
                                     weights = "invalid"),
               "Weights column invalid is not present in point data y.")
  example_vect <- terra::project(terra::vect(example_pts,
                                             crs = "EPSG:4326"), "EPSG:3577")
  dist_rast_1 <- (terra::distance(template_rast, example_vect[1,])*
                    (template_rast + 1))
  dist_rast_1 <- exp(dist_rast_1/(1000/-0.03))
  dist_rast_1 <- dist_rast_1/sum(dist_rast_1[], na.rm = TRUE)*1
  dist_rast_2 <- (terra::distance(template_rast, example_vect[2,])*
                    (template_rast + 1))
  dist_rast_2 <- exp(dist_rast_2/(1000/-0.03))
  dist_rast_2 <- dist_rast_2/sum(dist_rast_2[], na.rm = TRUE)*2
  expect_rast <- dist_rast_1 + dist_rast_2
  expect_silent(dist_w_rast <- distance_weight_layer(
    template_rast, example_pts, beta = -0.03, weights = "weights"))
  expect_equal(dist_w_rast[][,1], expect_rast[][,1])
})
