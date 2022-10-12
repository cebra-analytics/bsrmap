context("points_to_layer")

test_that("converts points to layer within range", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  expect_error(points_to_layer(template_rast, data.frame(a = 1:5, b = 1:5)),
               "Point y data should have at least 3 columns.")
  expect_error(points_to_layer(template_rast,
                               data.frame(a = 1:5, b = 1:5, c = 1:5)),
               "Point y data coordinates should be 'lon' and 'lat'.")
  expect_error(points_to_layer(template_rast,
                               data.frame(lat = 1:5, lon = 1:5, c = 1:5)),
               "Point y data values should be in column specified by 'name'.")
  example_pts <- data.frame(lon = c(144.3, 144.3, 144.4, 144.5, 144.5),
                            lat = c(-38.0, -37.6, -37.8, -37.9, -37.8),
                            values = 1:5)
  example_vect <- terra::project(terra::vect(example_pts, crs = "EPSG:4326"),
                                 "EPSG:3577")
  template_rast <- terra::crop(template_rast,
                               terra::buffer(example_vect, width = 5000))
  template_rast[length(template_rast[]) - 20] <- NA
  cell_idx <- terra::cells(template_rast, example_vect)[,2]
  template_vect <- terra::as.points(template_rast, na.rm = FALSE)
  near_vect <- terra::nearest(terra::as.points(template_rast, na.rm = FALSE),
                              template_vect[cell_idx,])
  near_vect <- near_vect[which(terra::values(near_vect)[,5] <= 10000),]
  expect_rast <- template_rast*NA
  expect_rast[terra::values(near_vect)[,1]] <- terra::values(near_vect)[,4]
  expect_rast <- expect_rast*(template_rast + 1)
  expect_silent(result_rast <- points_to_layer(template_rast, example_pts,
                                               name = "values",
                                               max_distance = 10)) # km
  diff_idx <- which(result_rast[][,1] != expect_rast[][,1])
  diff_expect_dist <- terra::distance(
    template_vect[cell_idx[expect_rast[][diff_idx, 1]]],
    template_vect[diff_idx], pairwise = TRUE)
  diff_result_dist <- terra::distance(
    template_vect[cell_idx[result_rast[][diff_idx, 1]]],
    template_vect[diff_idx], pairwise = TRUE)
  expect_equal(diff_result_dist, diff_expect_dist)
})
