context("buffered_hull_layer")

test_that("builds alpha or convex hull layer via set of points", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  expect_error(buffered_hull_layer(template_rast, data.frame(a = 1:4)),
               "Point (features) y data should have at least 2 columns.",
               fixed = TRUE)
  expect_error(buffered_hull_layer(template_rast,
                                   data.frame(a = 1:4, b = 3:6)),
               paste("Point (features) y data coordinates should be 'lon'",
                     "and 'lat'."), fixed = TRUE)
  example_pts <- data.frame(lon = c(144.3, 144.3, 144.4, 144.5, 144.5),
                            lat = c(-38.0, -37.6, -37.8, -37.9, -37.8))
  expect_error(buffered_hull_layer(template_rast, example_pts),
               "Fitting an alpha hull requires a numeric 'alpha' parameter.")
  expect_silent(new_layer1 <- suppressMessages(
    buffered_hull_layer(template_rast, example_pts, alpha = 100000)))
  expect_silent(new_layer2 <- suppressMessages(
    buffered_hull_layer(template_rast, example_pts, hull = "convex")))
  expect_silent(new_layer3 <- suppressMessages(
    buffered_hull_layer(template_rast, example_pts, hull = "convex",
                        buffer = 10000)))
  example_pts[6,] <- c(144.5, -38.1) # add point in sea
  expect_silent(new_layer4 <- suppressMessages(
    buffered_hull_layer(template_rast, example_pts, alpha = 100000)))
  expect_silent(new_layer5 <- suppressMessages(
    buffered_hull_layer(template_rast, example_pts, alpha = 100000,
                        mask = TRUE)))
  idx_1 <- which(new_layer1[][,1] > 0)
  idx_2 <- which(new_layer2[][,1] > 0)
  idx_3 <- which(new_layer3[][,1] > 0)
  idx_4 <- which(new_layer4[][,1] > 0)
  idx_5 <- which(new_layer5[][,1] > 0)
  expect_true(all(idx_1 %in% idx_3))
  expect_true(all(idx_2 %in% idx_3))
  expect_true(all(idx_1 %in% idx_4))
  expect_true(all(idx_1 %in% idx_5))
  expect_true(length(idx_1) < length(idx_2))
  expect_true(length(idx_2) < length(idx_3))
  expect_true(length(idx_1) < length(idx_4))
  expect_true(length(idx_1) == length(idx_5))
})
