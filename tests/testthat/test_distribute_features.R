context("distribute_features")

test_that("distributes features across a template raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  expect_error(distribute_features(template_rast, 10),
               paste("Feature y data should be a terra::SpatVector or",
                     "sf::sf object."))
  containers <- terra::vect(file.path(TEST_DIRECTORY, "containers.shp"))
  template_rast <- terra::crop(template_rast,
                               terra::buffer(containers, width = 1000))
  expected_rast <- template_rast*0
  for (i in 1:nrow(containers)) {
    cells_df <- as.data.frame(terra::cells(template_rast, containers[i,],
                                           exact = TRUE, touches = TRUE))
    cells_df$weights <- cells_df$weights/sum(cells_df$weights)
    expected_rast[cells_df$cell] <-
      (expected_rast[cells_df$cell][,1] +
         terra::values(containers[i,])[,1]*cells_df$weights)
  }
  expect_silent(distr_rast <- distribute_features(template_rast, containers,
                                                  vars = "Melbourne"))
  expect_equal(sum(distr_rast[][,1], na.rm = TRUE),
               sum(terra::values(containers)))
  expect_equal(round(distr_rast[][,1], 2), round(expected_rast[][,1], 2))
})

test_that("distributes features across a mask raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))*0
  containers <- terra::vect(file.path(TEST_DIRECTORY, "containers.shp"))
  mask_rast <- terra::crop(template_rast,
                           terra::buffer(containers, width = 1000))
  set.seed(1234)
  mask_rast[sample(which(is.finite(mask_rast[][,1])), 150)] <- 1
  expected_rast <- mask_rast*0
  for (i in 1:nrow(containers)) {
    cells_df <- as.data.frame(terra::extract(
      mask_rast, containers[i,], cells = TRUE, exact = TRUE, touches = TRUE))
    cells_df <- cells_df[which(cells_df$prod > 0),]
    cells_df$fraction <- cells_df$fraction/sum(cells_df$fraction)
    expected_rast[cells_df$cell] <- (expected_rast[cells_df$cell][,1] +
                                       terra::values(containers[i,])[,1]*cells_df$fraction)
  }
  cell_df <- unique(terra::cells(mask_rast, containers, touches = TRUE))
  avail <- unique(cell_df[which(mask_rast[cell_df[,2]][,1] > 0), 1])
  expect_silent(distr_rast <- distribute_features(mask_rast, containers,
                                                  vars = "Melbourne"))
  expect_equal(sum(distr_rast[][,1], na.rm = TRUE),
               sum(terra::values(containers)[avail,1]))
  expect_equal(round(distr_rast[][,1], 2), round(expected_rast[][,1], 2))
})
