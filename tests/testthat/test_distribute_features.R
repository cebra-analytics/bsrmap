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
    idx <- unique(terra::cells(template_rast, containers[i,])[,2])
    expected_rast[idx] <- (expected_rast[idx][,1] +
                             terra::values(containers[i,])[,1]/length(idx))
  }
  expect_silent(distr_rast <- distribute_features(template_rast, containers,
                                                  vars = "Melbourne"))
  expect_equal(sum(distr_rast[][,1], na.rm = TRUE),
               sum(terra::values(containers)))
  expect_equal(distr_rast[][,1], expected_rast[][,1])
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
    idx <- unique(terra::cells(mask_rast, containers[i,])[,2])
    idx <- idx[which(mask_rast[idx] > 0)]
    expected_rast[idx] <- (expected_rast[idx][,1] +
                             terra::values(containers[i,])[,1]/length(idx))
  }
  cell_df <- unique(terra::cells(mask_rast, containers))
  avail <- unique(cell_df[which(mask_rast[cell_df[,2]][,1] > 0), 1])
  expect_silent(distr_rast <- distribute_features(mask_rast, containers,
                                                  vars = "Melbourne"))
  expect_equal(sum(distr_rast[][,1], na.rm = TRUE),
               sum(terra::values(containers)[avail,1]))
  expect_equal(distr_rast[][,1], expected_rast[][,1])
})
