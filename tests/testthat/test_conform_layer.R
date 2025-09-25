context("conform_layer")

test_that("conforms CRS", {
  TEST_DIRECTORY <- test_path("test_inputs")
  albers_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  lonlat_rast <- terra::rast(file.path(TEST_DIRECTORY,
                                       "greater_melb_wgs84.tif"))
  expect_silent(new_layer <- suppressMessages(
    conform_layer(lonlat_rast, albers_rast*0, use_method = "near")))
  expect_true(terra::crs(new_layer) == terra::crs(albers_rast))
  expect_silent(new_layer <- suppressMessages(
    conform_layer(albers_rast, lonlat_rast*0, use_method = "near")))
  expect_true(terra::crs(new_layer) == terra::crs(lonlat_rast))
  expect_equal(round(new_layer[][,1], 3), round(lonlat_rast[][,1], 3))
})

test_that("conforms extent and resolution", {
  TEST_DIRECTORY <- test_path("test_inputs")
  aclum_rast <- terra::rast(file.path(TEST_DIRECTORY, "aclum.tif"))
  suit_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(suit_rast, aclum_rast*0)))
  expect_true(terra::ext(new_layer) == terra::ext(aclum_rast))
  expect_true(all(terra::res(new_layer) == terra::res(aclum_rast)))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(aclum_rast, suit_rast*0)))
  expect_true(terra::ext(new_layer) == terra::ext(suit_rast))
  expect_true(all(terra::res(new_layer) == terra::res(suit_rast)))
  global_sdm <- terra::rast(file.path(TEST_DIRECTORY, "global_sdm.tif"))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(global_sdm, suit_rast*0, na_strategy = "retain")))
  idx <- which(is.na(new_layer[]) & !is.na(suit_rast[]))
  adj_idx <- terra::adjacent(new_layer, idx,
                             t(array(c(0,1,rep(0,7)),c(3,3))))[,1]
  adj_values <- new_layer[adj_idx][,1]
  expect_true(terra::ext(new_layer) == terra::ext(suit_rast))
  expect_true(all(terra::res(new_layer) == terra::res(suit_rast)))
  expect_true(all(which(is.finite(new_layer[])) %in%
                    which(is.finite(suit_rast[]))))
  expect_false(all(which(is.finite(suit_rast[])) %in%
                     which(is.finite(new_layer[]))))
  expect_true(all(is.na(new_layer[idx][,1])))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(global_sdm, suit_rast*0)))
  expect_true(all(which(is.finite(new_layer[])) %in%
                    which(is.finite(suit_rast[]))))
  expect_true(all(which(is.finite(suit_rast[])) %in%
                    which(is.finite(new_layer[]))))
  expect_true(all(new_layer[idx][,1] == 0))
  new_layer <- expect_silent(suppressMessages(
    conform_layer(global_sdm, suit_rast*0, na_strategy = "nearest")))
  expect_true(all(which(is.finite(new_layer[])) %in%
                    which(is.finite(suit_rast[]))))
  expect_true(all(which(is.finite(suit_rast[])) %in%
                    which(is.finite(new_layer[]))))
  expect_equal(new_layer[idx][,1], adj_values)
})

test_that("conforms to NA template", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template_rast <- terra::rast(file.path(TEST_DIRECTORY,
                                         "greater_melb.tif"))*0 + 1
  cbd_pt <- terra::vect(terra::xyFromCell(template_rast, 5922),
                        crs = terra::crs(template_rast))
  buffer_rast <- terra::rasterize(terra::buffer(cbd_pt, 20000), template_rast)
  new_layer <- suppressMessages(conform_layer(buffer_rast, template_rast))
  expect_true(all(which(is.finite(new_layer[])) %in%
                    which(is.finite(template_rast[]))))
  expect_true(all(which(is.finite(template_rast[])) %in%
                    which(is.finite(new_layer[]))))
  expect_true(all(new_layer[which(is.finite(buffer_rast[]) &
                                    is.finite(template_rast[]))][,1] == 1))
  expect_true(all(new_layer[which(is.na(buffer_rast[]) &
                                    is.finite(template_rast[]))][,1] == 0))
  expect_true(all(is.na(new_layer[which(is.na(template_rast[]))][,1])))
})

test_that("normalizes and binarizes raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  suit_rast <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  new_layer <- expect_silent(suppressMessages(
    conform_layer((suit_rast - 0.1)*suit_rast, suit_rast*0, normalize = TRUE)))
  expect_equal(min(new_layer[][,1], na.rm = TRUE), 0)
  expect_equal(max(new_layer[][,1], na.rm = TRUE), 1)
  new_layer <- expect_silent(suppressMessages(
    conform_layer(suit_rast, suit_rast*0, binarize = TRUE)))
  expect_true(all(terra::unique(new_layer)[,1] %in% c(0, 1)))
  expect_true(all(which(new_layer[][,1] == 0) == which(suit_rast[][,1] == 0)))
  expect_true(all(which(new_layer[][,1] == 1) == which(suit_rast[][,1] > 0)))
})

test_that("conforms aggregate via union: 1 - prod(1 - x)", {
  TEST_DIRECTORY <- test_path("test_inputs")
  region_aggr <- terra::rast(file.path(TEST_DIRECTORY, "region_aggr.tif"))
  occur_pr <- terra::rast(file.path(TEST_DIRECTORY, "occur_pr.tif"))
  expect_silent(new_layer <- suppressMessages(
    conform_layer(occur_pr, region_aggr, use_aggr_fun = "union")))
  region_1km <- terra::disagg(region_aggr, 10)
  occur_pr_cut <- terra::crop(occur_pr, region_1km)*region_1km
  expect_equal(round(1 - prod(1 - occur_pr_cut[][,1], na.rm = T), 4),
               round(1 - prod(1 - new_layer[][,1], na.rm = T), 4))
})
