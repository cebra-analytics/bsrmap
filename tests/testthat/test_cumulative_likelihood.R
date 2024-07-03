context("cumulative_likelihood")

test_that("calculates ordered cumulative sum", {
  TEST_DIRECTORY <- test_path("test_inputs")
  x <- terra::rast(file.path(TEST_DIRECTORY, "likelihood.tif"))
  x_sum <- as.numeric(terra::global(x, sum, na.rm = TRUE))
  idx <- which(x[] > quantile(x[][,1], probs = (1:10)/10, na.rm = TRUE)[9])
  # Ascending
  expected <- data.frame(cell = idx, value = x[idx][,1])
  expected <- expected[order(expected$cell, decreasing = TRUE),]
  expected <- expected[order(expected$value, decreasing = TRUE),]
  expected$cumul <- x_sum
  for (i in 2:nrow(expected)) {
    expected$cumul[i] <- expected$cumul[i - 1] - expected$value[i - 1]
  }
  expect_silent(x_cumul <- cumulative_likelihood(x))
  expect_true(all(round(x_cumul[expected$cell][,1],10) ==
                    round(expected$cumul, 10)))
  # Descending
  expected2 <- data.frame(cell = idx, value = x[idx][,1])
  expected2 <- expected2[order(expected2$value, decreasing = TRUE),]
  expect_silent(x_cumul <- cumulative_likelihood(x, order = "descending"))
  expect_true(all(round(x_cumul[expected2$cell][,1],10) ==
                    round(cumsum(expected2$value), 10)))
})

test_that("calculates percentage of ordered cumulative sum total", {
  TEST_DIRECTORY <- test_path("test_inputs")
  x <- terra::rast(file.path(TEST_DIRECTORY, "likelihood.tif"))
  x_sum <- as.numeric(terra::global(x, sum, na.rm = TRUE))
  idx <- which(x[] > quantile(x[][,1], probs = (1:10)/10, na.rm = TRUE)[9])
  # Ascending
  expected <- data.frame(cell = idx, value = x[idx][,1])
  expected <- expected[order(expected$cell, decreasing = TRUE),]
  expected <- expected[order(expected$value, decreasing = TRUE),]
  expected$cumul <- x_sum
  for (i in 2:nrow(expected)) {
    expected$cumul[i] <- expected$cumul[i - 1] - expected$value[i - 1]
  }
  expected$cat <- sapply(expected$cumul, function(l) sum(l > x_sum*(1:9)/10))
  expected_cats <- data.frame(ID = 0:9, cum_lhood = paste0((0:9)*10, "-",
                                                           (1:10)*10, "%"))
  expect_silent(x_cumul <- cumulative_likelihood(x, output = "percentage"))
  expect_equal((x_cumul*1)[expected$cell][,1], expected$cat)
  expect_equal(terra::cats(x_cumul)[[1]], expected_cats)
  # Descending
  expected2 <- data.frame(cell = idx, value = x[idx][,1])
  expected2 <- expected2[order(expected2$value, decreasing = TRUE),]
  expected2$cat <- sapply(cumsum(expected2$value),
                          function(l) sum(l > x_sum*(1:9)/10))
  expect_silent(x_cumul <- cumulative_likelihood(x, output = "percentage",
                                                 order = "descending"))
  expect_equal((x_cumul*1)[expected2$cell][,1], expected2$cat)
  expect_equal(terra::cats(x_cumul)[[1]], expected_cats)
})
