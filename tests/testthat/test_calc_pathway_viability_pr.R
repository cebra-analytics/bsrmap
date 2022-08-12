context("calc_pathway_viability_pr")

test_that("distributes viability probability across raster layer", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  distr_layer <- popn_rast/as.numeric(terra::global(popn_rast, fun = "sum",
                                                    na.rm = TRUE))
  event_pr <- data.frame(N_incursions = c(0, 1, 2, 4, 8),
                         probability = c(0.5, 0.25, 0.15, 0.07, 0.03))
  expected_values <- 1 - rowSums(sapply(1:nrow(event_pr), function(i) {
    event_pr$probability[i]*(1 - distr_layer[][,1])^event_pr$N_incursions[i]
  }))
  expect_silent(new_layer <- calc_pathway_viability_pr(distr_layer, event_pr))
  idx <- which(is.finite(distr_layer[][,1]))
  expect_true(all(round(new_layer[][idx, 1], 8) ==
                    round(expected_values[idx], 8)))
})
