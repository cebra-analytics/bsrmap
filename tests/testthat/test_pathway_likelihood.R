context("pathway_likelihood")

test_that("estimates pathway pest establishment viability likelihood", {
  TEST_DIRECTORY <- test_path("test_inputs")
  popn_rast <- terra::rast(file.path(TEST_DIRECTORY, "population.tif"))
  veg_rast <- terra::rast(file.path(TEST_DIRECTORY, "vegetation.tif"))
  pathway_layers <- terra::rast(list(popn_rast, veg_rast))
  expect_error(pathway_likelihood(pathway_layers, leakage_rate_ci = 10,
                                  viability_rate_ci = 1.0),
               "Leakage rate CI should have numeric form: (lower, upper).",
               fixed = TRUE)
  expect_error(pathway_likelihood(pathway_layers, leakage_rate_ci = c(10, 1),
                                  viability_rate_ci = 1.0),
               "Leakage rate CI should have numeric form: (lower, upper).",
               fixed = TRUE)
  expect_error(pathway_likelihood(pathway_layers, leakage_rate_ci = c(1, 10),
                                  viability_rate_ci = 1.0),
               paste("Establishment rate CI should have numeric form:",
                     "(lower, upper)."), fixed = TRUE)
  expect_error(pathway_likelihood(pathway_layers, leakage_rate_ci = c(1, 10),
                                  viability_rate_ci = c(1, 0)),
               paste("Establishment rate CI should have numeric form:",
                     "(lower, upper)."), fixed = TRUE)
  expect_error(pathway_likelihood(pathway_layers, leakage_rate_ci = c(1, 10),
                                  viability_rate_ci = c(-0.5, 1)),
               paste("Establishment rate CI probability values should be",
                     ">= 0 and <= 1."), fixed = TRUE)
  set.seed(1234)
  event_pr <- calc_viable_event_pr(n_events = c(1, 10), p_viable = c(0, 1))
  distr_layer <- (popn_rast*veg_rast/
                    as.numeric(terra::global(popn_rast*veg_rast, fun = "sum",
                                             na.rm = TRUE)))
  expected_values <- calc_pathway_viability_pr(distr_layer, event_pr)[][,1]
  set.seed(1234)
  expect_silent(new_layer <- suppressMessages(
    pathway_likelihood(pathway_layers, leakage_rate_ci = c(1, 10),
                       viability_rate_ci = c(0, 1))))
  idx <- which(is.finite(rowSums(pathway_layers[])))
  expect_true(all(round(new_layer[][idx, 1], 8) ==
                    round(expected_values[idx], 8)))
})
