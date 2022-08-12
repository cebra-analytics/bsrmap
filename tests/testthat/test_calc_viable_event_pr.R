context("calc_viable_event_pr")

test_that("generates a event probability table via simulated sampling", {
  TEST_DIRECTORY <- test_path("test_inputs")
  expect_error(calc_viable_event_pr(n_events = 10, p_viable = 1.0),
               "n_events (CI) should have numeric form: (lower, upper).",
               fixed = TRUE)
  expect_error(calc_viable_event_pr(n_events = c(1, 10), p_viable = 1.0),
               "p_viable (CI) should have numeric form: (lower, upper).",
               fixed = TRUE)
  expect_error(calc_viable_event_pr(n_events = c(1, 10), p_viable = c(1.0, 0.5)),
               "p_viable (CI) should have numeric form: (lower, upper).",
               fixed = TRUE)
  expect_error(calc_viable_event_pr(n_events = c(1, 10),
                                    p_viable = c(-0.5, 1.0)),
               "p_viable (CI) probability values should be >= 0 and <= 1.",
               fixed = TRUE)
  expect_silent(event_pr <- calc_viable_event_pr(n_events = c(1, 10),
                                                 p_viable = c(0, 1)))
  expect_is(event_pr, "data.frame")
  expect_named(event_pr, c("N_incursions", "probability"))
  expect_equal(sum(event_pr$probability), 1)
})
