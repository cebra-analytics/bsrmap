#' Calculate establishment viability event probabilities
#'
#' Calculate (via stochastic sampling) the probability of one or more
#' arrival events with establishment viability occurring as as a function of
#' estimated leakage numbers and the probability a leakage event could result
#' in an arrival with establishment viability.
#'
#' @param n_events Numeric vector containing lower and upper bounds (CI) for
#'   the number of leakage events.
#' @param p_viable Numeric vector containing the lower and upper bounds
#'   (CI) for the probability a leakage event could result in an arrival with
#'   establishment viability.
#' @param confidence Confidence interval (CI). Default = 0.95.
#' @param n_sims Integer. Number of samples to be taken from event and
#'   establishment viability distributions. Default = 100000.
#' @param ... Additional parameters (unused).
#' @return A \code{data.frame} containing the possible number of incursions
#'   that may occur, and their corresponding probabilities.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Modified version of
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}::calc_EE}.
#' @export
calc_viable_event_pr <- function(n_events, p_viable,
                                 confidence = 0.95,
                                 n_sims = 100000, ...) {

  # Check n_events and p_viable confidence intervals
  if (is.numeric(n_events) && (length(n_events) != 2 ||
      n_events[1] > n_events[2])) {
    stop("n_events (CI) should have numeric form: (lower, upper).",
         call. = FALSE)
  }
  if (is.numeric(p_viable) && (length(p_viable) != 2 ||
      p_viable[1] > p_viable[2])) {
    stop("p_viable (CI) should have numeric form: (lower, upper).",
         call. = FALSE)
  } else if (is.numeric(p_viable) &&
             (any(p_viable < 0) || any(p_viable > 1))) {
    stop("p_viable (CI) probability values should be >= 0 and <= 1.",
         call. = FALSE)
  }
  if (p_viable[1] == 0) {
    p_viable[1] <- 1e-10
  }
  if (p_viable[2] == 1) {
    p_viable[2] <- 1 - 1e-10
  }

  ## Calculate leakage number
  log_mean <- mean(log(n_events))
  log_sd <- ((log_mean - log(min(n_events)))/
               stats::qnorm(1 - (1 - confidence)/2))

  # Sample lambda from lognormal
  lambda <- stats::rlnorm(n = n_sims, meanlog = log_mean, sdlog = log_sd)

  # Sample leakage from poisson using lambda
  n <- stats::rpois(n = n_sims, lambda = lambda)

  ## Calculate establishment viability probability
  logit_mean <- mean(stats::qlogis(p_viable))
  logit_sd <- ((logit_mean - stats::qlogis(min(p_viable)))/
                 stats::qnorm(1 - (1 - confidence)/2))

  # Sample probability from logit normal
  logit_p <- stats::rnorm(n = n_sims, mean = logit_mean, sd = logit_sd)

  # Sample incursion events from binomial distribution
  est <- stats::rbinom(n = n_sims, size = n, prob = stats::plogis(logit_p))
  est <- table(est)/length(est)

  # Return a table of probabilities for incursion numbers
  return(data.frame(N_incursions = as.integer(names(est)),
                    probability = c(est)))
}
