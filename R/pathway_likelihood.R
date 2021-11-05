#' Estimate pathway pest establishment viability likelihood
#'
#' Estimates spatially-explicit pest establishment viability likelihoods for
#' a given pathway from overall leakage and establishment viability rate
#' estimates and their likely spatial distribution.
#'
#' @param pathway_layers A \code{raster::Raster*} or \code{terra::SpatRaster}
#'   object containing one or more layers for estimating the spatial
#'   distribution of establishment viability likelihood.
#' @param leakage_rate_ci Numeric vector of 2 values, giving the lower and
#'   upper bounds (CI) for leakage rate (the number of pest leakage events in
#'   a random year).
#' @param viability_rate_ci Numeric vector of 2 values, giving the lower
#'   and upper bounds (CI) for establishment viability rate (the rate of
#'   pests arriving with viability to establish for leakage events).
#' @param confidence Confidence interval (CI). Default = 0.95.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   (as per \code{pathway_layers}) containing estimated pathway pest
#'   establishment viability likelihoods.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}::arrivals_by_*}.
#' @include calc_viable_event_pr.R
#' @include calc_pathway_viability_pr.R
#' @include combine_layers.R
#' @export
pathway_likelihood <- function(pathway_layers,
                               leakage_rate_ci,
                               viability_rate_ci,
                               confidence = 0.95,
                               filename = "", ...) {
  UseMethod("pathway_likelihood")
}

#' @name pathway_likelihood
#' @export
pathway_likelihood.Raster <- function(pathway_layers,
                                      leakage_rate_ci,
                                      viability_rate_ci,
                                      confidence = 0.95,
                                      filename = "", ...) {

  # Call the terra version of the function
  pathway_likelihood(terra::rast(pathway_layers),
                     leakage_rate_ci, viability_rate_ci,
                     confidence = confidence,
                     filename = filename, ...)
}

#' @name pathway_likelihood
#' @export
pathway_likelihood.SpatRaster <- function(pathway_layers,
                                          leakage_rate_ci,
                                          viability_rate_ci,
                                          confidence = 0.95,
                                          filename = "", ...) {

  # Check leakage and establishment viability rate confidence intervals
  if (is.numeric(leakage_rate_ci) &&
      length(leakage_rate_ci) != 2 &&
      leakage_rate_ci[1] < leakage_rate_ci[2]) {
    stop("Leakage rate CI should have numeric form: (lower, upper).",
         call. = FALSE)
  }
  if (is.numeric(viability_rate_ci) &&
      length(viability_rate_ci) != 2 &&
      leakage_rate_ci[1] < leakage_rate_ci[2]) {
    stop("Establishment rate CI should have numeric form: (lower, upper).",
         call. = FALSE)
  }

  # Combine layers (via product)
  combined_layer <- combine_layers(pathway_layers, use_fun = "prod")

  # Convert to proportion of total full layer
  combined_layer <- combined_layer/as.numeric(
    terra::global(combined_layer, fun = "sum", na.rm = TRUE))

  # Calculate establishment viability event probabilities
  viable_event_probs <- calc_viable_event_pr(leakage_rate_ci,
                                             viability_rate_ci,
                                             confidence)

  # Distribute probabilities across combined layer
  return(calc_pathway_viability_pr(combined_layer, viable_event_probs,
                                   filename = filename, ...))
}
