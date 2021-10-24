#' Calculate the pathway-specific probability of pest arrival
#'
#' Calculate the pathway-specific probability of pest arrival for each raster
#' cell.
#'
#' @param rast A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   containing an estimate of the spatial distribution of the probability of
#'   pest arrival.
#' @param EE \code{data.frame} containing overall establishment event
#'   probabilities calculated via \code{\link{calc_EE}}.
#' @param ... Additional parameters (unused).
#' @return A \code{raster::RasterLayer} or \code{terra::SpatRaster} object (as
#'   per \code{rast}) containing the calculated pathway-specific
#'   spatially-explicit pest arrival probability estimates.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Modified version of
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}::calc_pathway_pr}.
#' @export
calc_pathway_pr <- function(rast, EE, ...) {
  UseMethod("calc_pathway_pr")
}

#' @name calc_pathway_pr
#' @export
calc_pathway_pr.Raster <- function(rast, EE, ...) {

  # Call the terra version of the function
  calc_pathway_pr(terra::rast(rast), EE, ...)
}

#' @name calc_pathway_pr
#' @export
calc_pathway_pr.SpatRaster <- function(rast, EE, ...) {

  # EE contains possible number of incursions (N) and their probabilities (p)
  # cell_arrival_prob = 1 - sum_over_N(p*(1 - cell_value)^N)
  rast_df <- terra::as.data.frame(rast, cells = TRUE, na.rm = TRUE)
  arrival_prob <- numeric(nrow(rast_df))
  for (i in 1:nrow(EE)) {
    arrival_prob <- (arrival_prob +
                       EE$probability[i]*(1 - rast_df[,2])^EE$N_incursions[i])
  }
  arrival_prob_rast <- terra::rast(rast) # empty
  arrival_prob_rast[rast_df$cell] <- 1 - arrival_prob

  return(arrival_prob_rast)
}
