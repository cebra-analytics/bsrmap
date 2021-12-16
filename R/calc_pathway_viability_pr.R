#' Calculate the pathway-specific probability of pest establishment viability
#'
#' Calculate the pathway-specific probability at each raster cell of the pest
#' arriving with establishment viability.
#'
#' @param rast A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   containing an estimate of the spatial distribution of the probability of
#'   pest establishment viability.
#' @param eve_pr \code{data.frame} containing overall establishment viability
#'   event probabilities calculated via \code{\link{calc_viable_event_pr}}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the calculated
#'   pathway-specific spatially-explicit pest establishment viability
#'   probability estimates.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Modified version of
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}::calc_pathway_pr}.
#' @export
calc_pathway_viability_pr <- function(rast, eve_pr,
                                      filename = "", ...) {
  UseMethod("calc_pathway_viability_pr")
}

#' @name calc_pathway_viability_pr
#' @export
calc_pathway_viability_pr.Raster <- function(rast, eve_pr,
                                             filename = "", ...) {

  # Call the terra version of the function
  calc_pathway_viability_pr(terra::rast(rast), eve_pr,
                            filename = filename, ...)
}

#' @name calc_pathway_viability_pr
#' @export
calc_pathway_viability_pr.SpatRaster <- function(rast, eve_pr,
                                                 filename = "", ...) {

  # eve_pr contains possible number of incursions (N) and their probabilities
  # (p): cell_viable_prob = 1 - sum_over_N(p*(1 - cell_value)^N)
  rast_df <- terra::as.data.frame(rast, cells = TRUE, na.rm = TRUE)
  viable_prob <- numeric(nrow(rast_df))
  for (i in 1:nrow(eve_pr)) {
    viable_prob <- (
      viable_prob + (eve_pr$probability[i]*
                       (1 - rast_df[,2])^eve_pr$N_incursions[i]))
  }
  viable_prob_rast <- terra::rast(rast) # empty
  viable_prob_rast[rast_df$cell] <- 1 - viable_prob

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    viable_prob_rast <- terra::writeRaster(viable_prob_rast,
                                           filename, ...)
  }

  return(viable_prob_rast)
}
