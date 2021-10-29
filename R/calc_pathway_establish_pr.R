#' Calculate the pathway-specific probability of pest establishment
#'
#' Calculate the pathway-specific probability of pest establishment for each
#' raster cell.
#'
#' @param rast A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   containing an estimate of the spatial distribution of the probability of
#'   pest establishment.
#' @param ee_pr \code{data.frame} containing overall establishment event
#'   probabilities calculated via \code{\link{calc_establish_event_pr}}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{raster::RasterLayer} or \code{terra::SpatRaster} object (as
#'   per \code{rast}) containing the calculated pathway-specific
#'   spatially-explicit pest establishment probability estimates.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Modified version of
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}::calc_pathway_pr}.
#' @export
calc_pathway_establish_pr <- function(rast, ee_pr,
                                      filename = "", ...) {
  UseMethod("calc_pathway_establish_pr")
}

#' @name calc_pathway_establish_pr
#' @export
calc_pathway_establish_pr.Raster <- function(rast, ee_pr,
                                             filename = "", ...) {

  # Call the terra version of the function
  calc_pathway_establish_pr(terra::rast(rast), ee_pr,
                            filename = filename, ...)
}

#' @name calc_pathway_establish_pr
#' @export
calc_pathway_establish_pr.SpatRaster <- function(rast, ee_pr,
                                                 filename = "", ...) {

  # ee_pr contains possible number of incursions (N) and their probabilities
  # (p): cell_establish_prob = 1 - sum_over_N(p*(1 - cell_value)^N)
  rast_df <- terra::as.data.frame(rast, cells = TRUE, na.rm = TRUE)
  establish_prob <- numeric(nrow(rast_df))
  for (i in 1:nrow(ee_pr)) {
    establish_prob <- (
      establish_prob + (ee_pr$probability[i]*
                          (1 - rast_df[,2])^ee_pr$N_incursions[i]))
  }
  establish_prob_rast <- terra::rast(rast) # empty
  establish_prob_rast[rast_df$cell] <- 1 - establish_prob

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    establish_prob_rast <- terra::writeRaster(establish_prob_rast,
                                              filename, ...)
  }

  return(establish_prob_rast)
}
