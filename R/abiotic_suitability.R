#' Abiotic suitability risk mapping layer
#'
#' Builds an abiotic suitability spatial layer by conforming an existing
#' (global) species distribution model (SDM) output layer to the spatial
#' configuration (CRS, extent, and resolution) of the selected risk mapping
#' template layer.
#'
#' @param sdm_output A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the SDM output spatial layer.
#' @param template A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the risk mapping template spatial layer.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the conformed abiotic
#'   suitability layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Informed by various functions in
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include conform_layer.R
#' @export
abiotic_suitability <- function(sdm_output,
                                template,
                                filename = "", ...) {
  return(conform_layer(sdm_output, template,
                       filename = filename, ...))
}
