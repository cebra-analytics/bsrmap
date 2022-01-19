#' Pest establishment likelihood risk mapping layer
#'
#' Builds a pest establishment likelihood spatial layer by combining
#' (multiplying) pest suitability and arrival likelihood layers.
#'
#' @param pest_suitability_layer A \code{raster::RasterLayer} or
#'   \code{terra::SpatRaster} object representing the pest suitability
#'   (combined abiotic/SDM and biotic/host) spatial layer.
#' @param arrival_likelihood_layer A \code{raster::RasterLayer} or
#'   \code{terra::SpatRaster} object representing the pest arrival likelihood
#'   spatial layer (a combination of one or more pathway layers).
#' @param use_fun One of \code{"prod"}, \code{"sum"}, or \code{"union"}. The
#'   union function is intended for probabilities (via \code{1 - sum(1 - x)}).
#'   Default = \code{"prod"}.
#' @param na.rm Logical indicating whether or not to ignore \code{"NA"} values
#'   when applying the combination function (\code{"use_fun"}). Default =
#'   \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the pest establishment
#'   likelihood spatial layer (combined pest suitability and arrival
#'   likelihood).
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding layer-combining
#'   functions in \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include combine_layers.R
#' @export
establishment_likelihood <- function(pest_suitability_layer,
                                     arrival_likelihood_layer,
                                     use_fun = "prod",
                                     na.rm = FALSE,
                                     filename = "", ...) {

  # Convert layers to terra
  if (class(pest_suitability_layer)[1] %in%
      c("Raster", "RasterStack", "RasterBrick")) {
    pest_suitability_layer <- terra::rast(pest_suitability_layer)
  }
  if (class(arrival_likelihood_layer)[1] %in%
      c("Raster", "RasterStack", "RasterBrick")) {
    arrival_likelihood_layer <- terra::rast(arrival_likelihood_layer)
  }

  # Combine via multiplication
  return(combine_layers(terra::rast(list(pest_suitability_layer,
                                         arrival_likelihood_layer)),
                        use_fun = use_fun,
                        na.rm = na.rm,
                        filename = filename, ...))
}
