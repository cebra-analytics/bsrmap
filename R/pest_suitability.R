#' Pest suitability risk mapping layer
#'
#' Builds a pest suitability spatial layer by combining (multiplying) abiotic
#' (SDM) and biotic (host) suitability layers.
#'
#' @param abiotic_layer A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the abiotic (SDM) spatial layer.
#' @param biotic_layer A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the biotic (host) spatial layer.
#' @param use_fun One of \code{"prod"}, \code{"sum"}, or \code{"union"}. The
#'   union function is intended for probabilities (via \code{1 - prod(1 - x)}).
#'   Default = \code{"prod"}.
#' @param na.rm Logical indicating whether or not to ignore \code{"NA"} values
#'   when applying the combination function (\code{"use_fun"}). Default =
#'   \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the combined pest
#'   suitability layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding layer-combining
#'   functions in \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include combine_layers.R
#' @export
pest_suitability <- function(abiotic_layer, biotic_layer,
                             use_fun = "prod",
                             na.rm = FALSE,
                             filename = "", ...) {

  # Convert layers to terra
  if (class(abiotic_layer)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    abiotic_layer <- terra::rast(abiotic_layer)
  }
  if (class(biotic_layer)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    biotic_layer <- terra::rast(biotic_layer)
  }

  # Combine via multiplication
  return(combine_layers(terra::rast(list(abiotic_layer, biotic_layer)),
                        use_fun = use_fun,
                        na.rm = na.rm,
                        filename = filename, ...))
}
