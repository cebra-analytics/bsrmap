#' Biotic suitability risk mapping layer
#'
#' Builds a biotic suitability spatial layer by combining (multiplying) biotic
#' (host) layers.
#'
#' @param biotic_layers A multi-layer \code{raster::Raster*} or
#'   \code{terra::SpatRaster} object representing the biotic (host) spatial
#'   layers to combine (multiply together) to form a biotic suitability layer.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the combined biotic
#'   suitability layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding layer-combining
#'   functions in \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include combine_layers.R
#' @export
biotic_suitability <- function(biotic_layers,
                               filename = "", ...) {
  return(combine_layers(biotic_layers,
                        use_fun = "prod",
                        filename = filename, ...))
}
