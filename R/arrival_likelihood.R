#' Pest arrival likelihood risk mapping layer
#'
#' Builds a arrival likelihood spatial layer by combining pest pathway
#' likelihood layers (via layer union).
#'
#' @param pathway_likelihood_layers A multi-layer \code{raster::Raster*} or
#'   \code{terra::SpatRaster} object representing the pathway likelihood layers
#'   to combine (union) to form a pest arrival likelihood layer.
#' @param use_fun One of \code{"prod"}, \code{"sum"}, or \code{"union"}. The
#'   union function is intended for probabilities (via \code{1 - sum(1 - x)}).
#'   Default = \code{"union"}.
#' @param na.rm Logical indicating whether or not to ignore \code{"NA"} values
#'   when applying the combination function (\code{"use_fun"}). Default =
#'   \code{TRUE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the combined pest
#'   arrival likelihood layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding layer-combining
#'   functions in \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include combine_layers.R
#' @export
arrival_likelihood <- function(pathway_likelihood_layers,
                               use_fun = "union",
                               na.rm = TRUE,
                               filename = "", ...) {
  return(combine_layers(pathway_likelihood_layers,
                        use_fun = use_fun,
                        na.rm = na.rm,
                        filename = filename, ...))
}
