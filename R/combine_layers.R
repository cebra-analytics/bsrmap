#' Combine spatial layers
#'
#' Combines multiple spatial layers via cell-wise multiplication, addition,
#' or union (via complements), and optionally binarizes the output.
#'
#' @param x A multi-layer \code{raster::Raster*} or \code{terra::SpatRaster}
#'   object representing the spatial layers to combine.
#' @param use_fun One of \code{"prod"}, \code{"sum"}, or \code{"union"}.
#' @param na.rm Logical indicating whether or not to ignore \code{"NA"} values
#'   when applying the combination function (\code{"use_fun"}).
#' @param binarize Logical indicating if the combined cells should be
#'   binarized, i.e. set to 1 for values > 0. Default = \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   (as per \code{pathway_layers}) containing the combined layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding layer-combining
#'   functions in \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @export
combine_layers <- function(x,
                           use_fun = c("prod", "sum", "union"),
                           na.rm = FALSE,
                           binarize = FALSE,
                           filename = "", ...) {
  UseMethod("combine_layers")
}

#' @name combine_layers
#' @export
combine_layers.Raster <- function(x,
                                  use_fun = c("prod", "sum", "union"),
                                  na.rm = FALSE,
                                  binarize = FALSE,
                                  filename = "", ...) {

  # Call the terra version of the function
  combine_layers(terra::rast(x),
                 use_fun = use_fun,
                 na.rm = na.rm,
                 binarize = binarize,
                 filename = filename, ...)
}

#' @name combine_layers
#' @export
combine_layers.SpatRaster <- function(x,
                                      use_fun = c("prod", "sum", "union"),
                                      na.rm = FALSE,
                                      binarize = FALSE,
                                      filename = "", ...) {

  # Apply function to layers when multiple layers
  if (terra::nlyr(x) > 1) {
    message(sprintf("Calculating raster %s ...", use_fun))
    if (use_fun %in% c("prod", "sum")) {
      x_combined <- terra::app(x, fun = use_fun, na.rm = na.rm)
    } else if (use_fun == "union") {
      x_combined <- 1 - terra::app(1 - x, fun = "prod", na.rm = na.rm)
    }
  } else { # single layer
    x_combined <- x
  }

  # Binarize when required
  if (binarize) {
    message("Binarizing raster ...")
    x_combined <- x_combined > 0
  }

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x_combined <- terra::writeRaster(x_combined, filename, ...)
  }

  return(x_combined)
}
