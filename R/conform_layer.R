#' Conform spatial layer
#'
#' Conforms the spatial configuration (CRS, extent, and resolution) of a
#' spatial layer to that of another (template) layer.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer to be conformed.
#' @param y A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer to conform to.
#' @param normalize Logical indicating if the combined cells should be
#'   normalized, i.e. set to a value 0-1 based on cell-wise minimum and
#'   maximum values, i.e. \code{(value - min)/(max - min)}. Default =
#'   \code{FALSE}.
#' @param binarize Logical indicating if the combined cells should be
#'   binarized, i.e. set to 1 for values > 0. Default = \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   (as per \code{pathway_layers}) containing the conformed layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Informed by various functions in
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include aggregate_layer.R
#' @include combine_layers.R
#' @include equivalent_crs.R
#' @export
conform_layer <- function(x, y,
                          normalize = FALSE,
                          binarize = FALSE,
                          filename = "", ...) {
  UseMethod("conform_layer")
}

#' @name conform_layer
#' @export
conform_layer.Raster <- function(x, y,
                                 normalize = FALSE,
                                 binarize = FALSE,
                                 filename = "", ...) {

  # Call the terra version of the function
  conform_layer(terra::rast(x), y,
                normalize = normalize,
                binarize = binarize,
                filename = filename, ...)
}

#' @name conform_layer
#' @export
conform_layer.SpatRaster <- function(x, y,
                                     normalize = FALSE,
                                     binarize = FALSE,
                                     filename = "", ...) {
  # Convert y to terra
  if (class(y)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    y <- terra::rast(y)
  }

  # Conform resolution, CRS and extent via aggregation and/or re-sampling
  x_proj <- terra::project(terra::rast(x), terra::crs(y)) # empty
  aggregation_factor <- unique(terra::res(y) %/% terra::res(x_proj))
  if (any(aggregation_factor > 1)) { # resolution y is courser than x
    x <- aggregate_layer(x, y, use_fun = "mean") # includes re-sampling
  } else if (any(terra::res(x) != terra::res(y)) ||
             !equivalent_crs(x, y) ||
             terra::ext(x) != terra::ext(y)) {
    message("Resampling raster ...")
    x <- terra::resample(x, y, method = "near")
  }

  # Conform equivalent CRS
  if (equivalent_crs(x, y)) {
    terra::crs(x) <- terra::crs(y)
  }

  # Normalize when required
  if (normalize) {
    message("Normalizing raster ...")
    terra::setMinMax(x)
    x <- ((x - terra::minmax(x)[1])/
            (terra::minmax(x)[2] - terra::minmax(x)[1]))
  }

  # Conform to non-NA cells when present
  if (length(terra::unique(y))) {
    message("Conforming to non-NA values ...")
    x <- combine_layers(terra::rast(list(x, y*0)),
                        use_fun = "sum", na.rm = TRUE) + y*0
  }

  # Binarize when required
  if (binarize) {
    message("Binarizing raster ...")
    x <- x > 0
  }

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x <- terra::writeRaster(x, filename, ...)
  }

  return(x)
}
