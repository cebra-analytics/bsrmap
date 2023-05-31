#' Conform spatial layer
#'
#' Conforms the spatial configuration (CRS, extent, and resolution) of a
#' spatial layer to that of another (template) layer.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer to be conformed.
#' @param y A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer to conform to.
#' @param normalize Logical indicating if the conformed cells should be
#'   normalized, i.e. set to a value 0-1 based on cell-wise minimum and
#'   maximum values, i.e. \code{(value - min)/(max - min)}. Default =
#'   \code{FALSE}.
#' @param binarize Logical indicating if the combined cells should be
#'   binarized, i.e. set to 1 for values > 0. Default = \code{FALSE}.
#' @param platform Logical indicating function is to be run in a platform
#'   environment requiring workaround code. Default = \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the conformed layer.
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
                          platform = FALSE,
                          filename = "", ...) {
  UseMethod("conform_layer")
}

#' @name conform_layer
#' @export
conform_layer.Raster <- function(x, y,
                                 normalize = FALSE,
                                 binarize = FALSE,
                                 platform = FALSE,
                                 filename = "", ...) {

  # Call the terra version of the function
  conform_layer(terra::rast(x), y,
                normalize = normalize,
                binarize = binarize,
                platform = platform,
                filename = filename, ...)
}

#' @name conform_layer
#' @export
conform_layer.SpatRaster <- function(x, y,
                                     normalize = FALSE,
                                     binarize = FALSE,
                                     platform = FALSE,
                                     filename = "", ...) {
  # Convert y to terra
  if (class(y)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    y <- terra::rast(y)
  }

  # Make CRS equal when equivalent but not equal
  if (equivalent_crs(x, y) && terra::crs(x) != terra::crs(y)) {
    terra::crs(x) <- terra::crs(y)
  }

  # Conform resolution, CRS and extent via aggregation and/or re-sampling
  if (platform) { # workaround code
    x_proj_res <- terra::res(terra::project(
      terra::crop(terra::rast(x),
                  (terra::xyFromCell(x, terra::ncell(x) %/% 2)[1,] +
                     c(0, 0, terra::res(x)))[c(1,3,2,4)]), terra::crs(y)))
    x_proj_ext <- terra:: ext(terra::project(
      terra::rast(crs = terra::crs(x), ext = terra::ext(x),
                  res = terra::res(terra::project(terra::rast(y),
                                                  terra::crs(x)))),
      terra::crs(y)))
    x_proj <- terra::rast(crs = terra::crs(y), res = x_proj_res,
                          ext = x_proj_ext)
  } else {
    x_proj <- terra::project(terra::rast(x), terra::crs(y))
  }
  aggregation_factor <- unique(round(terra::res(y)/terra::res(x_proj)))
  if (any(aggregation_factor > 1)) { # resolution y is courser than x
    x <- aggregate_layer(x, y, use_fun = "mean",
                         platform = platform) # includes re-sampling
  } else if (any(terra::res(x) != terra::res(y)) ||
             !equivalent_crs(x, y) ||
             terra::ext(x) != terra::ext(y)) {
    if (!equivalent_crs(x, y)) {
      message("Projecting raster ...")
      x <- terra::project(x, terra::crs(y), method = "near")
    }
    if (any(terra::res(x) != terra::res(y)) ||
        terra::ext(x) != terra::ext(y)) {
      message("Resampling raster ...")
      x <- terra::resample(x, y, method = "near")
    }
  }

  # Normalize when required
  if (normalize) {
    message("Normalizing raster ...")
    terra::setMinMax(x)
    x <- ((x - terra::minmax(x)[1])/
            (terra::minmax(x)[2] - terra::minmax(x)[1]))
    x[][which(x[][,1] > 1), 1] <- 1 # correct minmax rounding error
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
    x <- as.numeric(x > 0)
  }

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x <- terra::writeRaster(x, filename, ...)
  }

  return(x)
}
