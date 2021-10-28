#' Aggregate spatial layer
#'
#' Aggregate the spatial resolution of a fine grain layer to that of another
#' (template) layer having a course resolution. Other spatial configuration
#' (CRS and extent) are checked and adjusted to ensure they also match that of
#' the template.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer to be aggregated.
#' @param y A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer with the spatial resolution (and
#'   configuration) to aggregate (conform) to.
#' @param use_fun One of \code{"mean"}, \code{"max"}, \code{"min"},
#'   \code{"median"}, \code{"sum"}, or \code{"modal"}.
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
#' @include equivalent_crs.R
#' @export
aggregate_layer <- function(x, y,
                            use_fun = c("mean", "max", "min",
                                        "median", "sum", "modal"),
                            filename = "", ...) {
  UseMethod("aggregate_layer")
}

#' @name aggregate_layer
#' @export
aggregate_layer.Raster <- function(x, y,
                                   use_fun = c("mean", "max", "min",
                                               "median", "sum", "modal"),
                                   filename = "", ...) {
  # Call the terra version of the function
  aggregate_layer(terra::rast(x), y,
                  use_fun = use_fun,
                  filename = filename, ...)
}

#' @name aggregate_layer
#' @export
aggregate_layer.SpatRaster <- function(x, y,
                                       use_fun = c("mean", "max", "min",
                                                   "median", "sum", "modal"),
                                       filename = "", ...) {
  # Convert y to terra
  if (class(y)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    y <- terra::rast(y)
  }

  # Aggregation when resolution y is courser than x
  x_proj <- terra::project(terra::rast(x), terra::crs(y))
  aggregation_factor <- unique(terra::res(y) %/% terra::res(x_proj))
  if (any(aggregation_factor > 1)) {

    # Project when CRS or extent are not equal (retain resolution)
    if (!equivalent_crs(x, y) ||
        terra::ext(x_proj) != terra::ext(y)) {
      y_template <- terra::rast(crs = terra::crs(y),
                                extent = terra::ext(y),
                                resolution = terra::res(x))
      message("Projecting raster ...")
      x <- terra::project(x, y_template)
    }

    # Aggregate
    message("Aggregating raster ...")
    x <- terra::aggregate(x,
                          fact = aggregation_factor,
                          fun = use_fun,
                          na.rm = TRUE)
  }

  # Re-sample when the resolution, CRS or extent are not equal
  if (any(terra::res(x) != terra::res(y)) ||
      !equivalent_crs(x, y) ||
      terra::ext(x) != terra::ext(y)) {
    message("Resampling raster ...")
    x <- terra::resample(x, y, method = "near")
  }

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x <- terra::writeRaster(x, filename, ...)
  }

  return(x)
}
