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
#'   \code{"median"}, \code{"sum"}, \code{"modal"}, or \code{"union"}:
#'   \code{1 - prod(1 - x)}.
#' @param use_method Use method when projecting and/or re-sampling. One of
#'   \code{"auto"} (uses "near" for categorical rasters else "bilinear"),
#'   \code{"bilinear"}, or \code{"near"}. Default = \code{"auto"}.
#' @param platform Logical indicating function is to be run in a platform
#'   environment requiring workaround code. Default = \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the aggregated layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Informed by various functions in
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include equivalent_crs.R
#' @export
aggregate_layer <- function(x, y,
                            use_fun = c("mean", "max", "min", "median",
                                        "sum", "modal", "union"),
                            use_method = c("auto", "bilinear", "near"),
                            platform = FALSE,
                            filename = "", ...) {
  UseMethod("aggregate_layer")
}

#' @name aggregate_layer
#' @export
aggregate_layer.Raster <- function(x, y,
                                   use_fun = c("mean", "max", "min", "median",
                                               "sum", "modal", "union"),
                                   use_method = c("auto", "bilinear", "near"),
                                   platform = FALSE,
                                   filename = "", ...) {
  # Call the terra version of the function
  aggregate_layer(terra::rast(x), y,
                  use_fun = use_fun,
                  use_method = use_method,
                  platform = platform,
                  filename = filename, ...)
}

#' @name aggregate_layer
#' @export
aggregate_layer.SpatRaster <- function(x, y,
                                       use_fun = c("mean", "max", "min",
                                                   "median", "sum", "modal",
                                                   "union"),
                                       use_method = c("auto", "bilinear", "near"),
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

  # Aggregation when resolution y is courser than x
  x_proj <- terra::project(terra::crop(
    terra::rast(x), terra::project(terra::rast(y), terra::crs(x))),
    terra::crs(y))
  aggregation_factor <- unique(round(terra::res(y)/terra::res(x_proj)))
  if (any(aggregation_factor > 1)) {

    # Project when different CRS or x has larger extent (retain resolution)
    if (!equivalent_crs(x, y) || terra::ext(x_proj) > terra::ext(y)) {
      if (terra::ext(x_proj) > terra::ext(y)) { # use the smaller extent
        project_ext <- terra::ext(y)
      } else {
        project_ext <- terra::ext(x_proj)
      }
      y_template <- terra::rast(crs = terra::crs(y),
                                extent = project_ext,
                                resolution = terra::res(x_proj))
      message("Projecting raster ...")
      if (use_method == "auto") {
        x <- terra::project(x, y_template)
      } else {
        x <- terra::project(x, y_template, method = use_method)
      }
    }

    # Aggregate
    use_fun <- match.arg(use_fun)
    if (use_fun == "union") {
      use_fun <- function(x_vect, na.rm = TRUE) {
        1 - prod(1 - x_vect, na.rm = na.rm)
      }
    }
    message("Aggregating raster ...")
    x <- terra::aggregate(x,
                          fact = aggregation_factor,
                          fun = use_fun,
                          na.rm = TRUE)
  }

  # Re-sample when the resolution, CRS or extent are not equal
  use_method <- match.arg(use_method)
  if (any(terra::res(x) != terra::res(y)) || !equivalent_crs(x, y) ||
      terra::ext(x) != terra::ext(y)) {
    if (!equivalent_crs(x, y)) {
      message("Projecting raster ...")
      if (use_method == "auto") {
        x <- terra::project(x, y)
      } else {
        x <- terra::project(x, y, method = use_method)
      }
    }
    if (any(terra::res(x) != terra::res(y)) ||
        terra::ext(x) != terra::ext(y)) {
      message("Resampling raster ...")
      if (use_method == "auto") {
        x <- terra::resample(x, y)
      } else {
        x <- terra::resample(x, y, method = use_method)
      }
    }
  }

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x <- terra::writeRaster(x, filename, ...)
  }

  return(x)
}
