#' Checks if CRS of spatial layers is equivalent
#'
#' Checks if the coordinate reference system (CRS) of two spatial layers are
#' equivalent via removing some redundant PROJ string information. Not
#' guaranteed to work in every case. A work in progress.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing a spatial layer to be compared.
#' @param y Another \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing a spatial layer for comparison.
#' @param ... Additional parameters (unused).
#' @return A logical value indicating if the CRS of spatial layers \code{x} and
#'   \code{y} are equivalent (in some cases).
#' @export
equivalent_crs <- function(x, y, ...) {
  UseMethod("equivalent_crs")
}

#' @name equivalent_crs
#' @export
equivalent_crs.Raster <- function(x, y, ...) {

  # Call the terra version of the function
  equivalent_crs(terra::rast(x), y, ...)
}

#' @name equivalent_crs
#' @export
equivalent_crs.SpatRaster <- function(x, y, ...) {

  # Convert y to terra
  if (class(y)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    y <- terra::rast(y)
  }

  # Eliminate the "towgs84" element in the CRS projection strings
  return(sub("..towgs84=[^[:space:]]*", "", terra::crs(x, proj = TRUE)) ==
           sub("..towgs84=[^[:space:]]*", "", terra::crs(y, proj = TRUE)))
}
