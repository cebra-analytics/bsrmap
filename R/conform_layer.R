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
#' @param binarize Logical indicating if the conformed layer should be
#'   binarized, i.e. set to 1 for values > 0. Default = \code{FALSE}.
#' @param na_strategy One of \code{"zero"}, \code{"nearest"}, or
#'   \code{"retain"}, indicating the strategy for how to set locations where
#'   \code{x} is \code{NA} but the conform template \code{y} has non-NA values.
#'   By default they are set to \code{"zero"}. Alternatively, unwanted
#'   \code{NA} values (e.g. due to mismatches in coastlines) may be set to the
#'   \code{"nearest"} non-NA values, or when applicable, simply \code{"retain"}
#'   \code{NA} values.
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
                          na_strategy = c("zero", "nearest", "retain"),
                          platform = FALSE,
                          filename = "", ...) {
  UseMethod("conform_layer")
}

#' @name conform_layer
#' @export
conform_layer.Raster <- function(x, y,
                                 normalize = FALSE,
                                 binarize = FALSE,
                                 na_strategy = c("zero", "nearest", "retain"),
                                 platform = FALSE,
                                 filename = "", ...) {

  # Call the terra version of the function
  conform_layer(terra::rast(x), y,
                normalize = normalize,
                binarize = binarize,
                na_strategy = na_strategy,
                platform = platform,
                filename = filename, ...)
}

#' @name conform_layer
#' @export
conform_layer.SpatRaster <- function(x, y,
                                     normalize = FALSE,
                                     binarize = FALSE,
                                     na_strategy = c("zero", "nearest",
                                                     "retain"),
                                     platform = FALSE,
                                     filename = "", ...) {
  # Convert y to terra
  if (class(y)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    y <- terra::rast(y)
  }

  # NA strategy?
  na_strategy <- match.arg(na_strategy)

  # Make CRS equal when equivalent but not equal
  if (equivalent_crs(x, y) && terra::crs(x) != terra::crs(y)) {
    terra::crs(x) <- terra::crs(y)
  }

  # Conform resolution, CRS and extent via aggregation and/or re-sampling
  x_proj <- terra::project(terra::crop(
    terra::rast(x), terra::project(terra::rast(y), terra::crs(x))),
    terra::crs(y))
  aggregation_factor <- unique(round(terra::res(y)/terra::res(x_proj)))
  if (any(aggregation_factor > 1)) { # resolution y is courser than x
    x <- aggregate_layer(x, y, use_fun = "mean",
                         platform = platform) # includes re-sampling
  } else if (any(terra::res(x) != terra::res(y)) ||
             !equivalent_crs(x, y) ||
             terra::ext(x) != terra::ext(y)) {
    if (!equivalent_crs(x, y)) {
      message("Projecting raster ...")
      x <- terra::project(x, y, method = "near")
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
    x[x > 1] <- 1 # correct minmax rounding error
  }

  # Conform to non-NA cells when present
  if (length(terra::unique(y))) {
    if (na_strategy == "zero") {
      message("Conforming to non-NA values (new NAs to zero) ...")
      x <- combine_layers(terra::rast(list(x, y*0)),
                          use_fun = "sum", na.rm = TRUE) + y*0
    } else if (na_strategy == "nearest") {
      message("Conforming to non-NA values (new NAs to nearest) ...")
      idx <- which(is.na(x[]) & !is.na(y[]))
      idx_vect <- terra::vect(terra::xyFromCell(x, idx), crs = terra::crs(x))
      non_na_vect <- terra::as.points(x, values = TRUE, na.rm = TRUE)
      nearest_idx <- as.data.frame(terra::nearest(idx_vect, non_na_vect))$to_id
      x[idx] <- as.data.frame(non_na_vect)[nearest_idx,]
      x <- x*(y*0 + 1)
    } else { # retain NAs
      message("Conforming to non-NA values (new NAs retained) ...")
      x <- x*(y*0 + 1)
    }
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
