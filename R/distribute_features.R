#' Distribute features across a raster layer
#'
#' Distributes feature polygon values across a template or mask raster spatial
#' layer. The total value assigned to each polygon is divided across all
#' corresponding raster cells when a template (single value plus NAs) raster
#' is provided, or across non-zero cells when a mask (zero and non-zero values
#' plus NAs) raster is provided, which is used as a binary mask (regardless of
#' the variety of non-zero values).
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial configuration (template or mask) for
#'   distributing the feature (polygon) values from \code{y}.
#' @param y Feature polygon data as a \code{terra::SpatVector} or a simple
#'   feature \code{sf::sf}. Note that only the feature polygons of \code{y}
#'   contained entirely within the spatial extent of \code{x} will be
#'   distributed.
#' @param vars A vector of character variable names corresponding to the
#'   features (columns) in \code{y} to distribute.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A multi-layer \code{terra::SpatRaster} object containing a spatial
#'   layer for each feature variable from \code{y} specified in \code{vars}.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}} functionality for
#'   distributing polygons across raster cells.
#' @include equivalent_crs.R
#' @export
distribute_features <- function(x, y,
                                vars = NULL,
                                filename = "", ...) {
  UseMethod("distribute_features")
}

#' @name distribute_features
#' @export
distribute_features.Raster <- function(x, y,
                                       vars = NULL,
                                       filename = "", ...) {
  # Call the terra version of the function
  distribute_features(terra::rast(x), y,
                      vars = vars,
                      filename = filename, ...)
}

#' @name distribute_features
#' @export
distribute_features.SpatRaster <- function(x, y,
                                           vars = NULL,
                                           filename = "", ...) {
  if (!missing(y)) {

    # Is y a terra vector or sf object?
    if (!class(y)[1] %in% c("SpatVector", "sf")) {
      stop("Feature y data should be a terra::SpatVector or sf::sf object.",
           call. = FALSE)
    }

    # Convert to terra vector
    if ("sf" %in% class(y)) {
      y <- terra::vect(y)
    }

    # Conform the CRS of y to that of x
    if (!equivalent_crs(x, y)) {
      y <- terra::project(y, terra::crs(x))
    } else if (equivalent_crs(x, y) && terra::crs(x) != terra::crs(y)) {
      terra::crs(y) <- terra::crs(x)
    }
  }

  # Resolve x as a mask or a template
  if (nrow(terra::unique(x)) > 1) { # mask
    x <- +(x > 0)
  } else { # template
    x <- x*0 + 1
  }

  # Select variables and add index column to y
  if (!is.null(vars) && all(vars %in% names(y))) {
    y <- y[,vars]
  }
  y$index <- 1:nrow(y)

  # Extract x cell and coverage for each feature in y
  x_y_df <- exactextractr::exact_extract(
    x, sf::st_as_sf(y[,"index"]),
    fun = function(a) a,
    include_cell = TRUE,
    summarize_df = TRUE,
    include_cols = "index",
    progress = FALSE)

  # Only include active (non-NA and/or non-zero) cells
  x_y_df <- dplyr::select(dplyr::filter(x_y_df, x_y_df$value == 1),
                          index, cell, coverage_fraction)

  # Normalise coverage fraction
  x_y_df <- dplyr::mutate(
    dplyr::group_by(x_y_df, index),
    coverage_fraction = coverage_fraction/sum(coverage_fraction))

  # Extract variable column data from y
  y_df <- terra::as.data.frame(y)

  # Distribute feature values for each variable across active cells
  y_rast <- list()
  for (v in vars) {

    # Select cells with non-zero feature variable values
    y_df_v <- dplyr::filter(y_df[,c("index", v)], y_df[,v] > 0)
    names(y_df_v)[2] <- "value"
    x_y_df_v <- dplyr::inner_join(x_y_df, y_df_v, by = c("index" = "index"))

    # Distribute total variable values across cell coverage
    x_y_df_v <- dplyr::select(
      dplyr::mutate(x_y_df_v, value = value*coverage_fraction),
      index, cell, value)

    # Sum the distributed values at each cell (note: duplicates are possible)
    x_y_df_v <- dplyr::summarise(dplyr::group_by(x_y_df_v, cell),
                                 value = sum(value))

    # Place values in a spatial raster using x as a template
    y_rast[[v]] <- as.numeric(x*0)
    y_rast[[v]][x_y_df_v$cell] <- x_y_df_v$value
  }

  # Collect as a multilayered raster
  y_rast <- terra::rast(y_rast)

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    y_rast <- terra::writeRaster(y_rast, filename, ...)
  }

  return(y_rast)
}
