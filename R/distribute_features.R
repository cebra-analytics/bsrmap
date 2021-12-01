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
#' @return A \code{raster::Raster*} or \code{terra::SpatRaster} object (as per
#'   \code{x}) containing a spatial layer for each feature variable from
#'   \code{y} specified in \code{vars}.
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
    }
  }

  # Only use features in y that are entirely within the extent of x
  within_x <- sapply(1:nrow(y), function (i) {
    (terra::xmin(y[i]) >= terra::xmin(x) &
       terra::xmax(y[i]) <= terra::xmax(x) &
       terra::ymin(y[i]) >= terra::ymin(x) &
       terra::ymax(y[i]) <= terra::ymax(x)
    )
  })
  y <- y[within_x]

  # Get cell indices for active (non-NA and/or non-zero) cells in x
  x_df <- terra::as.data.frame(x, cells = TRUE, na.rm = TRUE)
  if (length(unique(x_df[,2])) > 1) { # non-zero cells
    x_df <- dplyr::select(dplyr::filter(x_df, x_df[,2] > 0), cell)
  } else { # non-NA only
    x_df <- dplyr::select(x_df, cell)
  }

  # Extract variable column data from y
  y_df <- cbind(ID = 1:nrow(y), terra::as.data.frame(y[,vars]))

  # Match the active cells in x with corresponding features in y
  cell_id <- as.data.frame(terra::cells(x, y))
  cell_id <- dplyr::inner_join(cell_id, x_df, by = c("cell" = "cell"))

  # Count the number of active cells corresponding to each feature
  cell_count <- dplyr::count(dplyr::group_by(cell_id, ID))
  cell_id <- dplyr::left_join(cell_id, cell_count, by = c("ID" = "ID"))

  # Distribute feature values for each variable across active cells
  y_rast <- list()
  for (v in vars) {

    # Select active cells with non-zero feature variable values
    y_df_v <- dplyr::filter(y_df[,c("ID", v)], y_df[,v] > 0)
    cell_id_v <- dplyr::inner_join(cell_id, y_df_v, by = c("ID" = "ID"))

    # Distribute total variable values across active cells
    names(cell_id_v)[names(cell_id_v) == v] <- "value"
    cell_id_v <- dplyr::mutate(cell_id_v, value = value/n)

    # Sum the distributed values at each cell (note: duplicates are possible)
    cell_id_v <- dplyr::summarise(dplyr::group_by(cell_id_v, cell),
                                  value = sum(value))

    # Place values in a spatial raster using x as a template
    y_rast[[v]] <- x*0
    y_rast[[v]][cell_id_v$cell] <- cell_id_v$value
  }

  # Collect as a multilayered raster
  y_rast <- terra::rast(y_rast)

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    y_rast <- terra::writeRaster(y_rast, filename, ...)
  }

  return(y_rast)
}
