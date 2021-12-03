#' Convert point data to a raster layer
#'
#' Converts point data to a raster layer defined by a template raster by
#' setting the value of each raster cell to the value of the nearest point
#' within a maximum distance range. The template may optionally define the
#' nearest point indices and distances as separate spatial layers to save
#' processing time for reusable data sets.
#'
#' @param x A \code{raster::Raster*} or \code{terra::SpatRaster}
#'   object representing the spatial configuration (template) of the layer to
#'   be set via point values \code{y}. Optionally \code{x} may define the
#'   nearest point indices and distances via two spatial layers labelled
#'   "nearest" and "distance" respectively.
#' @param y Point data as a \code{data.frame} (or \code{matrix}) with WGS84
#'   \emph{lon} and \emph{lat} columns, as well as a named column of point
#'   values specified by the \code{name} parameter.
#' @param name Character name of the column of \code{y} containing the point
#'   values. Default is "predicted" (as per outputs of
#'   \code{\href{bssdm}{https://github.com/cebra-analytics/bssdm}::predict}).
#' @param max_distance Numeric maximum distance range used to find the nearest
#'   point from \code{y} for each cell in \code{x}. Default is 250 km.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{raster::Raster} or \code{terra::SpatRaster} object (as per
#'   \code{x}) containing a spatial layer with each cell value set to the
#'   value of the nearest point from \code{y} when within the specified
#'   \code{max_distance} range.
#' @include equivalent_crs.R
#' @export
points_to_layer <- function(x, y,
                            name = "predicted",
                            max_distance = 250, # km
                            filename = "", ...) {
  UseMethod("points_to_layer")
}

#' @name points_to_layer
#' @export
points_to_layer.Raster <- function(x, y,
                                   name = "predicted",
                                   max_distance = 250, # km
                                   filename = "", ...) {
  # Call the terra version of the function
  points_to_layer(terra::rast(x), y,
                  name = name,
                  max_distance = max_distance,
                  filename = filename, ...)
}

#' @name points_to_layer
#' @export
points_to_layer.SpatRaster <- function(x, y,
                                       name = "predicted",
                                       max_distance = 250, # km
                                       filename = "", ...) {
  if (!missing(y)) {

    # Check points (features) y
    y <- as.data.frame(y)
    if (ncol(y) < 3) {
      stop("Point y data should have at least 3 columns.",
           call. = FALSE)
    }
    if (!all(c("lon", "lat") %in% names(y))) {
      stop("Point y data coordinates should be 'lon' and 'lat'.",
           call. = FALSE)
    }
    if (is.null(name)) { # use name of first non-coordinate column
      name <- names(y)[which.max(!names(y) %in% c("lon", "lat"))]
    }
    if (is.null(name) || !name %in% names(y)) {
      stop("Point y data values should be in column specified by 'name'",
           call. = FALSE)
    }

    # Convert to terra vector
    y <- terra::vect(y, crs = "EPSG:4326")

    # Conform the CRS of y to that of x
    if (!equivalent_crs(x, y)) {
      y <- terra::project(y, terra::crs(x))
    }
  }

  # Match the active cells in x with corresponding points in y
  y_cell <- as.data.frame(terra::cells(x, y))
  y <- data.frame(ID = 1:nrow(y), value = y[, name, drop = TRUE][,1])
  y_cell <- dplyr::inner_join(y_cell, y, by = c("ID" = "ID"))

  # Average the matched values at each cell (note: duplicates are possible)
  y_cell <- dplyr::summarise(dplyr::group_by(y_cell, cell),
                             value = mean(value))

  # Convert cells to simple features (for distance calculations)
  y_cell <- sf::st_as_sf(cbind(terra::xyFromCell(x, y_cell$cell), y_cell),
                         coords = c("x", "y"), crs = terra::crs(x))
  x_cell <- sf::st_as_sf(terra::as.data.frame(x, xy = TRUE, cells = TRUE),
                         coords = c("x", "y"), crs = terra::crs(x))

  # Calculate the nearest cell in y cell for each cell in x
  if (!all(c("nearest", "distance") %in% names(x_cell))) { # predefined?
    x_cell$nearest <- sf::st_nearest_feature(x_cell, y_cell)
    x_cell$distance <- as.numeric(sf::st_distance(x_cell,
                                                  y_cell[x_cell$nearest,],
                                                  by_element = TRUE))/1000
  }

  # Set cell values to that of the nearest cell up to a maximum distance
  x_cell$value <- y_cell$value[x_cell$nearest]
  x_cell <- dplyr::filter(x_cell, distance <= max_distance)

  # Place values in a spatial raster using x as a template
  y_rast <- terra::rast(x[[1]])
  y_rast[x_cell$cell] <- x_cell$value

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    y_rast <- terra::writeRaster(y_rast, filename, ...)
  }

  return(y_rast)
}
