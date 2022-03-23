#' Calculate a distance weight layer
#'
#' Calculates a distance-weighted spatial layer based on the proximity of each
#' cell to a series of points (features), or via a pre-calculated distance
#' layer. Each cell is weighted via a negative exponential function applied to
#' the distance of the nearest point or pre-calculated distances.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial configuration (template) for calculating
#'   the distance weight layer from points \code{y}, or a for a pre-calculated
#'   distance layer (assumed when \code{y} is missing).
#' @param y Point (feature) data as a \code{data.frame} (or \code{matrix})
#'   with WGS84 \emph{lon} and \emph{lat} columns, and optionally a weights
#'   column having character name specified in \emph{weights}.
#' @param beta Numeric parameter passed to the exponential function:
#'   \code{exp(distance/(1000/beta))}, which transforms cell-to-nearest-point
#'   or pre-calculated distances (m) to distance-weighted cell values. To
#'   generate a distribution that ensures a proportion \emph{p} of a pathway
#'   likelihood is distributed within distance of the nearest point (feature)
#'   or pre-calculated distance \emph{d}, specify \code{beta=log(p)/d} (e.g.
#'   to have 50% of the likelihood distributed within 200 km use default:
#'   \code{log(0.5)/200}).
#' @param weights Optional (default is none) numeric vector of weights for
#'   each point (feature), or character name of column or attribute in
#'   \code{y}, that contains weight values for each point, such that the
#'   distance-weighted cell values will be additionally proportionally
#'   weighted. Thus the exponential function (above) becomes: the maximum of
#'   \code{exp(distance/(1000/beta))*weight} for each cell.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing distance-weighted cell
#'   values.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding distance-weight
#'   functions in \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @export
distance_weight_layer <- function(x, y,
                                  beta = log(0.5)/200,
                                  weights = NULL,
                                  filename = "", ...) {
  UseMethod("distance_weight_layer")
}

#' @name distance_weight_layer
#' @export
distance_weight_layer.Raster <- function(x, y,
                                         beta = log(0.5)/200,
                                         weights = NULL,
                                         filename = "", ...) {
  # Call the terra version of the function
  distance_weight_layer(terra::rast(x), y,
                        beta = beta,
                        weights = weights,
                        filename = filename, ...)
}

#' @name distance_weight_layer
#' @export
distance_weight_layer.SpatRaster <- function(x, y,
                                             beta = log(0.5)/200,
                                             weights = NULL,
                                             filename = "", ...) {
  if (!missing(y)) {

    # Check points (features) y
    y <- as.data.frame(y)
    if (ncol(y) < 2) {
      stop("Point (features) y data should have at least 2 columns.",
           call. = FALSE)
    }
    if (!all(c("lon", "lat") %in% names(y))) {
      stop("Point (features) y data coordinates should be 'lon' and 'lat'.",
           call. = FALSE)
    }

    # Check weights - should have the same number of rows as y
    if (is.numeric(weights) && length(weights) != nrow(y)) {
      stop("Weights should have the same number of rows as y.", call. = FALSE)
    }

    # Weights specified as a column of y
    if (is.character(weights)) {
      if (weights %in% names(y)) {
        weights <- y[, weights]
      } else {
        stop(sprintf("Weights column %s is not present in point data y.",
                     weights), call. = FALSE)
      }
    }

    # Conform y coordinates CRS with x
    y <- terra::crds(terra::project(terra::vect(y, crs = "EPSG:4326"), x))
  }

  # Are weights present?
  if (missing(y) || is.null(weights) || all(weights[1] == weights)) {

    if (!missing(y)) {

      # Calculate cell distances from point (features) y
      # Note: currently implemented with raster::distanceFromPoints
      #       until/if terra::distance is fixed...
      suppressWarnings(d_rast <- terra::rast(
        raster::distanceFromPoints(raster::raster(x), y)) + x*0)

    } else { # assume pre-calculated distances
      d_rast <- x
    }

    # Calculate distance weights (write to file when required)
    if (is.character(filename) && nchar(filename) > 0) {
      weight_rast <- terra::writeRaster(exp(d_rast/(1000/beta)), filename, ...)
    } else {
      weight_rast <- exp(d_rast/(1000/beta))
    }

  } else {

    # Normalize weights
    weights <- weights/max(weights)

    # Calculate 'weighted' distance weights
    weight_rast <-  x*0
    w <- terra::values(weight_rast)
    suppressWarnings(x_rast <- raster::raster(x))
    for (i in 1:nrow(y)) {
      d <- raster::values(raster::distanceFromPoints(x_rast, y[i, ]))
      w <- pmax(w, exp(d/(1000/beta))*weights[i])
    }

    # Write to file when required
    if (is.character(filename) && nchar(filename) > 0) {
      weight_rast <- terra::init(terra::rast(weight_rast), w, filename, ...)
    } else {
      terra::values(weight_rast) <- w
    }
  }

  return(weight_rast)
}
