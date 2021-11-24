#' Calculate a buffered hull layer
#'
#' Calculates a spatial layer based on fitting a alpha or convex hull and/or a
#' distance-based buffer to a series of points (features).
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial configuration (template) for calculating
#'   the buffered hull layer from points \code{y}.
#' @param y Point (feature) data as a \code{data.frame} (or \code{matrix})
#'   with WGS84 \emph{lon} and \emph{lat} columns.
#' @param hull One of \code{"alpha"}, \code{"convex"}, or \code{"none"} for
#'   the type of hull to fit to the points \code{y}.
#' @param alpha Numeric alpha parameter for fitting an alpha hull to the
#'   points \code{y}. The value should be in the same units as the coordinates
#'   (or CRS) of the template layer \code{x}. For example, use degrees if the
#'   coordinates are in WGS84 longitudes and latitudes, otherwise coordinates
#'   are typically in meters.
#' @param buffer Numeric buffer distance to place around selected hull, or
#'   around points \code{y} when no hull \code{"none"} is selected. The value
#'   should be in meters when template layer \code{x} coordinates (or CRS) are
#'   in WGS84 longitudes and latitudes, otherwise use the same units as the
#'   coordinates (typically also in meters).
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{raster::RasterLayer} or \code{terra::SpatRaster} object
#'   (as per \code{x}) containing the calculated buffered hull layer.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Generalized modified version of corresponding alpha hull function
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}::rasterize_range}.
#' @export
buffered_hull_layer <- function(x, y,
                                hull = c("alpha", "convex", "none"),
                                alpha = NULL,
                                buffer = NULL,
                                filename = "", ...) {
  UseMethod("buffered_hull_layer")
}

#' @name buffered_hull_layer
#' @export
buffered_hull_layer.Raster <- function(x, y,
                                       hull = c("alpha", "convex", "none"),
                                       alpha = NULL,
                                       buffer = NULL,
                                       filename = "", ...) {
  # Call the terra version of the function
  buffered_hull_layer(terra::rast(x), y,
                      hull = hull,
                      alpha = alpha,
                      buffer = buffer,
                      filename = filename, ...)
}

#' @name buffered_hull_layer
#' @export
buffered_hull_layer.SpatRaster <- function(x, y,
                                           hull = c("alpha", "convex", "none"),
                                           alpha = NULL,
                                           buffer = NULL,
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

    # Conform y coordinates CRS with x
    y <- terra::crds(terra::project(terra::vect(y, crs = "EPSG:4326"), x))
  }

  # Match the points to the non-NA region defined by the template x
  y_vect <- terra::vect(unique(y), crs = terra::crs(x))
  matched <- terra::extract(x, y_vect)
  y_vect <- y_vect[matched[which(is.finite(matched[, 2])), 1]]

  # Fit a hull
  if (hull == "alpha") {

    # Check alpha parameter
    if (!is.numeric(alpha)) {
      stop("Fitting an alpha hull requires a numeric 'alpha' parameter.",
           call. = FALSE)
    }

    # Calculate the alpha hull arcs
    y <- terra::crds(y_vect)
    arcs <- alphahull::ahull(y, alpha = alpha)$arcs

    # Construct the arcs as lines
    arcs_sf <- sf::st_multilinestring(
      apply(arcs, 1, function(arc) {
        angles <- alphahull::anglesArc(arc[c("v.x", "v.y")], arc["theta"])
        seqang <- seq(angles[1], angles[2], length = 1000)
        cbind(x = arc["c1"] + arc["r"]*cos(seqang),
              y = arc["c2"] + arc["r"]*sin(seqang))
      }, simplify = FALSE))

    # Convert to polygons via a small buffer around the arcs and resolve holes
    buffered_geom <- terra::geom(
      terra::vect(sf::st_buffer(arcs_sf, dist = terra::res(x)/100)))
    buffered_geom <- buffered_geom[which(buffered_geom[, "hole"] == 0),]
    y_vect <- terra::vect(buffered_geom, type = "polygons",
                          crs = terra::crs(x))

  } else if (hull == "convex") {
    y_vect <- terra::convHull(y_vect)
  }

  # Place buffer around the hull or points (if no hull)
  if (is.numeric(buffer) && buffer > 0) {
    y_vect <- terra::buffer(y_vect, width = buffer)
  }

  # Rasterize the results and fit within the non-NA region of x
  # y_rast <- terra::rasterize(y_vect, x) # unstable - wait for new terra
  y_rast <- x*0
  cell_idx <- terra::cells(x, y_vect)
  y_rast[cell_idx[, "cell"]] <- 1

  # Conform to template x
  y_rast <- terra::app(terra::rast(list(y_rast, x*0)), fun = "sum",
                       na.rm = TRUE) + x*0

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    y_rast <- terra::writeRaster(y_rast, filename, ...)
  }

  return(y_rast)
}
