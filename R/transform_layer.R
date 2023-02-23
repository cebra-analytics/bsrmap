#' Transform spatial layer
#'
#' Transforms a spatial layer via a variety of operations, including the
#' application of: linear, exponential, or logarithmic expressions; or lower
#' or upper thresholds; to layer values.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer to be transformed.
#' @param type One of \code{"linear"} (\code{a*x + b}), \code{"exponential"}
#'   (\code{a^x*x^b}), \code{"logarithmic"} (\code{log(x, base = b)}),
#'   \code{"lower"} (\code{x[x < a] = b}), or \code{"upper"}
#'   (\code{x[x > a] = b}).
#' @param a Numeric parameter for transformation as specified by \code{type}.
#'   Default (\code{NULL}) implies: \code{1} for \code{"linear"},
#'   \code{"exponential"}, and \code{"upper"}; and \code{0} for \code{"lower"}.
#' @param b Numeric parameter for transformation as specified by \code{type}.
#'   Default (\code{NULL}) implies: \code{0} for \code{"linear"},
#'   \code{"exponential"}, and \code{"lower"}; \code{1} for \code{"upper"}; and
#'   \code{exp(1)} (natural) for \code{"logarithmic"}. Can be set to \code{NA}
#'   for \code{"lower"} or \code{"upper"} transformations.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the transformed layer.
#' @export
transform_layer <- function(x,
                            type = c("linear",
                                     "exponential",
                                     "logarithmic",
                                     "lower",
                                     "upper"),
                            a = NULL,
                            b = NULL,
                            filename = "", ...) {
  UseMethod("transform_layer")
}

#' @name transform_layer
#' @export
transform_layer.Raster <- function(x,
                                   type = c("linear",
                                            "exponential",
                                            "logarithmic",
                                            "lower",
                                            "upper"),
                                   a = NULL,
                                   b = NULL,
                                   filename = "", ...) {

  # Call the terra version of the function
  transform_layer(terra::rast(x),
                  type = type,
                  a = a,
                  b = b,
                  filename = filename, ...)
}

#' @name transform_layer
#' @export
transform_layer.SpatRaster <- function(x,
                                       type = c("linear",
                                                "exponential",
                                                "logarithmic",
                                                "lower",
                                                "upper"),
                                       a = NULL,
                                       b = NULL,
                                       filename = "", ...) {

  # Match arguments for type
  type <- match.arg(type)

  # Resolve/check a and b
  if (is.null(a)) {
    if (type %in% c("lower")) {
      a <- 0
    } else if (type %in% c("linear", "exponential", "upper")) {
      a <- 1
    }
  } else if (!is.numeric(a) || length(a) > 1) {
    stop("Parameter 'a' should be a single value numeric.", call. = FALSE)
  }
  if (is.null(b)) {
    if (type %in% c("linear", "exponential", "lower")) {
      b <- 0
    } else if (type %in% c("upper")) {
      b <- 1
    } else if (type %in% c("logarithmic")) {
      b <- exp(1)
    }
  } else if (type %in% c("linear", "exponential", "logarithmic") &&
             (!is.numeric(b) || length(b) > 1)) {
    stop("Parameter 'b' should be a single value numeric.", call. = FALSE)
  } else if (type %in% c("lower", "upper") &&
             (!(is.numeric(b) || is.na(b)) || length(b) > 1)) {
    stop("Parameter 'b' should be a single value numeric or NA.",
         call. = FALSE)
  }

  # Perform transformation
  if (type == "linear") {
    x <- a*x + b
  } else if (type == "exponential") {
    x <- a^x*x^b
  } else if (type == "logarithmic") {
    x <- log(x, base = b)
  } else if (type == "lower") {
    if (is.na(b)) {
      x <- terra::app(x, function(v) ((v >= a) | NA)*v)
    } else {
      x <- (x < a)*b + (x >= a)*x
    }
  } else if (type == "upper") {
    if (is.na(b)) {
      x <- terra::app(x, function(v) ((v <= a) | NA)*v)
    } else {
      x <- (x > a)*b + (x <= a)*x
    }
  }
  names(x) <- sprintf("%s_trans", type)

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x <- terra::writeRaster(x, filename, ...)
  }

  return(x)
}
