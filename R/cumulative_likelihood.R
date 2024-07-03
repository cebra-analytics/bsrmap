#' Cumulative likelihood
#'
#' Calculates the ordered cumulative likelihood of a spatial likelihood layer.
#' The output may be displayed as cumulative ordered likelihood, or as
#' percentage of ordered cumulative total in decile-like groupings.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial likelihood layer.
#' @param output One of ordered cumulative \code{"likelihood"} (default) or
#'   \code{"percentage"} of ordered cumulative total in decile-like groupings.
#' @param order_by Either \code{"ascending"} (default) or \code{"descending"}
#'   order of likelihood.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing the cumulative
#'   likelihood layer. Percentage outputs produce decile-like categorical
#'   layers.
#' @export
cumulative_likelihood <- function(x,
                                  output = c("likelihood",
                                             "percentage"),
                                  order_by = c("ascending",
                                               "descending"),
                                  filename = "", ...) {
  UseMethod("cumulative_likelihood")
}

#' @name cumulative_likelihood
#' @export
cumulative_likelihood.Raster <- function(x,
                                         output = c("likelihood",
                                                    "percentage"),
                                         order_by = c("ascending",
                                                      "descending"),
                                         filename = "", ...) {

  # Call the terra version of the function
  cumulative_likelihood(terra::rast(x),
                        output = output,
                        order_by = order_by,
                        filename = filename, ...)
}

#' @name cumulative_likelihood
#' @export
cumulative_likelihood.SpatRaster <- function(x,
                                             output = c("likelihood",
                                                        "percentage"),
                                             order_by = c("ascending",
                                                          "descending"),
                                             filename = "", ...) {

  # Match arguments
  output <- match.arg(output)
  order_by <- match.arg(order_by)

  # Calculate ordered cumulative sum
  x_df <- terra::as.data.frame(x, cells = TRUE, na.rm = TRUE)
  names(x_df) <- c("cell", "val")
  x_df <- x_df[order(x_df$val, decreasing = (order_by == "descending")),]
  x_df$val <- cumsum(x_df$val)
  x[x_df$cell] <- x_df$val

  # Convert to percentage decile-like groupings when required
  if (output == "percentage") {
    intervals <- (0:10)*max(x_df$val)/10
    x_p <- terra::classify(x, data.frame(from = intervals[1:10],
                                         to = intervals[2:11], ID = 0:9),
                           include.lowest = TRUE)
    levels(x_p) <- data.frame(ID = 0:9, cum_lhood = paste0((0:9)*10, "-",
                                                           (1:10)*10, "%"))
    x <- x_p
  }

  # Write to file when required
  if (is.character(filename) && nchar(filename) > 0) {
    x <- terra::writeRaster(x, filename, ...)
  }

  return(x)
}
