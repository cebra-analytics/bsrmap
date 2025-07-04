#' Aggregate categories within a spatial layer
#'
#' Aggregate the categories within a spatial layer to a courser resolution,
#' that of another (template) layer, based on the presence of each of the
#' user-selected categories in each cell. The resulting aggregated layer cells
#' may be binarized to one or zero, or indicate the proportion of selected
#' categories present in the cell.
#'
#' @param x A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer of category values to be aggregated.
#' @param y A \code{raster::RasterLayer} or \code{terra::SpatRaster}
#'   object representing the spatial layer with the spatial resolution (and
#'   configuration) to aggregate (conform) to.
#' @param categories A numeric vector of all possible category values. If this
#'   is left \code{NULL} (default) the category values are extracted from the
#'   \code{x} layer via possible pre-existing categories (if present) or via
#'   calculating unique cell values (slower).
#' @param selected A numeric vector of selected category values for inclusion
#'   in the resultant aggregate layer. If this is left \code{NULL} (default) no
#'   categories will be selected.
#' @param binarize Logical indicating if the aggregated cells should be
#'   binarized, i.e. set to 1 for cells containing selected category values.
#'   Default = \code{TRUE}.
#' @param platform Logical indicating function is to be run in a platform
#'   environment requiring workaround code. Default = \code{FALSE}.
#' @param filename Optional file writing path (character).
#' @param ... Additional parameters (passed to \code{writeRaster}).
#' @return A \code{terra::SpatRaster} object containing an aggregated layer of
#'   selected categories.
#' @references Camac, J. & Baumgartner, J. (2021). \emph{edmaps} (early
#'   detection maps) : An R package for creating Australian maps of
#'   establishment likelihood for terrestrial plant pests.
#'   \url{https://github.com/jscamac/edmaps}.
#' @note Informed by various functions in
#'   \code{\href{edmaps}{https://github.com/jscamac/edmaps}}.
#' @include aggregate_layer.R
#' @include conform_layer.R
#' @export
aggregate_categories <- function(x, y,
                                 categories = NULL,
                                 selected = NULL,
                                 binarize = TRUE,
                                 platform = FALSE,
                                 filename = "", ...) {
  UseMethod("aggregate_categories")
}

#' @name aggregate_categories
#' @export
aggregate_categories.Raster <- function(x, y,
                                        categories = NULL,
                                        selected = NULL,
                                        binarize = TRUE,
                                        platform = FALSE,
                                        filename = "", ...) {
  # Call the terra version of the function
  aggregate_categories(terra::rast(x), y,
                       categories = categories,
                       selected = selected,
                       binarize = binarize,
                       platform = platform,
                       filename = filename, ...)
}

#' @name aggregate_categories
#' @export
aggregate_categories.SpatRaster <- function(x, y,
                                            categories = NULL,
                                            selected = NULL,
                                            binarize = TRUE,
                                            platform = FALSE,
                                            filename = "", ...) {
  # Convert y to terra
  if (class(y)[1] %in% c("Raster", "RasterStack", "RasterBrick")) {
    y <- terra::rast(y)
  }

  # Resolve categories when not present
  if (is.null(categories)) {
    if (terra::is.factor(x)) {
      categories <- terra::cats(x)[[1]][,1]
    } else {
      message("Resolving raster categories ...")
      categories <- sort(unname(unlist(terra::unique(x))))
    }
  }

  # Binarize selected categories of x
  message("Selecting raster categories ...")
  x_selected <- terra::classify(x, rcl = cbind(categories,
                                               categories %in% selected))

  # Aggregate and/or re-sample
  x_selected <- aggregate_layer(x_selected, y,
                                use_fun = ifelse(binarize, "max", "mean"),
                                use_method = ifelse(binarize, "near", "auto"),
                                platform = platform)

  # Conform
  x_selected <- conform_layer(x_selected, y,
                              use_method = ifelse(binarize, "near", "auto"),
                              filename = filename, ...)

  return(x_selected)
}
