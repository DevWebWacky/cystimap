#' Detect and validate CRS, never guessing
#'
#' @param x an sf or SpatVector object
#' @param crs optional user-supplied CRS (proj4string, EPSG code, or WKT)
#' @keywords internal
resolve_crs <- function(x, crs = NULL) {
  existing_crs <- tryCatch(sf::st_crs(x), error = function(e) NA)

  if (!is.null(crs)) {
    # user explicitly supplied a CRS -> respect it
    return(sf::st_crs(crs))
  }

  if (is.na(existing_crs) || is.null(existing_crs$input)) {
    stop(
      "CRS could not be determined automatically. ",
      "Please supply one explicitly via the `crs` argument. ",
      "cystimap will never guess a CRS.",
      call. = FALSE
    )
  }

  existing_crs
}

#' Validate and repair sf geometries
#' @param x an sf object
#' @keywords internal
repair_geometry <- function(x) {
  x <- sf::st_make_valid(x)
  x <- x[!sf::st_is_empty(x), ]
  x
}
