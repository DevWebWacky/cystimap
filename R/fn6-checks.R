#' Validate observations before surface modelling
#'
#' Checks spatial validity, CRS, missing values, duplicates, and
#' whether points fall inside the study area. Never guesses a CRS.
#'
#' @param obs an sf point object
#' @param variable character, column name of the response variable
#' @param study_area sf object, the analytical study area
#' @param crs the analytical CRS (from the cystimap object)
#' @keywords internal
check_observations <- function(obs, variable, study_area, crs) {

  issues <- list()
  excluded <- data.frame()

  if (!inherits(obs, "sf")) {
    stop("Observations must be an sf point object. If you have raw ",
         "coordinates, attach them first with create_indicator(coords = ...).",
         call. = FALSE)
  }
  if (!all(sf::st_geometry_type(obs) %in% c("POINT"))) {
    stop("Observations must be POINT geometries.", call. = FALSE)
  }
  if (is.na(sf::st_crs(obs))) {
    stop("Observations have no CRS. cystimap will not guess a CRS ",
         "— please supply one explicitly.", call. = FALSE)
  }
  if (sf::st_crs(obs) != crs) {
    obs <- sf::st_transform(obs, crs)
  }
  if (!(variable %in% names(obs))) {
    stop("Variable '", variable, "' not found in observations.", call. = FALSE)
  }

  # missing response values
  missing_val <- is.na(obs[[variable]])
  if (any(missing_val)) {
    excluded <- rbind(excluded, data.frame(
      row = which(missing_val), reason = "missing response value"
    ))
    issues$n_missing_value <- sum(missing_val)
  }

  # missing/invalid geometry
  missing_geom <- sf::st_is_empty(obs)
  if (any(missing_geom)) {
    excluded <- rbind(excluded, data.frame(
      row = which(missing_geom), reason = "missing/invalid location"
    ))
    issues$n_missing_geom <- sum(missing_geom)
  }

  drop_idx <- unique(excluded$row)
  clean <- if (length(drop_idx) > 0) obs[-drop_idx, ] else obs

  # duplicate coordinates
  coords <- sf::st_coordinates(clean)
  dup <- duplicated(coords)
  issues$n_duplicates <- sum(dup)

  # points outside study area
  inside <- sf::st_intersects(clean, sf::st_union(study_area), sparse = FALSE)[, 1]
  issues$n_outside_area <- sum(!inside)
  if (any(!inside)) {
    warning(sum(!inside), " observation(s) fall outside the study area boundary. ",
            "They will still be used for modelling unless you filter them.", call. = FALSE)
  }

  # spatial coverage / sample size warnings
  n_valid <- nrow(clean)
  if (n_valid < 10) {
    warning("Only ", n_valid, " valid observation(s) available. Interpolation ",
            "reliability is likely to be poor with this few points.", call. = FALSE)
  }
  if (length(unique(clean[[variable]])) == 1) {
    warning("The response variable is constant across all observations , ",
            "spatial interpolation is not meaningful.", call. = FALSE)
  }

  list(
    clean       = clean,
    excluded    = excluded,
    issues      = issues,
    n_valid     = n_valid
  )
}
