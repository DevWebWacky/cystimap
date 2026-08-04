#' Create indicators, attach locations, and aggregate observations
#'
#' Converts a raw dataset already attached to a cystimap object (via
#' `read_data()`) into an analysis-ready indicator, aggregated to one
#' value per spatial location. Combines what would otherwise be three
#' steps (indicator creation, location attachment, aggregation) into a
#' single traceable call.
#'
#' @param x a cystimap object
#' @param data_name character, name of the dataset in `x$data` to use
#'   (as attached by `read_data()`)
#' @param indicator_name character, name to store the resulting
#'   indicator under (in `x$data$indicators`)
#' @param formula an expression defining the indicator, evaluated
#'   within the dataset, e.g. `cases / population * 1000`. Use
#'   `variable` names as columns exist in the raw dataset.
#' @param type character, one of "binary", "continuous", "ordinal",
#'   "categorical", "probability", "percentage", "ratio", "rate",
#'   "index". Purely descriptive/metadata — does not change computation
#'   unless `standardise = TRUE`.
#' @param standardise logical, should the resulting indicator be
#'   rescaled to 0-1? Default FALSE.
#' @param coords optional character vector of length 2, column names
#'   for longitude/latitude (or x/y) if attaching by coordinates.
#' @param join_key optional character, column name to join locations
#'   by by (e.g. district ID) if attaching via a lookup instead of
#'   coordinates. If supplied, `location_lookup` must also be supplied.
#' @param location_lookup optional sf object (points or polygons) with
#'   a matching join column, used when `join_key` is supplied.
#' @param aggregate_by character, column name defining the grouping
#'   location for aggregation (e.g. district ID, or a coordinate-derived
#'   ID). Required if more than one observation shares a location.
#' @param agg_fun character, aggregation method: one of "mean",
#'   "median", "sum", "count", "min", "max", "weighted_mean",
#'   "weighted_sum", "proportion", "sd". Default "mean".
#' @param weights optional character, column name of weights, required
#'   if `agg_fun` is "weighted_mean" or "weighted_sum".
#' @param coords_crs CRS that `coords` are recorded in (e.g. 4326 for
#'   standard GPS lon/lat). If different from the study area's
#'   analytical CRS, coordinates will be transformed automatically.
#'
#' @return the cystimap object `x`, with the aggregated spatial
#'   indicator added to `x$data$indicators[[indicator_name]]`
#' @export
create_indicator <- function(x,
                             data_name,
                             indicator_name,
                             formula,
                             type = "continuous",
                             standardise = FALSE,
                             coords = NULL,
                             coords_crs = 4326,
                             join_key = NULL,
                             location_lookup = NULL,
                             aggregate_by = NULL,
                             agg_fun = c("mean", "median", "sum", "count", "min", "max",
                                         "weighted_mean", "weighted_sum", "proportion", "sd"),
                             weights = NULL) {

  validate_cystimap(x)
  agg_fun <- match.arg(agg_fun)

  if (is.null(x$data[[data_name]])) {
    stop("Dataset '", data_name, "' not found in x$data. ",
         "Did you run read_data() first?", call. = FALSE)
  }
  raw <- x$data[[data_name]]

  #Create the indicator variable
  formula_expr <- substitute(formula)
  raw$.indicator_value <- eval(formula_expr, envir = raw, enclos = parent.frame())

  if (isTRUE(standardise)) {
    rng <- range(raw$.indicator_value, na.rm = TRUE)
    raw$.indicator_value <- (raw$.indicator_value - rng[1]) / (rng[2] - rng[1])
  }

  #Attach spatial locations
  spatial_data <- attach_locations_internal(raw, coords, join_key, location_lookup, x$crs, coords_crs)

  #Aggregate to one value per location
  if (!is.null(aggregate_by)) {
    aggregated <- aggregate_observations_internal(
      spatial_data, group_col = aggregate_by, value_col = ".indicator_value",
      agg_fun = agg_fun, weights = weights
    )
  } else {
    aggregated <- spatial_data
    names(aggregated)[names(aggregated) == ".indicator_value"] <- indicator_name
  }

  if (".indicator_value" %in% names(aggregated)) {
    names(aggregated)[names(aggregated) == ".indicator_value"] <- indicator_name
  }

  #Store
  if (is.null(x$data$indicators)) x$data$indicators <- list()
  x$data$indicators[[indicator_name]] <- aggregated

  x <- log_step(x, "create_indicator", details = list(
    data_name       = data_name,
    indicator_name  = indicator_name,
    type            = type,
    standardise     = standardise,
    aggregate_by    = aggregate_by,
    agg_fun         = if (!is.null(aggregate_by)) agg_fun else NA,
    n_locations     = nrow(aggregated)
  ))

  x
}

#' Internal: attach coordinates or join-based locations
#' @keywords internal
attach_locations_internal <- function(data, coords, join_key, location_lookup, crs, coords_crs = 4326) {

  if (!is.null(coords)) {
    if (length(coords) != 2) stop("`coords` must be length 2, e.g. c('lon','lat').", call. = FALSE)
    missing_coords <- is.na(data[[coords[1]]]) | is.na(data[[coords[2]]])
    if (any(missing_coords)) {
      warning(sum(missing_coords), " row(s) dropped due to missing coordinates.", call. = FALSE)
      data <- data[!missing_coords, ]
    }
    sf_data <- sf::st_as_sf(data, coords = coords, crs = coords_crs, remove = FALSE)
    sf_data <- sf::st_transform(sf_data, crs)   # <-- transform to analytical CRS
    return(sf_data)
  }

  if (!is.null(join_key)) {
    if (is.null(location_lookup)) {
      stop("`location_lookup` must be supplied when using `join_key`.", call. = FALSE)
    }
    if (!(join_key %in% names(data)) || !(join_key %in% names(location_lookup))) {
      stop("`join_key` column '", join_key, "' not found in both data and location_lookup.", call. = FALSE)
    }
    merged <- merge(location_lookup, data, by = join_key, all.x = FALSE)
    unmatched <- nrow(data) - nrow(merged)
    if (unmatched > 0) {
      warning(unmatched, " row(s) in data had no match in location_lookup and were dropped.", call. = FALSE)
    }
    return(sf::st_as_sf(merged))
  }

  stop("Either `coords` or `join_key` + `location_lookup` must be supplied to attach locations.", call. = FALSE)
}

#' Internal: aggregate one value per location
#' @keywords internal
aggregate_observations_internal <- function(sf_data, group_col, value_col, agg_fun, weights = NULL) {

  if (!(group_col %in% names(sf_data))) {
    stop("`aggregate_by` column '", group_col, "' not found in the data.", call. = FALSE)
  }

  df <- sf::st_drop_geometry(sf_data)
  groups <- split(seq_len(nrow(df)), df[[group_col]])

  agg_one <- function(idx) {
    vals <- df[[value_col]][idx]
    w <- if (!is.null(weights)) df[[weights]][idx] else NULL

    result <- switch(agg_fun,
                     mean          = mean(vals, na.rm = TRUE),
                     median        = stats::median(vals, na.rm = TRUE),
                     sum           = sum(vals, na.rm = TRUE),
                     count         = length(stats::na.omit(vals)),
                     min           = min(vals, na.rm = TRUE),
                     max           = max(vals, na.rm = TRUE),
                     sd            = stats::sd(vals, na.rm = TRUE),
                     proportion    = mean(vals, na.rm = TRUE),
                     weighted_mean = stats::weighted.mean(vals, w, na.rm = TRUE),
                     weighted_sum  = sum(vals * w, na.rm = TRUE)
    )
    data.frame(group = idx[1], value = result, n = length(idx))
  }

  agg_list <- lapply(groups, function(idx) agg_one(idx))
  agg_df <- do.call(rbind, agg_list)
  agg_df[[group_col]] <- names(groups)
  agg_df$group <- NULL
  names(agg_df)[names(agg_df) == "value"] <- ".indicator_value"

  # take first geometry per group (assumes locations are consistent within group)
  first_idx <- sapply(groups, `[`, 1)
  geoms <- sf::st_geometry(sf_data)[first_idx]

  out <- sf::st_sf(agg_df, geometry = geoms)
  out
}
