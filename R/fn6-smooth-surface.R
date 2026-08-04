#' Create a smooth spatial surface from point observations
#'
#' Converts spatial observations into a continuous raster surface, or
#' validates/prepares a user-supplied existing raster. Acts as a
#' coordinating engine over standardized method modules (ordinary
#' kriging, IDW, nearest neighbour), so results share a consistent
#' structure regardless of method.
#'
#' @param x a cystimap object with a study area already prepared
#' @param indicator_name character, name of the indicator in
#'   `x$data$indicators` to model (as produced by `create_indicator()`)
#' @param surface_name character, name to store the result under in
#'   `x$surfaces`
#' @param method one of "ordinary_kriging", "idw", "nearest_neighbour",
#'   or "existing_raster"
#' @param mode one of "automatic" or "manual". Ignored for idw,
#'   nearest_neighbour, and existing_raster (their parameters are
#'   always explicit/manual by nature).
#' @param existing_raster for method = "existing_raster": a SpatRaster
#'   or file path to an already-complete surface
#' @param cv_method cross-validation method: "none", "loo", or "kfold"
#' @param k number of folds if cv_method = "kfold"
#' @param ... method-specific parameters passed through (e.g. `power`
#'   for idw, `manual_model` for kriging, `max_distance` for nearest
#'   neighbour, `resample_method` for existing_raster)
#'
#' @return the cystimap object `x`, with the surface result stored in
#'   `x$surfaces[[surface_name]]`
#' @export
smooth_surface <- function(x, indicator_name, surface_name,
                           method = c("ordinary_kriging", "idw", "nearest_neighbour", "existing_raster"),
                           mode = c("automatic", "manual"),
                           existing_raster = NULL,
                           cv_method = c("none", "loo", "kfold"),
                           k = 5,
                           ...) {

  validate_cystimap(x)
  method <- match.arg(method)
  mode <- match.arg(mode)
  cv_method <- match.arg(cv_method)

  if (is.null(x$template) || is.null(x$mask)) {
    stop("No raster template/mask found. Run prepare_study_area() first.", call. = FALSE)
  }

  # ---- existing raster pathway short-circuits everything else ----
  if (method == "existing_raster") {
    if (is.null(existing_raster)) {
      stop("`existing_raster` must be supplied when method = 'existing_raster'.", call. = FALSE)
    }
    result <- prepare_existing_raster(existing_raster, x$template, x$mask, x$crs, ...)

  } else {
    if (is.null(x$data$indicators[[indicator_name]])) {
      stop("Indicator '", indicator_name, "' not found. Did you run create_indicator()?", call. = FALSE)
    }
    obs_raw <- x$data$indicators[[indicator_name]]
    var_col <- indicator_name

    checked <- check_observations(obs_raw, var_col, x$study_area, x$crs)
    obs <- checked$clean

    result <- switch(method,
                     idw = run_idw(obs, var_col, x$template, x$mask, cv_method = cv_method, k = k, ...),
                     ordinary_kriging = run_ordinary_kriging(obs, var_col, x$template, x$mask, mode = mode,
                                                             cv_method = cv_method, k = k, ...),
                     nearest_neighbour = run_nearest_neighbour(obs, var_col, x$template, x$mask, ...)
    )

    result$excluded_obs <- checked$excluded
    result$data_issues  <- checked$issues
  }

  if (!is.null(result$cv_summary) && !is.na(result$cv_summary$correlation)) {
    if (result$cv_summary$correlation < 0.2) {
      warning(
        "Cross-validated correlation is low (", round(result$cv_summary$correlation, 2),
        "). This suggests weak spatial structure in the data, interpret this ",
        "surface with caution.", call. = FALSE
      )
    }
  }

  x$surfaces[[surface_name]] <- result

  x <- log_step(x, "smooth_surface", details = list(
    surface_name     = surface_name,
    method_requested = result$method_requested,
    method_used      = result$method_used,
    mode             = mode,
    cv_method        = cv_method,
    n_obs            = if (!is.null(result$diagnostics$n_obs)) result$diagnostics$n_obs else NA
  ))

  x
}
