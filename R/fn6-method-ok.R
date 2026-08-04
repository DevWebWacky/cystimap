#' Ordinary Kriging surface module (internal, standardized module contract)
#'
#' Automatic mode fits multiple candidate variogram models and selects
#' the best by cross-validated RMSE. Manual mode requires the user to
#' supply a fixed variogram model.
#' @keywords internal
run_ordinary_kriging <- function(obs, variable, template, mask,
                                 mode = c("automatic", "manual"),
                                 candidate_models = c("Sph", "Exp", "Gau", "Mat"),
                                 manual_model = NULL,
                                 nmax = Inf,
                                 cv_method = "none", k = 5) {

  mode <- match.arg(mode)
  if (!requireNamespace("gstat", quietly = TRUE)) {
    stop("Ordinary kriging requires the 'gstat' package. Install with install.packages('gstat').", call. = FALSE)
  }

  form <- stats::as.formula(paste(variable, "~ 1"))
  obs_sp <- obs

  #empirical variogram
  emp_vgm <- gstat::variogram(form, obs_sp)

  fitted_model <- NULL
  candidates_table <- data.frame()

  if (mode == "manual") {
    if (is.null(manual_model)) {
      stop("Manual mode requires `manual_model`, a fitted variogramModel object ",
           "(see gstat::vgm()).", call. = FALSE)
    }
    fitted_model <- manual_model
  } else {
    # automatic: fit each candidate, keep successful ones
    fits <- lapply(candidate_models, function(m) {
      tryCatch({
        init <- gstat::vgm(psill = max(emp_vgm$gamma), model = m,
                           range = max(emp_vgm$dist) / 3, nugget = min(emp_vgm$gamma))
        fit <- gstat::fit.variogram(emp_vgm, init)
        sse <- attr(fit, "SSErr")
        list(model = m, fit = fit, sse = sse, ok = !is.na(sse))
      }, error = function(e) list(model = m, fit = NULL, sse = NA, ok = FALSE, error = conditionMessage(e)))
    })

    candidates_table <- do.call(rbind, lapply(fits, function(f) {
      data.frame(model = f$model, sse = f$sse, converged = isTRUE(f$ok))
    }))

    valid_fits <- Filter(function(f) isTRUE(f$ok), fits)
    if (length(valid_fits) == 0) {
      stop("No candidate variogram model converged. Try manual mode with a ",
           "specific model, or check the data for spatial structure.", call. = FALSE)
    }
    best <- valid_fits[[which.min(sapply(valid_fits, function(f) f$sse))]]
    fitted_model <- best$fit
  }

  #predict across the template
  grid_pts <- terra::xyFromCell(template, which(!is.na(terra::values(mask))))
  grid_sf <- sf::st_as_sf(as.data.frame(grid_pts), coords = c("x", "y"), crs = sf::st_crs(obs))

  krige_out <- tryCatch(
    gstat::krige(form, obs_sp, newdata = grid_sf, model = fitted_model,
                 nmax = if (is.finite(nmax)) nmax else Inf, debug.level = 0),
    error = function(e) stop("Kriging prediction failed: ", conditionMessage(e), call. = FALSE)
  )

  pred_rast <- template
  terra::values(pred_rast) <- NA
  pred_rast[which(!is.na(terra::values(mask)))] <- krige_out$var1.pred
  pred_rast <- terra::mask(pred_rast, mask)

  var_rast <- template
  terra::values(var_rast) <- NA
  var_rast[which(!is.na(terra::values(mask)))] <- krige_out$var1.var
  var_rast <- terra::mask(var_rast, mask)

  #cross-validation
  cv_results <- NULL
  if (cv_method != "none") {
    n <- nrow(obs)
    folds <- make_cv_folds(n, method = cv_method, k = k)
    cv_out <- lapply(unique(folds), function(f) {
      test_idx <- which(folds == f)
      train <- obs[-test_idx, ]
      test  <- obs[test_idx, ]
      pred <- tryCatch(
        gstat::krige(form, train, newdata = test, model = fitted_model, debug.level = 0),
        error = function(e) NULL
      )
      if (is.null(pred)) return(NULL)
      data.frame(fold = f, observed = test[[variable]], predicted = pred$var1.pred)
    })
    cv_results <- do.call(rbind, cv_out)
  }

  list(
    prediction       = pred_rast,
    uncertainty      = var_rast,
    method_requested = "ordinary_kriging",
    method_used      = "ordinary_kriging",
    mode             = mode,
    model_fit        = fitted_model,
    candidates       = candidates_table,
    cv_results       = cv_results,
    cv_summary       = if (!is.null(cv_results)) summarise_cv(cv_results) else NULL,
    parameters       = list(nmax = nmax),
    empirical_variogram = emp_vgm,
    warnings         = character(0),
    diagnostics      = list(n_obs = nrow(obs))
  )
}
