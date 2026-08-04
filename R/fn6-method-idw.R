#' IDW surface module (internal, standardized module contract)
#' @keywords internal
run_idw <- function(obs, variable, template, mask, power = 2,
                    max_points = Inf, cv_method = "none", k = 5) {

  coords <- sf::st_coordinates(obs)
  values <- obs[[variable]]

  idw_predict <- function(train_coords, train_values, target_coords, power) {
    apply(target_coords, 1, function(pt) {
      d <- sqrt((train_coords[, 1] - pt[1])^2 + (train_coords[, 2] - pt[2])^2)
      if (any(d == 0)) return(train_values[which(d == 0)[1]])
      w <- 1 / (d ^ power)
      if (is.finite(max_points) && length(w) > max_points) {
        keep <- order(d)[seq_len(max_points)]
        w <- w[keep]; vals <- train_values[keep]
      } else {
        vals <- train_values
      }
      sum(w * vals) / sum(w)
    })
  }

  #predict across the raster template
  cell_coords <- terra::xyFromCell(template, which(!is.na(terra::values(mask))))
  preds <- idw_predict(coords, values, cell_coords, power)

  pred_rast <- template
  terra::values(pred_rast) <- NA
  pred_rast[which(!is.na(terra::values(mask)))] <- preds
  pred_rast <- terra::mask(pred_rast, mask)

  # cross-validation
  cv_results <- NULL
  if (cv_method != "none") {
    n <- nrow(coords)
    folds <- make_cv_folds(n, method = cv_method, k = k)
    cv_out <- lapply(unique(folds), function(f) {
      test_idx <- which(folds == f)
      train_idx <- setdiff(seq_len(n), test_idx)
      pred_vals <- idw_predict(coords[train_idx, , drop = FALSE], values[train_idx],
                               coords[test_idx, , drop = FALSE], power)
      data.frame(fold = f, observed = values[test_idx], predicted = pred_vals)
    })
    cv_results <- do.call(rbind, cv_out)
  }

  list(
    prediction       = pred_rast,
    uncertainty      = NULL,
    method_requested = "idw",
    method_used      = "idw",
    mode             = NA,
    model_fit        = list(power = power, max_points = max_points),
    candidates       = NULL,
    cv_results       = cv_results,
    cv_summary       = if (!is.null(cv_results)) summarise_cv(cv_results) else NULL,
    parameters       = list(power = power, max_points = max_points),
    warnings         = character(0),
    diagnostics      = list(n_obs = nrow(obs))
  )
}
