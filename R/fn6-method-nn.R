#' Nearest neighbour surface module (internal, standardized module contract)
#' @keywords internal
run_nearest_neighbour <- function(obs, variable, template, mask, max_distance = Inf) {

  coords <- sf::st_coordinates(obs)
  values <- obs[[variable]]

  grid_coords <- terra::xyFromCell(template, which(!is.na(terra::values(mask))))

  nn_result <- apply(grid_coords, 1, function(pt) {
    d <- sqrt((coords[, 1] - pt[1])^2 + (coords[, 2] - pt[2])^2)
    idx <- which.min(d)
    c(value = values[idx], distance = d[idx], source_id = idx)
  })
  nn_result <- t(nn_result)

  preds <- nn_result[, "value"]
  dists <- nn_result[, "distance"]
  if (is.finite(max_distance)) preds[dists > max_distance] <- NA

  pred_rast <- template
  terra::values(pred_rast) <- NA
  pred_rast[which(!is.na(terra::values(mask)))] <- preds
  pred_rast <- terra::mask(pred_rast, mask)

  dist_rast <- template
  terra::values(dist_rast) <- NA
  dist_rast[which(!is.na(terra::values(mask)))] <- dists
  dist_rast <- terra::mask(dist_rast, mask)

  list(
    prediction       = pred_rast,
    uncertainty      = NULL,
    method_requested = "nearest_neighbour",
    method_used      = "nearest_neighbour",
    mode             = NA,
    model_fit        = NULL,
    candidates       = NULL,
    cv_results       = NULL,
    cv_summary       = NULL,
    parameters       = list(max_distance = max_distance),
    distance_raster  = dist_rast,
    warnings         = character(0),
    diagnostics      = list(n_obs = nrow(obs))
  )
}
