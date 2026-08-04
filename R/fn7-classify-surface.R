#' Classify a continuous raster surface into discrete classes
#'
#' Converts one or more continuous surfaces (from `smooth_surface()`)
#' into binary, multiclass, ordinal, categorical, probability, or rank
#' classes — or applies a display-only continuous gradient. Supports
#' sensitivity mode (multiple cut-offs -> separate binary rasters) and
#' combined mode (multiple cut-offs -> one multiclass raster).
#'
#' @param x a cystimap object
#' @param surface_name character, name of the surface in `x$surfaces`
#'   to classify (as produced by `smooth_surface()`)
#' @param classified_name character, name to store the result under in
#'   `x$classified`
#' @param type one of "binary", "multiclass", "ordinal", "categorical",
#'   "continuous_display", "probability", "rank"
#' @param cutoff_method one of "fixed", "quantile", "equal_interval",
#'   "jenks", "fisher", "geometric_interval", "mean_sd", "headtail",
#'   "custom"
#' @param cutoffs cut-off values (meaning depends on `cutoff_method` —
#'   see `calculate_cutoffs()`). Not required for `continuous_display`.
#' @param n_classes number of classes to calculate, for methods that
#'   derive breaks automatically (jenks, fisher, equal_interval, etc.)
#' @param sensitivity_mode logical. If multiple cut-offs are supplied
#'   and this is TRUE, produce separate binary rasters (one per
#'   cut-off) instead of one combined multiclass raster. Default FALSE.
#' @param labels optional character vector of class labels, must match
#'   the number of resulting classes.
#' @param colours optional character vector of colours (hex or R colour
#'   names), must match the number of resulting classes.
#'
#' @return the cystimap object `x`, with the result(s) stored in
#'   `x$classified[[classified_name]]`
#' @export
classify_surface <- function(x, surface_name, classified_name,
                             type = c("binary", "multiclass", "ordinal", "categorical",
                                      "continuous_display", "probability", "rank"),
                             cutoff_method = c("fixed", "quantile", "equal_interval", "jenks",
                                               "fisher", "geometric_interval", "mean_sd",
                                               "headtail", "custom"),
                             cutoffs = NULL,
                             n_classes = 2,
                             sensitivity_mode = FALSE,
                             labels = NULL,
                             colours = NULL) {

  validate_cystimap(x)
  type <- match.arg(type)

  if (is.null(x$surfaces[[surface_name]])) {
    stop("Surface '", surface_name, "' not found. Did you run smooth_surface()?", call. = FALSE)
  }
  rast <- x$surfaces[[surface_name]]$prediction
  if (!inherits(rast, "SpatRaster")) {
    stop("Surface '", surface_name, "' does not contain a valid prediction raster.", call. = FALSE)
  }

  vals <- terra::values(rast, na.rm = TRUE)
  if (length(vals) == 0) {
    stop("Surface '", surface_name, "' has no valid (non-NA) values to classify.", call. = FALSE)
  }

  #continuous display: no reclassification, just pass through
  if (type == "continuous_display") {
    result <- list(
      rasters        = list(display = rast),
      surface_name   = surface_name,
      type           = type,
      cutoff_method  = NA,
      cutoffs_table  = NULL,
      labels         = NULL,
      colours        = colours,
      frequencies    = NULL,
      area_summary   = NULL,
      warnings       = character(0)
    )
    x$classified[[classified_name]] <- result
    x <- log_step(x, "classify_surface", details = list(
      classified_name = classified_name, surface_name = surface_name, type = "continuous_display"
    ))
    return(x)
  }

  cutoff_method <- match.arg(cutoff_method)
  warn_msgs <- character(0)

  #calculate cut-offs
  cut_result <- calculate_cutoffs(vals, method = cutoff_method, cutoffs = cutoffs,
                                  n_classes = n_classes)
  breaks <- cut_result$breaks

  if (length(breaks) < 2) {
    stop("Cut-off calculation produced fewer than 2 breakpoints — cannot classify.", call. = FALSE)
  }

  #decide separate (sensitivity) vs combined mode
  n_cutpoints <- length(breaks) - 2  # interior cut-points, excluding min/max bounds
  use_separate <- isTRUE(sensitivity_mode) && n_cutpoints >= 1

  classify_one <- function(rast, breaks) {
    class_codes <- seq_along(breaks[-1]) - 1   # 0-indexed: 0,1 for binary; 0,1,2... for multiclass
    rc_matrix <- cbind(breaks[-length(breaks)], breaks[-1], class_codes)
    rc_matrix[nrow(rc_matrix), 2] <- rc_matrix[nrow(rc_matrix), 2] + 1e-9  # include max value
    terra::classify(rast, rc_matrix, include.lowest = TRUE)
  }

  rasters_out <- list()

  if (use_separate) {
    # one binary raster per interior cut-point
    interior_cuts <- breaks[-c(1, length(breaks))]
    for (i in seq_along(interior_cuts)) {
      cp <- interior_cuts[i]
      bin_rast <- terra::classify(rast, cbind(-Inf, cp, 0))
      bin_rast <- terra::classify(bin_rast, cbind(cp, Inf, 1))
      rasters_out[[paste0("cutoff_", round(cp, 3))]] <- bin_rast
    }
  } else {
    rasters_out[["combined"]] <- classify_one(rast, breaks)
  }

  #validate labels/colours
  n_final_classes <- if (use_separate) 2 else (length(breaks) - 1)
  if (!is.null(labels) && length(labels) != n_final_classes) {
    warn_msgs <- c(warn_msgs, paste0(
      "Supplied `labels` (n=", length(labels), ") do not match the number of ",
      "classes (n=", n_final_classes, "). Labels were ignored."
    ))
    labels <- NULL
  }
  if (!is.null(colours) && length(colours) != n_final_classes) {
    warn_msgs <- c(warn_msgs, paste0(
      "Supplied `colours` (n=", length(colours), ") do not match the number of ",
      "classes (n=", n_final_classes, "). Colours were ignored."
    ))
    colours <- NULL
  }
  for (w in warn_msgs) warning(w, call. = FALSE)

  # frequencies + area/percentage summaries
  cell_area <- prod(terra::res(rast))
  freq_list <- lapply(names(rasters_out), function(nm) {
    f <- terra::freq(rasters_out[[nm]])
    f$area <- f$count * cell_area
    f$percentage <- 100 * f$count / sum(f$count)
    f$raster <- nm
    f
  })
  frequencies <- do.call(rbind, freq_list)

  # warn on empty classes
  if (any(frequencies$count == 0)) {
    warning("One or more classes contain zero cells (empty class) for surface '",
            surface_name, "'.", call. = FALSE)
  }

  result <- list(
    rasters        = rasters_out,
    surface_name   = surface_name,
    type           = type,
    cutoff_method  = cutoff_method,
    cutoffs_requested = cut_result$requested,
    cutoffs_table  = data.frame(break_value = breaks),
    mode           = if (use_separate) "separate" else "combined",
    labels         = labels,
    colours        = colours,
    frequencies    = frequencies,
    warnings       = warn_msgs
  )

  x$classified[[classified_name]] <- result

  x <- log_step(x, "classify_surface", details = list(
    classified_name = classified_name,
    surface_name    = surface_name,
    type            = type,
    cutoff_method   = cutoff_method,
    mode            = result$mode,
    n_classes       = n_final_classes
  ))

  x
}
