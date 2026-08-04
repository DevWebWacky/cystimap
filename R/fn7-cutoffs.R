#' Calculate cut-off values from a raster using the requested method
#'
#' @param values numeric vector of valid (non-NA) raster cell values
#' @param method one of "fixed", "quantile", "equal_interval", "jenks",
#'   "fisher", "geometric_interval", "mean_sd", "headtail", "custom"
#' @param cutoffs user-supplied cut-offs (required for "fixed"; used as
#'   quantile probabilities for "quantile"; ignored otherwise)
#' @param n_classes number of classes desired (used for methods that
#'   calculate breaks automatically)
#' @param sd_multiplier multiplier for the mean_sd method (default 1)
#' @keywords internal
calculate_cutoffs <- function(values, method, cutoffs = NULL, n_classes = 2,
                              sd_multiplier = 1) {

  method <- match.arg(method, c("fixed", "quantile", "equal_interval", "jenks",
                                "fisher", "geometric_interval", "mean_sd",
                                "headtail", "custom"))

  result <- switch(method,

                   fixed = {
                     if (is.null(cutoffs)) stop("`cutoffs` must be supplied for method = 'fixed'.", call. = FALSE)
                     breaks <- sort(unique(c(min(values, na.rm = TRUE), cutoffs, max(values, na.rm = TRUE))))
                     list(breaks = breaks, requested = cutoffs, note = "user-supplied fixed values")
                   },

                   quantile = {
                     probs <- if (!is.null(cutoffs)) cutoffs else seq(0, 1, length.out = n_classes + 1)
                     breaks <- stats::quantile(values, probs = probs, na.rm = TRUE, names = FALSE)
                     # always bound with actual min/max so classify() covers the full range
                     breaks <- sort(unique(c(min(values, na.rm = TRUE), breaks, max(values, na.rm = TRUE))))
                     list(breaks = breaks, requested = probs, note = "calculated from percentiles")
                   },

                   equal_interval = {
                     breaks <- seq(min(values, na.rm = TRUE), max(values, na.rm = TRUE), length.out = n_classes + 1)
                     list(breaks = breaks, requested = n_classes, note = "equal-width intervals")
                   },

                   jenks = {
                     if (!requireNamespace("classInt", quietly = TRUE)) {
                       stop("Jenks classification requires the 'classInt' package.", call. = FALSE)
                     }
                     cls <- classInt::classIntervals(values, n = n_classes, style = "jenks")
                     list(breaks = cls$brks, requested = n_classes, note = "Jenks natural breaks")
                   },

                   fisher = {
                     if (!requireNamespace("classInt", quietly = TRUE)) {
                       stop("Fisher classification requires the 'classInt' package.", call. = FALSE)
                     }
                     cls <- classInt::classIntervals(values, n = n_classes, style = "fisher")
                     list(breaks = cls$brks, requested = n_classes, note = "Fisher-Jenks natural breaks")
                   },

                   geometric_interval = {
                     if (!requireNamespace("classInt", quietly = TRUE)) {
                       stop("Geometric interval classification requires the 'classInt' package.", call. = FALSE)
                     }
                     cls <- classInt::classIntervals(values, n = n_classes, style = "geometric")
                     list(breaks = cls$brks, requested = n_classes, note = "geometric intervals")
                   },

                   mean_sd = {
                     m <- mean(values, na.rm = TRUE)
                     s <- stats::sd(values, na.rm = TRUE)
                     breaks <- m + seq(-2, 2, by = sd_multiplier) * s
                     breaks <- sort(unique(c(min(values, na.rm = TRUE), breaks, max(values, na.rm = TRUE))))
                     list(breaks = breaks, requested = sd_multiplier, note = "mean +/- SD bands")
                   },

                   headtail = {
                     # repeatedly split values above the mean (head/tail breaks for skewed data)
                     breaks <- min(values, na.rm = TRUE)
                     remaining <- values[values > mean(values, na.rm = TRUE)]
                     current_mean <- mean(values, na.rm = TRUE)
                     breaks <- c(breaks, current_mean)
                     iter <- 0
                     while (length(remaining) / length(values) > 0.4 && iter < 10 && length(remaining) > 1) {
                       current_mean <- mean(remaining, na.rm = TRUE)
                       breaks <- c(breaks, current_mean)
                       remaining <- remaining[remaining > current_mean]
                       iter <- iter + 1
                     }
                     breaks <- sort(unique(c(breaks, max(values, na.rm = TRUE))))
                     list(breaks = breaks, requested = NA, note = paste0("head/tail breaks (", iter, " iterations)"))
                   },

                   custom = {
                     if (!is.function(cutoffs)) stop("For method = 'custom', `cutoffs` must be a function(values) returning breaks.", call. = FALSE)
                     breaks <- cutoffs(values)
                     list(breaks = sort(breaks), requested = "custom function", note = "user-supplied custom rule")
                   }
  )

  result
}
