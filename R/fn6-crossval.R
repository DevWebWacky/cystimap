#' Build cross-validation fold assignments
#' @param n integer, number of observations
#' @param method "loo" or "kfold"
#' @param k number of folds (used only if method = "kfold")
#' @keywords internal
make_cv_folds <- function(n, method = c("loo", "kfold"), k = 5) {
  method <- match.arg(method)
  if (method == "loo") {
    return(seq_len(n))
  }
  sample(rep(seq_len(k), length.out = n))
}

#' Summarise cross-validation results
#' @param cv_df data.frame with columns observed, predicted
#' @keywords internal
summarise_cv <- function(cv_df) {
  resid <- cv_df$observed - cv_df$predicted
  data.frame(
    rmse = sqrt(mean(resid^2, na.rm = TRUE)),
    mae  = mean(abs(resid), na.rm = TRUE),
    bias = mean(resid, na.rm = TRUE),
    correlation = suppressWarnings(stats::cor(cv_df$observed, cv_df$predicted, use = "complete.obs"))
  )
}
