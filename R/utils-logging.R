#' Append a log entry to a cystimap object
#'
#' Internal helper used by every cystimap function to record what was
#' done, preserving a full processing history for reproducibility.
#'
#' @param x a cystimap object
#' @param step character, short description of the step (e.g. "prepare_study_area")
#' @param details a named list of relevant parameters/decisions to record
#' @keywords internal
log_step <- function(x, step, details = list()) {
  validate_cystimap(x)

  entry <- list(
    step      = step,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    details   = details
  )

  x$log[[length(x$log) + 1]] <- entry
  x
}
