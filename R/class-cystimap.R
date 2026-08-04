#' Create a new cystimap object
#'
#' Internal constructor for the cystimap S3 class. Not usually called
#' directly by users, use `prepare_study_area()` to start a workflow.
#'
#' @param ... named elements to include in the object
#' @return an object of class "cystimap"
#' @keywords internal
new_cystimap <- function(...) {
  fields <- list(...)

  default_fields <- list(
    study_area = NULL,
    template   = NULL,
    mask       = NULL,
    grid       = NULL,
    exclusions = NULL,
    data       = list(),
    surfaces   = list(),
    classified = list(),
    scenarios  = list(),
    results    = list(),
    crs        = NULL,
    log        = list(),
    meta       = list(created = Sys.time(), package_version = utils::packageVersion("cystimap"))
  )

  # overwrite defaults with whatever was passed in
  default_fields[names(fields)] <- fields

  structure(default_fields, class = "cystimap")
}

#' Check if an object is a cystimap object
#' @param x object to test
#' @export
is_cystimap <- function(x) inherits(x, "cystimap")

#' Validate a cystimap object
#' @param x a cystimap object
#' @keywords internal
validate_cystimap <- function(x) {
  if (!is_cystimap(x)) {
    stop("Object is not of class 'cystimap'.", call. = FALSE)
  }
  invisible(x)
}

#' Print method for cystimap objects
#' @param x a cystimap object
#' @param ... additional arguments (unused)
#' @export
print.cystimap <- function(x, ...) {
  cat("<cystimap object>\n")
  cat("  CRS:        ", if (is.null(x$crs)) "not set" else x$crs$input, "\n")
  cat("  Study area: ", if (is.null(x$study_area)) "not set" else "set", "\n")
  cat("  Template:   ", if (is.null(x$template)) "not set" else "set", "\n")
  cat("  Surfaces:   ", length(x$surfaces), "\n")
  cat("  Classified: ", length(x$classified), "\n")
  cat("  Scenarios:  ", length(x$scenarios), "\n")
  cat("  Log entries:", length(x$log), "\n")
  cat("  Datasets:   ", length(x$data), "\n")
  invisible(x)
}

#' Summary method for cystimap objects
#' @param object a cystimap object
#' @param ... additional arguments (unused)
#' @export
summary.cystimap <- function(object, ...) {
  print(object)
  if (length(object$log) > 0) {
    cat("\nRecent processing history:\n")
    n <- length(object$log)
    show <- object$log[max(1, n - 4):n]
    for (entry in show) {
      cat(" -", entry$timestamp, ":", entry$step, "\n")
    }
  }
  invisible(object)
}
