#' Recursively wrap SpatRasters inside a plain list structure (internal)
#' @keywords internal
wrap_recursive <- function(obj) {
  if (inherits(obj, "SpatRaster")) return(terra::wrap(obj))
  if (is.list(obj) && is.null(attr(obj, "class"))) {
    return(lapply(obj, wrap_recursive))
  }
  obj
}

#' Recursively unwrap SpatRasters inside a plain list structure (internal)
#' @keywords internal
unwrap_recursive <- function(obj) {
  if (inherits(obj, "PackedSpatRaster")) return(terra::unwrap(obj))
  if (is.list(obj) && is.null(attr(obj, "class"))) {
    return(lapply(obj, unwrap_recursive))
  }
  obj
}

#' Save a cystimap object to disk, safely handling terra pointers
#'
#' `SpatRaster` objects store a pointer to external C++ memory that
#' does not survive `save()`/`saveRDS()` directly. This function wraps
#' every SpatRaster inside the object before saving, so it can be
#' safely restored in a future R session with `load_cystimap()`.
#'
#' @param x a cystimap object
#' @param file path to save to (should end in .rds)
#' @export
save_cystimap <- function(x, file) {
  validate_cystimap(x)
  wrapped <- wrap_recursive(unclass(x))
  saveRDS(wrapped, file)
  invisible(file)
}

#' Load a cystimap object previously saved with save_cystimap()
#'
#' @param file path to the .rds file created by `save_cystimap()`
#' @return a cystimap object with all SpatRasters restored to live pointers
#' @export
load_cystimap <- function(file) {
  if (!file.exists(file)) stop("File not found: ", file, call. = FALSE)
  raw <- readRDS(file)
  restored <- unwrap_recursive(raw)
  structure(restored, class = "cystimap")
}
