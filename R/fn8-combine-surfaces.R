#' Combine two or more classified raster surfaces
#'
#' Combines classified surfaces (from `classify_surface()`) into a
#' single analytical raster using binary overlap, additive scoring,
#' weighted combination, or logical rules.
#'
#' @param x a cystimap object
#' @param classified_names character vector of names in `x$classified`
#'   to combine. If a classified entry has multiple rasters (e.g. from
#'   sensitivity mode), the `combined` or first raster is used —
#'   specify exactly which via `raster_select` if needed.
#' @param combined_name character, name to store the result under in
#'   `x$results$combined`
#' @param method one of "binary_overlap", "additive", "weighted",
#'   "logical", "custom"
#' @param weights named numeric vector of weights (one per name in
#'   `classified_names`), required if method = "weighted"
#' @param logical_rule character string describing the rule for
#'   method = "logical": one of "AND", "OR", "XOR", "AND_NOT" (applies
#'   to exactly 2 rasters at a time for AND_NOT/XOR; AND/OR support any
#'   number)
#' @param custom_fun function(raster_list) returning a single SpatRaster,
#'   required if method = "custom"
#' @param raster_select optional named list mapping each classified
#'   name to which internal raster to use (default: "combined" if
#'   present, else the first available)
#'
#' @return the cystimap object `x`, with the combined result stored in
#'   `x$results$combined[[combined_name]]`
#' @export
combine_surfaces <- function(x, classified_names, combined_name,
                             method = c("binary_overlap", "additive", "weighted",
                                        "logical", "custom"),
                             weights = NULL,
                             logical_rule = c("AND", "OR", "XOR", "AND_NOT"),
                             custom_fun = NULL,
                             raster_select = NULL) {

  validate_cystimap(x)
  method <- match.arg(method)

  if (length(classified_names) < 2) {
    stop("At least two classified surfaces are required to combine.", call. = FALSE)
  }

  missing_names <- setdiff(classified_names, names(x$classified))
  if (length(missing_names) > 0) {
    stop("Classified surface(s) not found: ", paste(missing_names, collapse = ", "), call. = FALSE)
  }

  #pick the raster to use from each classified entry
  pick_raster <- function(nm) {
    entry <- x$classified[[nm]]
    sel <- raster_select[[nm]]
    if (!is.null(sel)) {
      if (is.null(entry$rasters[[sel]])) stop("raster_select '", sel, "' not found for '", nm, "'.", call. = FALSE)
      return(entry$rasters[[sel]])
    }
    if (!is.null(entry$rasters[["combined"]])) return(entry$rasters[["combined"]])
    if (!is.null(entry$rasters[["display"]])) return(entry$rasters[["display"]])
    entry$rasters[[1]]
  }

  rast_list <- stats::setNames(lapply(classified_names, pick_raster), classified_names)

  # check alignment
  ref <- rast_list[[1]]
  aligned <- vapply(rast_list[-1], function(r) terra::compareGeom(ref, r, stopOnError = FALSE), logical(1))
  if (!all(aligned)) {
    stop("Rasters are not spatially aligned (CRS/resolution/extent/origin differ). ",
         "Align them (e.g. via smooth_surface()'s shared template) before combining.", call. = FALSE)
  }

  warn_msgs <- character(0)

  #apply combination method
  combined_rast <- switch(method,

                          binary_overlap = {
                            stk <- terra::rast(rast_list)
                            overlap_count <- sum(stk > 0, na.rm = FALSE)  # NA propagates if any layer NA
                            overlap_count
                          },

                          additive = {
                            stk <- terra::rast(rast_list)
                            sum(stk, na.rm = FALSE)
                          },

                          weighted = {
                            if (is.null(weights)) stop("`weights` must be supplied for method = 'weighted'.", call. = FALSE)
                            if (!all(classified_names %in% names(weights))) {
                              stop("`weights` must be a named vector covering all of `classified_names`.", call. = FALSE)
                            }
                            if (abs(sum(weights[classified_names]) - 1) > 1e-6) {
                              warn_msgs <- c(warn_msgs, paste0(
                                "Weights sum to ", round(sum(weights[classified_names]), 3),
                                ", not 1. Proceeding, but results may be harder to interpret."
                              ))
                            }
                            pieces <- lapply(classified_names, function(nm) rast_list[[nm]] * weights[[nm]])
                            stk <- terra::rast(pieces)
                            sum(stk, na.rm = FALSE)
                          },

                          logical = {
                            logical_rule <- match.arg(logical_rule)
                            bin_list <- lapply(rast_list, function(r) r > 0)
                            if (logical_rule %in% c("XOR", "AND_NOT") && length(bin_list) != 2) {
                              stop("logical_rule '", logical_rule, "' requires exactly 2 rasters.", call. = FALSE)
                            }
                            result <- switch(logical_rule,
                                             AND     = Reduce(function(a, b) a & b, bin_list),
                                             OR      = Reduce(function(a, b) a | b, bin_list),
                                             XOR     = xor(bin_list[[1]], bin_list[[2]]),
                                             AND_NOT = bin_list[[1]] & !bin_list[[2]]
                            )
                            terra::as.int(result)
                          },

                          custom = {
                            if (!is.function(custom_fun)) stop("`custom_fun` must be a function(raster_list) for method = 'custom'.", call. = FALSE)
                            out <- custom_fun(rast_list)
                            if (!inherits(out, "SpatRaster")) stop("`custom_fun` must return a SpatRaster.", call. = FALSE)
                            out
                          }
  )

  for (w in warn_msgs) warning(w, call. = FALSE)

  #frequency / area / percentage summary
  cell_area <- prod(terra::res(combined_rast))
  freq_tab <- terra::freq(combined_rast)
  freq_tab$area <- freq_tab$count * cell_area
  freq_tab$percentage <- 100 * freq_tab$count / sum(freq_tab$count)

  if (any(freq_tab$count == 0)) {
    warning("One or more combination classes contain zero cells.", call. = FALSE)
  }

  result <- list(
    combined_raster = combined_rast,
    input_names     = classified_names,
    method          = method,
    weights         = if (method == "weighted") weights[classified_names] else NULL,
    logical_rule    = if (method == "logical") logical_rule else NULL,
    frequencies     = freq_tab,
    warnings        = warn_msgs
  )

  if (is.null(x$results$combined)) x$results$combined <- list()
  x$results$combined[[combined_name]] <- result

  x <- log_step(x, "combine_surfaces", details = list(
    combined_name    = combined_name,
    input_names      = classified_names,
    method           = method,
    logical_rule     = if (method == "logical") logical_rule else NA
  ))

  x
}
