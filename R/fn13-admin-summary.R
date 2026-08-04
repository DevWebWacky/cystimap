#' Summarise a raster within administrative or other polygons
#'
#' Calculates zonal statistics for any cystimap raster output (a
#' smooth surface, classified raster, combined raster, or consensus
#' raster) within a set of polygons such as districts, villages, or
#' watersheds.
#'
#' @param x a cystimap object
#' @param raster_source where to pull the raster from: one of
#'   "surfaces", "classified", "combined", "consensus". Determines
#'   how `raster_name` is interpreted.
#' @param raster_name name of the specific raster within that source
#'   (e.g. a surface name, classified name, or combined scenario id).
#'   Ignored for `raster_source = "consensus"` (uses consensus_count).
#' @param raster_layer for classified/combined sources that may store
#'   multiple internal rasters (e.g. "combined" vs sensitivity cutoffs),
#'   which one to summarise. Default "combined".
#' @param polygons an sf polygon object (e.g. districts). Will be
#'   transformed to the analytical CRS if needed.
#' @param zone_id character, column name in `polygons` identifying
#'   each zone (e.g. district name or code)
#' @param stats character vector of summary statistics to calculate
#'   for continuous rasters: any of "mean", "median", "min", "max",
#'   "sd", "sum". Ignored for classified/discrete rasters, where class
#'   frequency/dominant-class/area/percentage are always calculated.
#' @param is_categorical logical, TRUE if the raster contains discrete
#'   class codes (classified/combined/consensus outputs), FALSE for
#'   continuous surfaces. If NULL (default), inferred from
#'   `raster_source`.
#'
#' @return the cystimap object `x`, with a summary table stored in
#'   `x$results$admin_summary[[zone_id + raster_name]]`
#' @export
admin_summary <- function(x, raster_source = c("surfaces", "classified", "combined", "consensus"),
                          raster_name = NULL, raster_layer = "combined",
                          polygons, zone_id,
                          stats = c("mean", "median", "min", "max", "sd"),
                          is_categorical = NULL) {

  validate_cystimap(x)
  raster_source <- match.arg(raster_source)

  if (!requireNamespace("exactextractr", quietly = TRUE)) {
    stop("admin_summary() requires the 'exactextractr' package. ",
         "Install with install.packages('exactextractr').", call. = FALSE)
  }

  #resolve the raster to summarise
  if (is.null(is_categorical)) {
    is_categorical <- raster_source %in% c("classified", "combined", "consensus")
  }

  rast <- switch(raster_source,
                 surfaces = {
                   if (is.null(x$surfaces[[raster_name]])) stop("Surface '", raster_name, "' not found.", call. = FALSE)
                   x$surfaces[[raster_name]]$prediction
                 },
                 classified = {
                   if (is.null(x$classified[[raster_name]])) stop("Classified surface '", raster_name, "' not found.", call. = FALSE)
                   entry <- x$classified[[raster_name]]
                   rl <- if (!is.null(entry$rasters[[raster_layer]])) raster_layer else names(entry$rasters)[1]
                   entry$rasters[[rl]]
                 },
                 combined = {
                   if (is.null(x$results$combined[[raster_name]]) && is.null(x$scenarios$outputs[[raster_name]])) {
                     stop("Combined/scenario raster '", raster_name, "' not found.", call. = FALSE)
                   }
                   cr <- x$results$combined[[raster_name]]$combined_raster
                   if (is.null(cr)) cr <- x$scenarios$outputs[[raster_name]]$combined_raster
                   cr
                 },
                 consensus = {
                   if (is.null(x$results$consensus)) stop("No consensus results found. Run scenario_consensus() first.", call. = FALSE)
                   x$results$consensus$consensus_count
                 }
  )

  if (is.null(rast)) stop("Could not resolve the requested raster.", call. = FALSE)

  #prepare polygons
  if (!inherits(polygons, "sf")) stop("`polygons` must be an sf object.", call. = FALSE)
  if (!(zone_id %in% names(polygons))) stop("`zone_id` column '", zone_id, "' not found in `polygons`.", call. = FALSE)
  if (is.na(sf::st_crs(polygons))) stop("`polygons` has no CRS. cystimap will not guess a CRS.", call. = FALSE)
  if (sf::st_crs(polygons) != x$crs) polygons <- sf::st_transform(polygons, x$crs)

  cell_area <- prod(terra::res(rast))

  if (is_categorical) {
    #categorical: class frequency, dominant class, area, percentage per zone
    ex <- exactextractr::exact_extract(rast, polygons, include_cols = zone_id)

    summary_rows <- lapply(ex, function(zone_df) {
      names(zone_df)[names(zone_df) == "value"] <- "value"
      zone_df <- zone_df[!is.na(zone_df$value), ]
      if (nrow(zone_df) == 0) {
        return(data.frame(class = NA, cells = NA, area = NA, percentage = NA))
      }
      # weight by coverage fraction (partial cell overlap)
      agg <- stats::aggregate(coverage_fraction ~ value, data = zone_df, FUN = sum)
      agg$area <- agg$coverage_fraction * cell_area
      agg$percentage <- 100 * agg$coverage_fraction / sum(agg$coverage_fraction)
      names(agg)[names(agg) == "value"] <- "class"
      names(agg)[names(agg) == "coverage_fraction"] <- "cells"
      agg[order(-agg$area), ]
    })

    zone_ids <- polygons[[zone_id]]
    full_table <- do.call(rbind, lapply(seq_along(summary_rows), function(i) {
      df <- summary_rows[[i]]
      df$zone <- zone_ids[i]
      df
    }))

    dominant <- do.call(rbind, lapply(seq_along(summary_rows), function(i) {
      df <- summary_rows[[i]]
      if (all(is.na(df$class))) {
        data.frame(zone = zone_ids[i], dominant_class = NA, dominant_percentage = NA)
      } else {
        top <- df[which.max(df$area), ]
        data.frame(zone = zone_ids[i], dominant_class = top$class, dominant_percentage = top$percentage)
      }
    }))

    result <- list(
      type = "categorical",
      class_table = full_table,
      dominant_class_table = dominant,
      raster_source = raster_source,
      raster_name = raster_name
    )

  } else {
    # continuous: mean/median/min/max/sd/sum per zone
    valid_stats <- intersect(stats, c("mean", "median", "min", "max", "sd", "sum"))
    ex <- exactextractr::exact_extract(rast, polygons, fun = valid_stats, append_cols = zone_id)

    result <- list(
      type = "continuous",
      summary_table = ex,
      stats = valid_stats,
      raster_source = raster_source,
      raster_name = raster_name
    )
  }

  key <- paste0(raster_source, "__", if (!is.null(raster_name)) raster_name else "consensus")
  if (is.null(x$results$admin_summary)) x$results$admin_summary <- list()
  x$results$admin_summary[[key]] <- result

  x <- log_step(x, "admin_summary", details = list(
    raster_source = raster_source,
    raster_name   = raster_name,
    zone_id       = zone_id,
    n_zones       = nrow(polygons),
    is_categorical = is_categorical
  ))

  x
}
