#' Create a publication-quality map of a cystimap raster output
#'
#' Produces a ggplot2 map from any cystimap raster output (a smooth
#' surface, classified raster, combined raster, or consensus raster),
#' with study area boundary, optional exclusions, legend, north arrow,
#' scale bar, and optional inset. Supports continuous gradients and
#' discrete/categorical colour schemes.
#'
#' @param x a cystimap object
#' @param raster_source one of "surfaces", "classified", "combined",
#'   "consensus" — same meaning as in `admin_summary()`
#' @param raster_name name of the raster within that source (ignored
#'   for "consensus")
#' @param raster_layer for classified/combined sources with multiple
#'   internal rasters, which one to plot. Default "combined".
#' @param title map title
#' @param legend_title legend title. Defaults to `raster_name`.
#' @param palette colour palette. For continuous rasters, a
#'   viridis option name (e.g. "viridis", "magma", "plasma") or a
#'   vector of colours for a custom gradient. For discrete rasters, a
#'   named vector of colours (names = class values) or NULL for
#'   automatic discrete colours.
#' @param class_labels optional named vector mapping class values to
#'   display labels (discrete rasters only)
#' @param show_boundary logical, overlay the study area boundary
#' @param show_exclusions logical, overlay exclusion zones if present
#' @param show_scalebar logical, add a scale bar (requires ggspatial)
#' @param show_north_arrow logical, add a north arrow (requires ggspatial)
#' @param inset logical, add a small inset locator map showing the
#'   study area within a wider context (uses `inset_context`, an sf
#'   object, e.g. a country or region boundary)
#' @param inset_context sf object for the inset map background
#'
#' @return a ggplot object (also invisibly stored in
#'   `x$results$maps[[raster_name or "consensus"]]` is NOT modified —
#'   this function returns a plot object directly, not the cystimap
#'   object, since maps are typically used/saved immediately)
#' @export
map_surface <- function(x, raster_source = c("surfaces", "classified", "combined", "consensus"),
                        raster_name = NULL, raster_layer = "combined",
                        title = NULL, legend_title = NULL, palette = "viridis",
                        class_labels = NULL,
                        show_boundary = TRUE, show_exclusions = TRUE,
                        show_scalebar = TRUE, show_north_arrow = TRUE,
                        inset = FALSE, inset_context = NULL) {

  validate_cystimap(x)
  raster_source <- match.arg(raster_source)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("map_surface() requires 'ggplot2'. Install with install.packages('ggplot2').", call. = FALSE)
  }
  if (!requireNamespace("tidyterra", quietly = TRUE)) {
    stop("map_surface() requires 'tidyterra'. Install with install.packages('tidyterra').", call. = FALSE)
  }

  #resolve raster (reuses the same lookup logic as admin_summary)
  is_categorical <- raster_source %in% c("classified", "combined", "consensus")

  rast <- switch(raster_source,
                 surfaces = {
                   if (is.null(x$surfaces[[raster_name]])) stop("Surface '", raster_name, "' not found.", call. = FALSE)
                   x$surfaces[[raster_name]]$prediction
                 },
                 classified = {
                   entry <- x$classified[[raster_name]]
                   if (is.null(entry)) stop("Classified surface '", raster_name, "' not found.", call. = FALSE)
                   rl <- if (!is.null(entry$rasters[[raster_layer]])) raster_layer else names(entry$rasters)[1]
                   entry$rasters[[rl]]
                 },
                 combined = {
                   cr <- x$results$combined[[raster_name]]$combined_raster
                   if (is.null(cr)) cr <- x$scenarios$outputs[[raster_name]]$combined_raster
                   if (is.null(cr)) stop("Combined/scenario raster '", raster_name, "' not found.", call. = FALSE)
                   cr
                 },
                 consensus = {
                   if (is.null(x$results$consensus)) stop("No consensus results found.", call. = FALSE)
                   x$results$consensus$consensus_count
                 }
  )

  if (is_categorical) {
    rast <- terra::as.factor(rast)
  }

  if (is.null(legend_title)) legend_title <- raster_name %||% raster_source

  #base plot
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = rast)

  #colour scale
  if (is_categorical) {
    rast_vals <- sort(as.numeric(levels(rast)[[1]][[1]]))
    if (!is.null(class_labels)) {
      labs <- class_labels[as.character(rast_vals)]
      labs[is.na(labs)] <- as.character(rast_vals[is.na(labs)])
    } else {
      labs <- as.character(rast_vals)
    }
    if (is.null(palette) || !is.character(palette) || length(palette) == 1) {
      cols <- grDevices::hcl.colors(length(rast_vals), palette = "Dynamic")
    } else {
      cols <- palette
    }
    p <- p + ggplot2::scale_fill_manual(
      values = stats::setNames(cols, rast_vals),
      labels = labs, na.value = "transparent", name = legend_title
    )
  } else {
    if (length(palette) == 1) {
      p <- p + ggplot2::scale_fill_viridis_c(option = palette, na.value = "transparent", name = legend_title)
    } else {
      p <- p + ggplot2::scale_fill_gradientn(colours = palette, na.value = "transparent", name = legend_title)
    }
  }

  #study area boundary
  if (isTRUE(show_boundary) && !is.null(x$study_area)) {
    p <- p + ggplot2::geom_sf(data = x$study_area, fill = NA, colour = "black", linewidth = 0.4)
  }

  #exclusions
  if (isTRUE(show_exclusions) && !is.null(x$exclusions)) {
    p <- p + ggplot2::geom_sf(data = x$exclusions, fill = "lightblue", colour = "steelblue", alpha = 0.6)
  }

  #scale bar + north arrow
  if (isTRUE(show_scalebar) || isTRUE(show_north_arrow)) {
    if (!requireNamespace("ggspatial", quietly = TRUE)) {
      warning("show_scalebar/show_north_arrow require 'ggspatial'. ",
              "Install with install.packages('ggspatial'). Skipping.", call. = FALSE)
    } else {
      if (isTRUE(show_scalebar)) {
        p <- p + ggspatial::annotation_scale(location = "bl", width_hint = 0.3)
      }
      if (isTRUE(show_north_arrow)) {
        p <- p + ggspatial::annotation_north_arrow(
          location = "tr", which_north = "true",
          style = ggspatial::north_arrow_fancy_orienteering()
        )
      }
    }
  }

  #title + theme
  p <- p +
    ggplot2::labs(title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(colour = "grey90")
    )

  #inset map
  if (isTRUE(inset)) {
    if (is.null(inset_context)) {
      warning("`inset = TRUE` but no `inset_context` supplied — skipping inset.", call. = FALSE)
    } else if (!requireNamespace("cowplot", quietly = TRUE)) {
      warning("Inset maps require the 'cowplot' package. Install with install.packages('cowplot'). Skipping.", call. = FALSE)
    } else {
      inset_plot <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = inset_context, fill = "grey95", colour = "grey60") +
        ggplot2::geom_sf(data = x$study_area, fill = "red", colour = "red") +
        ggplot2::theme_void()

      p <- cowplot::ggdraw() +
        cowplot::draw_plot(p) +
        cowplot::draw_plot(inset_plot, x = 0.02, y = 0.65, width = 0.3, height = 0.3)
    }
  }

  p
}

#' Arrange multiple cystimap maps into one multi-panel figure
#'
#' @param ... ggplot objects (typically from `map_surface()`)
#' @param ncol number of columns in the panel layout
#' @param labels optional panel labels (e.g. c("A","B","C"))
#'
#' @return a combined plot object
#' @export
map_panel <- function(..., ncol = 2, labels = NULL) {
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("map_panel() requires the 'cowplot' package. Install with install.packages('cowplot').", call. = FALSE)
  }
  plots <- list(...)
  cowplot::plot_grid(plotlist = plots, ncol = ncol, labels = labels)
}
