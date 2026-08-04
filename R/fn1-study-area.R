#' Prepare the study area for a cystimap workflow
#'
#' Establishes the geographical environment for the entire analysis:
#' coordinate reference system, boundary, raster template, prediction
#' grid, and exclusion zones. Every raster produced later in the
#' workflow will align to what is defined here.
#'
#' @param boundary an sf object, or a file path (shapefile, GeoPackage,
#'   GeoJSON, KML) defining the study boundary. Can be one polygon,
#'   multiple polygons, or a multipart polygon.
#' @param crs optional CRS to force (EPSG code, proj4string, or WKT).
#'   If NULL, cystimap will attempt to detect it from `boundary`. If it
#'   cannot be detected, the function stops with an informative error.
#' @param resolution numeric, target raster resolution in the units of
#'   `crs` (e.g. metres). Required unless `template` is supplied.
#' @param template optional existing SpatRaster to use as the template
#'   instead of building one from `resolution`.
#' @param exclusions optional sf object or file path defining areas to
#'   exclude (e.g. water bodies, protected areas).
#' @param exclusion_action how exclusions should be handled: one of
#'   "remove" (cut out of study area), "mask" (mask raster cells but
#'   keep boundary), "overlay" (keep as map layer only, no removal),
#'   or "ignore".
#' @param dissolve logical, should multiple boundary polygons be
#'   dissolved into a single analytical area? Original boundaries are
#'   preserved separately for mapping regardless.
#'
#' @return a `cystimap` object with `study_area`, `template`, `mask`,
#'   `grid`, `exclusions`, and `crs` populated.
#' @export
prepare_study_area <- function(boundary,
                               crs = NULL,
                               resolution = NULL,
                               template = NULL,
                               exclusions = NULL,
                               exclusion_action = c("remove", "mask", "overlay", "ignore"),
                               dissolve = FALSE) {

  exclusion_action <- match.arg(exclusion_action)

  #Read boundary if a path was given
  if (is.character(boundary)) {
    boundary <- sf::st_read(boundary, quiet = TRUE)
  }
  if (!inherits(boundary, "sf")) {
    stop("`boundary` must be an sf object or a valid file path.", call. = FALSE)
  }

  #Resolve CRS (never guess)
  analytical_crs <- resolve_crs(boundary, crs)
  boundary <- sf::st_transform(boundary, analytical_crs)

  # Validate + repair geometry
  boundary <- repair_geometry(boundary)

  # preserve original (undissolved) boundaries for mapping
  boundary_original <- boundary

  if (isTRUE(dissolve)) {
    boundary <- sf::st_union(boundary)
    boundary <- sf::st_as_sf(boundary)
  }

  #Handle exclusions
  exclusions_sf <- NULL
  if (!is.null(exclusions)) {
    if (is.character(exclusions)) {
      exclusions <- sf::st_read(exclusions, quiet = TRUE)
    }
    exclusions <- sf::st_transform(exclusions, analytical_crs)
    exclusions_sf <- repair_geometry(exclusions)

    if (exclusion_action == "remove") {
      boundary <- suppressWarnings(sf::st_difference(sf::st_union(boundary), sf::st_union(exclusions_sf)))
      boundary <- sf::st_as_sf(boundary)
    }
  }

  #Guard against building a metric-resolution raster on a geographic CRS
  if (is.null(template) && !is.null(resolution)) {
    if (sf::st_is_longlat(analytical_crs)) {
      stop(
        "`crs` is a geographic CRS (lat/lon, e.g. NAD27/WGS84), but ",
        "`resolution` is assumed to be in linear units (e.g. metres). ",
        "Please supply a projected `crs` (e.g. a UTM zone or national grid) ",
        "so that resolution is unambiguous. cystimap will not guess a ",
        "projection.",
        call. = FALSE
      )
    }
  }

  #Build or accept raster template
  if (!is.null(template)) {
    if (!inherits(template, "SpatRaster")) {
      stop("`template` must be a terra SpatRaster.", call. = FALSE)
    }
    rast_template <- template
  } else {
    if (is.null(resolution)) {
      stop(
        "Either `resolution` or `template` must be supplied ",
        "to build the raster template.",
        call. = FALSE
      )
    }
    ext <- terra::ext(terra::vect(boundary))
    rast_template <- terra::rast(
      ext, resolution = resolution, crs = analytical_crs$wkt
    )
  }

  #Build raster mask from boundary
  boundary_vect <- terra::vect(boundary)
  rast_mask <- terra::rasterize(boundary_vect, rast_template, field = 1)

  if (!is.null(exclusions_sf) && exclusion_action == "mask") {
    excl_vect <- terra::vect(exclusions_sf)
    excl_mask <- terra::rasterize(excl_vect, rast_template, field = 1)
    rast_mask <- terra::mask(rast_mask, excl_mask, inverse = TRUE)
  }

  #Build prediction grid (cell centres)
  grid_points <- terra::as.points(rast_mask, na.rm = TRUE)
  grid_sf <- sf::st_as_sf(grid_points)

  # Assemble the cystimap object
  x <- new_cystimap(
    study_area = boundary,
    template   = rast_template,
    mask       = rast_mask,
    grid       = grid_sf,
    exclusions = exclusions_sf,
    crs        = analytical_crs,
    meta       = list(
      created         = Sys.time(),
      package_version = utils::packageVersion("cystimap"),
      boundary_original = boundary_original
    )
  )

  x <- log_step(x, "prepare_study_area", details = list(
    resolution        = resolution,
    dissolve          = dissolve,
    exclusion_action  = exclusion_action,
    n_boundary_polygons = nrow(boundary_original),
    has_exclusions    = !is.null(exclusions_sf)
  ))

  x
}
