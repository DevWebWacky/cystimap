#' Prepare/validate a user-supplied existing raster surface (internal)
#' @keywords internal
prepare_existing_raster <- function(raster, template, mask, crs,
                                    resample_method = "bilinear") {

  if (is.character(raster)) raster <- terra::rast(raster)
  if (!inherits(raster, "SpatRaster")) {
    stop("`existing_raster` must be a terra SpatRaster or a file path.", call. = FALSE)
  }

  modifications <- character(0)

  if (is.na(terra::crs(raster)) || terra::crs(raster) == "") {
    stop("Existing raster has no CRS. cystimap will not guess a CRS.", call. = FALSE)
  }
  if (!terra::same.crs(raster, template)) {
    raster <- terra::project(raster, template)
    modifications <- c(modifications, "reprojected to analytical CRS")
  }
  if (!terra::compareGeom(raster, template, stopOnError = FALSE)) {
    raster <- terra::resample(raster, template, method = resample_method)
    modifications <- c(modifications, paste0("resampled (", resample_method, ") to template geometry"))
  }
  raster <- terra::crop(raster, template)
  raster <- terra::mask(raster, mask)
  modifications <- c(modifications, "cropped and masked to study area")

  list(
    prediction       = raster,
    uncertainty      = NULL,
    method_requested = "existing_raster",
    method_used      = "existing_raster",
    mode             = NA,
    model_fit        = NULL,
    candidates       = NULL,
    cv_results       = NULL,
    cv_summary       = NULL,
    parameters       = list(resample_method = resample_method),
    modifications    = modifications,
    warnings         = character(0),
    diagnostics      = list()
  )
}
