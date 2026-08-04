#' Read data into a cystimap workflow
#'
#' Reads spatial or non-spatial data from a variety of formats and
#' attaches it to a cystimap object under `data$<name>`. Format is
#' detected automatically from the file extension where possible.
#'
#' @param x a cystimap object (from `prepare_study_area()`)
#' @param file path to the data file, or a URL
#' @param name character, the name to store this dataset under in
#'   `x$data`. Defaults to the file's base name (without extension).
#' @param type optional override for the file type, one of "csv",
#'   "excel", "spss", "stata", "rds", "shapefile", "gpkg", "geojson",
#'   "raster". If NULL, detected from the file extension.
#' @param transform_crs logical, if the data is spatial, should it be
#'   transformed to `x$crs`? Default TRUE.
#' @param ... additional arguments passed to the underlying reader
#'   function (e.g. `sep` for csv, `sheet` for excel).
#'
#' @return the cystimap object `x`, with the new dataset added to
#'   `x$data[[name]]`
#' @export
read_data <- function(x, file, name = NULL, type = NULL,
                      transform_crs = TRUE, ...) {

  validate_cystimap(x)

  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(file))
  }

  detected_type <- type %||% detect_file_type(file)

  dataset <- switch(detected_type,
                    csv       = utils::read.csv(file, ...),
                    excel     = read_excel_safe(file, ...),
                    spss      = read_spss_safe(file, ...),
                    stata     = read_stata_safe(file, ...),
                    rds       = readRDS(file),
                    shapefile = ,
                    gpkg      = ,
                    geojson   = ,
                    kml       = sf::st_read(file, quiet = TRUE, ...),
                    raster    = terra::rast(file, ...),
                    stop("Unrecognised or unsupported file type: ", detected_type, call. = FALSE)
  )

  # if spatial, align CRS to the study area's analytical CRS
  is_spatial <- inherits(dataset, c("sf", "SpatVector"))
  if (is_spatial && isTRUE(transform_crs) && !is.null(x$crs)) {
    dataset <- sf::st_transform(dataset, x$crs)
  }

  n_rows <- tryCatch(nrow(dataset), error = function(e) NA)

  x$data[[name]] <- dataset

  x <- log_step(x, "read_data", details = list(
    name = name,
    file = file,
    type = detected_type,
    is_spatial = is_spatial,
    n_rows = n_rows
  ))

  x
}

#' Detect file type from extension
#' @param file file path or URL
#' @keywords internal
detect_file_type <- function(file) {
  ext <- tolower(tools::file_ext(file))

  switch(ext,
         csv             = "csv",
         tsv             = "csv",
         xls             = ,
         xlsx            = "excel",
         sav             = "spss",
         dta             = "stata",
         rds             = "rds",
         shp             = "shapefile",
         gpkg            = "gpkg",
         geojson         = ,
         json            = "geojson",
         kml             = "kml",
         tif             = ,
         tiff            = ,
         nc              = "raster",
         stop(
           "Could not automatically detect file type for '", file, "'. ",
           "Please supply `type` explicitly.",
           call. = FALSE
         )
  )
}

#' @keywords internal
read_excel_safe <- function(file, ...) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Reading Excel files requires the 'readxl' package. Install it with install.packages('readxl').", call. = FALSE)
  }
  readxl::read_excel(file, ...)
}

#' @keywords internal
read_spss_safe <- function(file, ...) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Reading SPSS files requires the 'haven' package. Install it with install.packages('haven').", call. = FALSE)
  }
  haven::read_sav(file, ...)
}

#' @keywords internal
read_stata_safe <- function(file, ...) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Reading Stata files requires the 'haven' package. Install it with install.packages('haven').", call. = FALSE)
  }
  haven::read_dta(file, ...)
}

#' Null-coalescing helper
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
