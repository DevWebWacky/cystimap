#' Export cystimap results to a reproducible directory structure
#'
#' Writes rasters (GeoTIFF), vectors (GeoPackage), tables (CSV), the
#' processing log, and a manifest file documenting everything that was
#' saved. Creates a tidy subfolder structure under `output_dir`.
#'
#' @param x a cystimap object
#' @param output_dir directory to export into (created if it doesn't exist)
#' @param what character vector of what to export: any of "surfaces",
#'   "classified", "combined", "consensus", "admin_summary",
#'   "sensitivity", "agreement", "log". Default exports everything
#'   available.
#' @param overwrite logical, overwrite existing files. Default FALSE.
#'
#' @return invisibly, a data.frame manifest of every file written
#'   (also saved as manifest.csv in `output_dir`)
#' @export
export_results <- function(x, output_dir,
                           what = c("surfaces", "classified", "combined", "consensus",
                                    "admin_summary", "sensitivity", "agreement", "log"),
                           overwrite = FALSE) {

  validate_cystimap(x)

  dirs <- list(
    rasters = file.path(output_dir, "rasters"),
    tables  = file.path(output_dir, "tables"),
    logs    = file.path(output_dir, "logs")
  )
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  manifest <- data.frame(category = character(0), name = character(0),
                         file = character(0), timestamp = character(0))

  add_manifest <- function(category, name, file) {
    manifest <<- rbind(manifest, data.frame(
      category = category, name = name, file = file,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }

  write_raster_safe <- function(rast, path) {
    if (!inherits(rast, "SpatRaster")) return(invisible(NULL))
    terra::writeRaster(rast, path, overwrite = overwrite)
    path
  }

  #surfaces (Function 6 outputs)
  if ("surfaces" %in% what) {
    for (nm in names(x$surfaces)) {
      s <- x$surfaces[[nm]]
      if (!is.null(s$prediction)) {
        f <- write_raster_safe(s$prediction, file.path(dirs$rasters, paste0("surface_", nm, "_prediction.tif")))
        if (!is.null(f)) add_manifest("surfaces", nm, f)
      }
      if (!is.null(s$uncertainty)) {
        f <- write_raster_safe(s$uncertainty, file.path(dirs$rasters, paste0("surface_", nm, "_uncertainty.tif")))
        if (!is.null(f)) add_manifest("surfaces", paste0(nm, "_uncertainty"), f)
      }
    }
  }

  #classified (Function 7 outputs)
  if ("classified" %in% what) {
    for (nm in names(x$classified)) {
      c_entry <- x$classified[[nm]]
      for (layer_nm in names(c_entry$rasters)) {
        f <- write_raster_safe(c_entry$rasters[[layer_nm]],
                               file.path(dirs$rasters, paste0("classified_", nm, "_", layer_nm, ".tif")))
        if (!is.null(f)) add_manifest("classified", paste0(nm, "_", layer_nm), f)
      }
      if (!is.null(c_entry$frequencies)) {
        f <- file.path(dirs$tables, paste0("classified_", nm, "_frequencies.csv"))
        utils::write.csv(c_entry$frequencies, f, row.names = FALSE)
        add_manifest("classified_table", nm, f)
      }
    }
  }

  #combined (Function 8 + scenario outputs)
  if ("combined" %in% what) {
    combined_sources <- c(x$results$combined, x$scenarios$outputs)
    for (nm in names(combined_sources)) {
      c_entry <- combined_sources[[nm]]
      if (!is.null(c_entry$combined_raster)) {
        f <- write_raster_safe(c_entry$combined_raster, file.path(dirs$rasters, paste0("combined_", nm, ".tif")))
        if (!is.null(f)) add_manifest("combined", nm, f)
      }
      if (!is.null(c_entry$frequencies)) {
        f <- file.path(dirs$tables, paste0("combined_", nm, "_frequencies.csv"))
        utils::write.csv(c_entry$frequencies, f, row.names = FALSE)
        add_manifest("combined_table", nm, f)
      }
    }
  }

  #consensus (Function 9 stage 5)
  if ("consensus" %in% what && !is.null(x$results$consensus)) {
    cons <- x$results$consensus
    for (layer_nm in c("consensus_count", "consensus_proportion", "consensus_flag", "weighted_consensus")) {
      if (!is.null(cons[[layer_nm]])) {
        f <- write_raster_safe(cons[[layer_nm]], file.path(dirs$rasters, paste0("consensus_", layer_nm, ".tif")))
        if (!is.null(f)) add_manifest("consensus", layer_nm, f)
      }
    }
    if (!is.null(cons$summary_table)) {
      f <- file.path(dirs$tables, "consensus_summary.csv")
      utils::write.csv(cons$summary_table, f, row.names = FALSE)
      add_manifest("consensus_table", "summary", f)
    }
  }

  #sensitivity / agreement tables (Function 9 stages 3-4)
  if ("sensitivity" %in% what && !is.null(x$results$sensitivity)) {
    f <- file.path(dirs$tables, "scenario_sensitivity.csv")
    utils::write.csv(x$results$sensitivity, f, row.names = FALSE)
    add_manifest("sensitivity_table", "scenario_sensitivity", f)
  }
  if ("agreement" %in% what && !is.null(x$results$agreement)) {
    f <- file.path(dirs$tables, "scenario_agreement.csv")
    utils::write.csv(x$results$agreement, f, row.names = FALSE)
    add_manifest("agreement_table", "scenario_agreement", f)
  }

  #admin summaries (Function 13)
  if ("admin_summary" %in% what && !is.null(x$results$admin_summary)) {
    for (nm in names(x$results$admin_summary)) {
      entry <- x$results$admin_summary[[nm]]
      tbl <- if (!is.null(entry$summary_table)) entry$summary_table else entry$class_table
      if (!is.null(tbl)) {
        f <- file.path(dirs$tables, paste0("admin_summary_", gsub("[^A-Za-z0-9_]", "_", nm), ".csv"))
        utils::write.csv(tbl, f, row.names = FALSE)
        add_manifest("admin_summary", nm, f)
      }
    }
  }

  #processing log
  if ("log" %in% what && length(x$log) > 0) {
    log_df <- do.call(rbind, lapply(x$log, function(entry) {
      data.frame(
        step = entry$step,
        timestamp = entry$timestamp,
        details = paste(names(entry$details), unlist(lapply(entry$details, paste, collapse = ";")),
                        sep = "=", collapse = " | ")
      )
    }))
    f <- file.path(dirs$logs, "processing_log.csv")
    utils::write.csv(log_df, f, row.names = FALSE)
    add_manifest("log", "processing_log", f)
  }

  #write the manifest itself
  manifest_path <- file.path(output_dir, "manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)

  message("Exported ", nrow(manifest), " file(s) to '", output_dir, "'. See manifest.csv for details.")
  invisible(manifest)
}
