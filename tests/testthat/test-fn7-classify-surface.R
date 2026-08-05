make_classified_x <- function(n = 30) {
  x <- make_test_study_area()
  dummy <- make_test_points(n = n)
  tmp <- tempfile(fileext = ".csv")
  write.csv(dummy, tmp, row.names = FALSE)
  x <- read_data(x, file = tmp, name = "cases")
  x <- create_indicator(x, data_name = "cases", indicator_name = "rate",
                         formula = cases * 5, coords = c("lon","lat"), coords_crs = 4326)
  suppressWarnings(smooth_surface(x, indicator_name = "rate", surface_name = "s1", method = "idw"))
}

test_that("classify_surface binary quantile produces two classes coded 0/1", {
  x <- make_classified_x()
  x <- classify_surface(x, surface_name = "s1", classified_name = "c1",
                         type = "binary", cutoff_method = "quantile", cutoffs = 0.5)

  r <- x$classified$c1$rasters$combined
  vals <- sort(unique(terra::values(r, na.rm = TRUE)))
  expect_equal(vals, c(0, 1))
})

test_that("classify_surface sensitivity mode produces one raster per cutoff", {
  x <- make_classified_x()
  x <- classify_surface(x, surface_name = "s1", classified_name = "sens1",
                         type = "binary", cutoff_method = "quantile",
                         cutoffs = c(0.25, 0.5, 0.75), sensitivity_mode = TRUE)

  expect_equal(length(x$classified$sens1$rasters), 3)
})

test_that("classify_surface errors when fewer than 2 breakpoints result", {
  x <- make_classified_x()
  expect_error(
    calculate_cutoffs(1:10, method = "fixed", cutoffs = NULL),
    "must be supplied"
  )
})

test_that("classify_surface continuous_display passes the raster through unchanged", {
  x <- make_classified_x()
  x <- classify_surface(x, surface_name = "s1", classified_name = "disp1", type = "continuous_display")

  expect_true(inherits(x$classified$disp1$rasters$display, "SpatRaster"))
  expect_null(x$classified$disp1$cutoffs_table)
})

