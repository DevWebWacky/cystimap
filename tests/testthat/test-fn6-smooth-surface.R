make_indicator_x <- function(n = 30) {
  x <- make_test_study_area()
  dummy <- make_test_points(n = n)
  tmp <- tempfile(fileext = ".csv")
  write.csv(dummy, tmp, row.names = FALSE)
  x <- read_data(x, file = tmp, name = "cases")
  create_indicator(
    x, data_name = "cases", indicator_name = "rate",
    formula = cases * 5, coords = c("lon", "lat"), coords_crs = 4326
  )
}

test_that("smooth_surface with IDW produces a masked prediction raster", {
  x <- make_indicator_x()
  x <- smooth_surface(x, indicator_name = "rate", surface_name = "idw_s", method = "idw", power = 2)

  s <- x$surfaces$idw_s
  expect_true(inherits(s$prediction, "SpatRaster"))
  expect_equal(s$method_used, "idw")
  expect_true(is.null(s$uncertainty))
})

test_that("smooth_surface with automatic Ordinary Kriging selects a variogram model", {
  x <- make_indicator_x()
  x <- smooth_surface(x, indicator_name = "rate", surface_name = "ok_s",
                       method = "ordinary_kriging", mode = "automatic")

  s <- x$surfaces$ok_s
  expect_true(inherits(s$prediction, "SpatRaster"))
  expect_true(inherits(s$uncertainty, "SpatRaster"))
  expect_true(nrow(s$candidates) > 0)
  expect_true(any(s$candidates$converged))
})

test_that("smooth_surface with manual Ordinary Kriging uses the supplied variogram exactly", {
  skip_if_not_installed("gstat")
  x <- make_indicator_x()

  obs <- x$data$indicators$rate
  emp_vgm <- gstat::variogram(rate ~ 1, obs)
  manual_fit <- gstat::fit.variogram(emp_vgm, gstat::vgm("Exp"))

  x <- smooth_surface(x, indicator_name = "rate", surface_name = "ok_manual",
                       method = "ordinary_kriging", mode = "manual",
                       manual_model = manual_fit)

  s <- x$surfaces$ok_manual
  expect_true(inherits(s$prediction, "SpatRaster"))
  expect_equal(s$mode, "manual")
  expect_identical(s$model_fit, manual_fit)
})

test_that("smooth_surface manual mode errors without manual_model", {
  x <- make_indicator_x()
  expect_error(
    smooth_surface(x, indicator_name = "rate", surface_name = "ok_bad",
                    method = "ordinary_kriging", mode = "manual"),
    "manual_model"
  )
})

test_that("smooth_surface with nearest_neighbour returns a distance raster", {
  x <- make_indicator_x()
  x <- smooth_surface(x, indicator_name = "rate", surface_name = "nn_s", method = "nearest_neighbour")

  s <- x$surfaces$nn_s
  expect_true(inherits(s$prediction, "SpatRaster"))
  expect_true(inherits(s$distance_raster, "SpatRaster"))
})

test_that("smooth_surface warns when cross-validated correlation is low", {
  x <- make_indicator_x()
  expect_warning(
    smooth_surface(x, indicator_name = "rate", surface_name = "ok_cv",
                    method = "ordinary_kriging", mode = "automatic", cv_method = "loo"),
    "correlation"
  )
})

test_that("smooth_surface existing_raster pathway crops and masks without reinterpolating", {
  x <- make_indicator_x()
  x <- smooth_surface(x, indicator_name = "rate", surface_name = "idw_s", method = "idw")

  tmp_tif <- tempfile(fileext = ".tif")
  terra::writeRaster(x$surfaces$idw_s$prediction, tmp_tif, overwrite = TRUE)

  x <- smooth_surface(x, indicator_name = NULL, surface_name = "existing_s",
                       method = "existing_raster", existing_raster = tmp_tif)

  s <- x$surfaces$existing_s
  expect_true(inherits(s$prediction, "SpatRaster"))
  expect_true("cropped and masked to study area" %in% s$modifications)
})

