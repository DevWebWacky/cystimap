test_that("prepare_study_area builds a valid cystimap object", {
  x <- make_test_study_area()

  expect_s3_class(x, "cystimap")
  expect_s3_class(x$study_area, "sf")
  expect_true(inherits(x$template, "SpatRaster"))
  expect_true(inherits(x$mask, "SpatRaster"))
  expect_false(is.null(x$crs))
  expect_equal(length(x$log), 1)
  expect_equal(x$log[[1]]$step, "prepare_study_area")
})

test_that("prepare_study_area refuses a geographic CRS with a resolution", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_error(
    prepare_study_area(boundary = nc, resolution = 5000, dissolve = TRUE),
    "geographic CRS"
  )
})

test_that("prepare_study_area errors on non-sf, non-path boundary input", {
  expect_error(
    prepare_study_area(boundary = list(a = 1), crs = 32119, resolution = 20000),
    "sf object"
  )
})

