test_that("read_data reads a csv and attaches it to x$data", {
  x <- make_test_study_area()
  dummy <- make_test_points()
  tmp <- tempfile(fileext = ".csv")
  write.csv(dummy, tmp, row.names = FALSE)

  x <- read_data(x, file = tmp, name = "cases")

  expect_true("cases" %in% names(x$data))
  expect_equal(nrow(x$data$cases), 30)
  expect_equal(x$log[[length(x$log)]]$step, "read_data")
})

test_that("read_data errors on an unrecognised file extension", {
  x <- make_test_study_area()
  tmp <- tempfile(fileext = ".xyz")
  writeLines("dummy", tmp)

  expect_error(read_data(x, file = tmp, name = "bad"), "Unrecognised|Could not automatically detect")
})

test_that("read_data transforms spatial data to the study area CRS", {
  x <- make_test_study_area()
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  tmp <- tempfile(fileext = ".gpkg")
  sf::st_write(nc, tmp, quiet = TRUE)

  x <- read_data(x, file = tmp, name = "nc_copy")

  expect_equal(sf::st_crs(x$data$nc_copy), x$crs)
})

