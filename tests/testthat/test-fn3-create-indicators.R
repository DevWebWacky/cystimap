test_that("create_indicator computes a formula and attaches locations", {
  x <- make_test_study_area()
  dummy <- make_test_points()
  tmp <- tempfile(fileext = ".csv")
  write.csv(dummy, tmp, row.names = FALSE)
  x <- read_data(x, file = tmp, name = "cases")

  x <- create_indicator(
    x, data_name = "cases", indicator_name = "rate",
    formula = cases * 10, coords = c("lon", "lat"), coords_crs = 4326
  )

  expect_true("rate" %in% names(x$data$indicators))
  ind <- x$data$indicators$rate
  expect_s3_class(ind, "sf")
  expect_equal(ind$rate, dummy$cases * 10)
})

test_that("create_indicator transforms coordinates into the analytical CRS (not raw lon/lat)", {
  x <- make_test_study_area()
  dummy <- make_test_points()
  tmp <- tempfile(fileext = ".csv")
  write.csv(dummy, tmp, row.names = FALSE)
  x <- read_data(x, file = tmp, name = "cases")

  x <- create_indicator(
    x, data_name = "cases", indicator_name = "rate",
    formula = cases * 10, coords = c("lon", "lat"), coords_crs = 4326
  )

  coords <- sf::st_coordinates(x$data$indicators$rate)
  expect_true(all(abs(coords[, 1]) > 1000))
})

test_that("create_indicator errors when the referenced dataset does not exist", {
  x <- make_test_study_area()
  expect_error(
    create_indicator(x, data_name = "missing", indicator_name = "rate", formula = 1),
    "not found"
  )
})

test_that("create_indicator aggregates when aggregate_by is supplied", {
  x <- make_test_study_area()
  dummy <- make_test_points(n = 20)
  dummy$zone <- rep(c("A", "B"), each = 10)
  tmp <- tempfile(fileext = ".csv")
  write.csv(dummy, tmp, row.names = FALSE)
  x <- read_data(x, file = tmp, name = "cases")

  x <- create_indicator(
    x, data_name = "cases", indicator_name = "zone_mean",
    formula = cases, coords = c("lon", "lat"), coords_crs = 4326,
    aggregate_by = "zone", agg_fun = "mean"
  )

  expect_equal(nrow(x$data$indicators$zone_mean), 2)
})

