test_that("admin_summary continuous stats returns one row per polygon", {
  x <- make_classified_x()
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  x <- admin_summary(x, raster_source = "surfaces", raster_name = "s1",
                      polygons = nc, zone_id = "NAME", stats = c("mean", "min", "max"))

  tbl <- x$results$admin_summary$surfaces__s1$summary_table
  expect_equal(nrow(tbl), nrow(nc))
  expect_true("mean" %in% names(tbl))
})

test_that("admin_summary errors when zone_id column is missing", {
  x <- make_classified_x()
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_error(
    admin_summary(x, raster_source = "surfaces", raster_name = "s1",
                   polygons = nc, zone_id = "NOT_A_COLUMN"),
    "not found"
  )
})

test_that("admin_summary errors when the raster source cannot be resolved", {
  x <- make_classified_x()
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_error(
    admin_summary(x, raster_source = "surfaces", raster_name = "missing_surface",
                   polygons = nc, zone_id = "NAME"),
    "not found"
  )
})

