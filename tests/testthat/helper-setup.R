# Shared test fixtures - re-created fresh in each test to avoid
# terra pointer/environment issues across test files.

make_test_study_area <- function() {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  prepare_study_area(boundary = nc, crs = 32119, resolution = 20000, dissolve = TRUE)
}

make_test_points <- function(n = 30, seed = 1) {
  set.seed(seed)
  data.frame(
    id    = 1:n,
    lon   = stats::runif(n, -81, -78),
    lat   = stats::runif(n, 34.5, 36),
    cases = stats::rpois(n, 10)
  )
}

