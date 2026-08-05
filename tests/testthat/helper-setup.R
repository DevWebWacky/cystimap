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

make_combine_x <- function() {
  x <- make_classified_x()
  x <- classify_surface(x, surface_name = "s1", classified_name = "c1",
                         type = "binary", cutoff_method = "quantile", cutoffs = 0.5)
  x <- classify_surface(x, surface_name = "s1", classified_name = "c2",
                         type = "binary", cutoff_method = "quantile", cutoffs = 0.25)
  x
}

make_scenario_x <- function() {
  x <- make_combine_x()
  x <- classify_surface(x, surface_name = "s1", classified_name = "c3",
                         type = "binary", cutoff_method = "quantile", cutoffs = 0.75)
  define_scenarios(x, variable_groups = list(choice = c("c1", "c2", "c3")),
                    combine_method = "binary_overlap")
}

