test_that("combine_surfaces binary_overlap sums 0/1 layers correctly", {
  x <- make_combine_x()
  x <- combine_surfaces(x, classified_names = c("c1", "c2"), combined_name = "ov",
                         method = "binary_overlap")

  r <- x$results$combined$ov$combined_raster
  vals <- sort(unique(terra::values(r, na.rm = TRUE)))
  expect_true(all(vals %in% c(0, 1, 2)))
})

test_that("combine_surfaces logical AND matches overlap value 2", {
  x <- make_combine_x()
  x <- combine_surfaces(x, classified_names = c("c1", "c2"), combined_name = "ov",
                         method = "binary_overlap")
  x <- combine_surfaces(x, classified_names = c("c1", "c2"), combined_name = "and1",
                         method = "logical", logical_rule = "AND")

  overlap_area <- x$results$combined$ov$frequencies
  and_area <- x$results$combined$and1$frequencies
  expect_equal(
    overlap_area$count[overlap_area$value == 2],
    and_area$count[and_area$value == 1]
  )
})

test_that("combine_surfaces errors with fewer than 2 classified surfaces", {
  x <- make_combine_x()
  expect_error(
    combine_surfaces(x, classified_names = "c1", combined_name = "bad", method = "binary_overlap"),
    "[Aa]t least two"
  )
})

test_that("combine_surfaces weighted method warns when weights do not sum to 1", {
  x <- make_combine_x()
  expect_warning(
    combine_surfaces(x, classified_names = c("c1", "c2"), combined_name = "w1",
                      method = "weighted", weights = c(c1 = 0.3, c2 = 0.3)),
    "sum to"
  )
})

