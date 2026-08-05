test_that("save_cystimap and load_cystimap round-trip a live SpatRaster", {
  x <- make_classified_x()
  tmp <- tempfile(fileext = ".rds")

  save_cystimap(x, tmp)
  restored <- load_cystimap(tmp)

  expect_s3_class(restored, "cystimap")
  expect_true(inherits(restored$surfaces$s1$prediction, "SpatRaster"))
  # confirm the pointer is genuinely live, not stale (this would error on a dangling pointer)
  expect_true(is.numeric(terra::values(restored$surfaces$s1$prediction, na.rm = TRUE)))
})

test_that("export_results writes rasters, tables, and a manifest", {
  x <- make_classified_x()
  x <- classify_surface(x, surface_name = "s1", classified_name = "c1",
                         type = "binary", cutoff_method = "quantile", cutoffs = 0.5)
  out_dir <- file.path(tempdir(), paste0("export_test_", as.integer(Sys.time())))

  manifest <- export_results(x, output_dir = out_dir, what = c("surfaces", "classified", "log"))

  expect_true(file.exists(file.path(out_dir, "manifest.csv")))
  expect_true(nrow(manifest) > 0)
  expect_true(any(grepl("surface_s1_prediction.tif", manifest$file)))
})

test_that("export_results creates the output directory if it does not exist", {
  x <- make_classified_x()
  out_dir <- file.path(tempdir(), paste0("export_new_", as.integer(Sys.time())))
  expect_false(dir.exists(out_dir))

  export_results(x, output_dir = out_dir, what = "surfaces")
  expect_true(dir.exists(out_dir))
})

