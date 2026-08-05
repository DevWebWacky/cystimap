test_that("define_scenarios generates one scenario per alternative", {
  x <- make_scenario_x()
  expect_equal(length(x$scenarios$definitions), 3)
})

test_that("run_scenarios executes without a real combination when only 1 group exists", {
  x <- make_scenario_x()
  expect_warning(x <- run_scenarios(x), "fewer than 2 variable groups")
  expect_equal(length(x$scenarios$outputs), 3)
})

test_that("scenario_sensitivity requires run_scenarios to have been called first", {
  x <- make_scenario_x()
  expect_error(scenario_sensitivity(x), "run_scenarios")
})

test_that("scenario_agreement requires at least 2 scenarios", {
  x <- make_scenario_x()
  x$scenarios$outputs <- x$scenarios$outputs[1]
  expect_error(scenario_agreement(x), "[Aa]t least 2")
})

test_that("scenario_consensus produces count, proportion, and flag rasters", {
  x <- make_scenario_x()
  x <- suppressWarnings(run_scenarios(x))
  x <- scenario_consensus(x, outcome_value = 0, min_scenarios = 2)

  expect_true(inherits(x$results$consensus$consensus_count, "SpatRaster"))
  expect_true(inherits(x$results$consensus$consensus_proportion, "SpatRaster"))
  expect_true(inherits(x$results$consensus$consensus_flag, "SpatRaster"))
})

