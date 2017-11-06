require(testthat)
require(grctoolkit)

context("fit_gld_3points_lambda2")

test_that("Known result #1", {
  expect_equal(
    round(
      fit_gld_3points_lambda2(
        estimated_range_min_value = 100,
        estimated_mode_value = 180,
        estimated_range_max_value = 200,
        estimated_range_size_proba = .9
        ),3
      ),
    0.947)
})

test_that("Known result #2", {
  expect_equal(
    round(
      fit_gld_3points_lambda2(
        estimated_range_min_value = 0,
        estimated_mode_value = 10,
        estimated_range_max_value = 200,
        estimated_range_size_proba = .9
      ),3
    ),
    1.895)
})

test_that("Known result #3", {
  expect_equal(
    round(
      fit_gld_3points_lambda2(
        estimated_range_min_value = .01,
        estimated_mode_value = .08,
        estimated_range_max_value = .1,
        estimated_range_size_proba = .9
      ),3
    ),
    947.368)
})

test_that("Known result #4", {
  expect_equal(
    round(
      fit_gld_3points_lambda2(
        estimated_range_min_value = 1000000,
        estimated_mode_value = 1990000,
        estimated_range_max_value = 2000000,
        estimated_range_size_proba = .95
      ),3
    ),
    0.004)
})
