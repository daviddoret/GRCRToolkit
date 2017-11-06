require(testthat)
require(grctoolkit)

context("fit_gld_3points_lambda3")

test_that("Known result #1", {
  expect_equal(
    round(
      fit_gld_3points_lambda3(
        lambda1 = 1000,
        lambda2 = 1,
        lambda4 = -1,
        estimated_range_min_value = 100,
        estimated_range_min_proba = .05
        ),3
      ),
    -2.588)
})

test_that("Known result #2", {
  expect_equal(
    round(
      fit_gld_3points_lambda3(
        lambda1 = 1000,
        lambda2 = 10,
        lambda4 = -1,
        estimated_range_min_value = 100,
        estimated_range_min_proba = .05
      ),3
    ),
    -3.453)
})

test_that("Known result #3", {
  expect_equal(
    round(
      fit_gld_3points_lambda3(
        lambda1 = 1000,
        lambda2 = .01,
        lambda4 = -1,
        estimated_range_min_value = 100,
        estimated_range_min_proba = .05
      ),3
    ),
    -0.639)
})

test_that("Known result #4", {
  expect_equal(
    round(
      fit_gld_3points_lambda3(
        lambda1 = 1000,
        lambda2 = 1,
        lambda4 = -1,
        estimated_range_min_value = 900,
        estimated_range_min_proba = .05
      ),3
    ),
    -1.72)
})

test_that("Known result #5", {
  expect_equal(
    round(
      fit_gld_3points_lambda3(
        lambda1 = 1000,
        lambda2 = 1,
        lambda4 = -10,
        estimated_range_min_value = 100,
        estimated_range_min_proba = .05
      ),3
    ),
    -2.588)
})
