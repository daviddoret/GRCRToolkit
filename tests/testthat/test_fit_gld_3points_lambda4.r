require(gld)
require(testthat)

context("fit_gld_3points_lambda4")

test_that("Known result #1", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 1,
        lambda3 = -1,
        lambda4 = -1,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .95
        ),3
      ),
    -1.72)
})

test_that("Known result #2", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 10,
        lambda3 = -1,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .95
      ),3
    ),
    -2.629)
})

test_that("Known result #3", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = .05,
        lambda3 = -1,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .95
      ),8
    ),
    -0.3231138)
})

test_that("Known result #4", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 1,
        lambda3 = -1,
        estimated_range_max_value = 1900,
        estimated_range_max_proba = .95
      ),3
    ),
    -2.588)
})

test_that("Known result #5", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 1,
        lambda3 = -10,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .95
      ),3
    ),
    -1.721)
})
