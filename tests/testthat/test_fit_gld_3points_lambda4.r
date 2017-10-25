if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit)

require(GRCRToolkit)

context("fit_gld_3points_lambda4")

test_that("Known result #1", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 1,
        lambda3 = -1,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .05
        ),3
      ),
    -962.25)
})

test_that("Known result #2", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 10,
        lambda3 = -1,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .05
      ),3
    ),
    -591.322)
})

test_that("Known result #3", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = .01,
        lambda3 = -1,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .05
      ),3
    ),
    -988.277)
})

test_that("Known result #4", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 1,
        lambda3 = -1,
        estimated_range_max_value = 1900,
        estimated_range_max_proba = .05
      ),3
    ),
    -651.619)
})

test_that("Known result #5", {
  expect_equal(
    round(
      fit_gld_3points_lambda4(
        lambda1 = 1000,
        lambda2 = 1,
        lambda3 = -10,
        estimated_range_max_value = 1100,
        estimated_range_max_proba = .05
      ),3
    ),
    -892.744)
})
