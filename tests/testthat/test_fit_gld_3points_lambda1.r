if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit)

context("fit_gld_3points_lambda1")

test_that("Known result #1", {
  expect_equal(
    round(
      fit_gld_3points_lambda1(
        lambda1 = 1,
        lambda2 = 1,
        lambda3 = -1,
        lambda4 = -1,
        estimated_range_min_value = 100,
        estimated_mode_value = 150,
        estimated_range_max_value = 200
        ),0
      ),
    51)
})

test_that("Known result #2", {
  expect_equal(
    round(
      fit_gld_3points_lambda1(
        lambda1 = 1,
        lambda2 = 1,
        lambda3 = -1000,
        lambda4 = -1,
        estimated_range_min_value = 100,
        estimated_mode_value = 150,
        estimated_range_max_value = 200
        ),
      0
      ),
    32)
})

test_that("Known result #3", {
  expect_equal(
    round(
      fit_gld_3points_lambda1(
        lambda1 = 12345,
        lambda2 = 345,
        lambda3 = -.678,
        lambda4 = -12,
        estimated_range_min_value = 100,
        estimated_mode_value = 150,
        estimated_range_max_value = 200
      ),
      0
    ),
    12295)
})

test_that("Known result #4", {
  expect_equal(
    round(
      fit_gld_3points_lambda1(
        lambda1 = 12345,
        lambda2 = 345,
        lambda3 = -.678,
        lambda4 = -12,
        estimated_range_min_value = 100,
        estimated_mode_value = 190,
        estimated_range_max_value = 200
      ),
      0
    ),
    12335)
})
