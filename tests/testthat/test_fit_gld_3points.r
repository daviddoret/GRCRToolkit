require(testthat)
require(grctoolkit)

context("fit_gld_3points")

test_that("Known result #1", {
  expect_true(
    all(
      round(
        fit_gld_3points(
          estimated_range_min_value = 100,
          estimated_mode_value = 150,
          estimated_range_max_value = 200,
          estimated_range_size_proba = .9
          ),2
        ) == c(150,0.38,-1,-1)
      ))
})

test_that("Known result #2", {
  expect_true(
    all(
      round(
        fit_gld_3points(
          estimated_range_min_value = 100,
          estimated_mode_value = 180,
          estimated_range_max_value = 200,
          estimated_range_size_proba = .9
        ),2
      ) == c(179.75, 0.95, -1.61, -1.01)
    ))
})

test_that("Known result #3", {
  expect_true(
    all(
      round(
        fit_gld_3points(
          estimated_range_min_value = 145,
          estimated_mode_value = 150,
          estimated_range_max_value = 155,
          estimated_range_size_proba = .9
        ),2
      ) == c(150.00, 3.79, -1.00, -1.00)
    ))
})


test_that("Known result #4", {
  expect_true(
    all(
      round(
        fit_gld_3points(
          estimated_range_min_value = 100,
          estimated_mode_value = 120,
          estimated_range_max_value = 200,
          estimated_range_size_proba = .9
        ),2
      ) == c(120.00, 0.95, -1.00, -1.61)
    ))
})
