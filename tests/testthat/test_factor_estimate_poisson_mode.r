if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit, random)

context("factor_estimate_poisson_mode")

test_that("a known simple example yields a precise mode", {
  fe1 <- factor_estimate_poisson_mode$new(
    limit_min_value = 0,
    estimated_mode_value = 0.1,
    limit_max_value = 3.14,
    fit_distribution = TRUE,
    simulate = FALSE)
  expect_equal(fe1$dist_mode, fe1$estimated_mode_value)
})

test_that("a few pseudo random samples with 0 < lambda < 1 yield precise modes", {
  for(iteration in 1:16) {
    limit_min_value <- 0
    estimated_mode_value <- limit_min_value + rand$get(max = 1)
    limit_max_value <- estimated_mode_value + rand$get(max = 100)
    fe1 <- factor_estimate_poisson_mode$new(
      estimated_mode_value = estimated_mode_value,
      limit_min_value = limit_min_value,
      limit_max_value = limit_max_value)

    delta <- abs(fe1$dist_mode - estimated_mode_value)
    expect_lt(delta, 0.00001) # TODO: This is arbitrary
    }
})

test_that("a few pseudo random samples with larger lambdas yield precise modes", {
  for(iteration in 1:16) {
    limit_min_value <- rand$get(max = 10)
    estimated_mode_value <- limit_min_value + rand$get(max = 100)
    limit_max_value <- estimated_mode_value + rand$get(max = 1000)
    fe1 <- factor_estimate_poisson_mode$new(
      estimated_mode_value = estimated_mode_value,
      limit_min_value = limit_min_value,
      limit_max_value = limit_max_value)

    delta <- abs(fe1$dist_mode - estimated_mode_value)
    expect_lt(delta, 0.00001) # TODO: This is arbitrary
  }
})
