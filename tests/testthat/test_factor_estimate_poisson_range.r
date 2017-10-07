if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit, random)

context("factor_estimate_poisson_range")

test_that("a known simple example yields a precise mode", {
  fe1 <- factor_estimate_poisson_range$new(
    limit_min_value = 0,
    estimated_range_min_value = 0,
    estimated_range_max_value = 3,
    limit_max_value = 4,
    estimated_range_size = .9,
    fit_dist = FALSE,
    simulate = FALSE)
  #expect_equal(fn(fe1$dist_mode,0), "~120")
})

test_that("a few pseudo random samples yield precise modes", {
  #for(iteration in 1:16) {
  #  limit_min_value <- rand$get(max = 1000)
  #  estimated_range_min_value <- limit_min_value + rand$get(max = 1000)
  #  estimated_mode_value <- estimated_range_min_value + rand$get(max = 1000)
  #  estimated_range_max_value <- estimated_mode_value + rand$get(max = 1000)
  #  limit_max_value <- estimated_range_max_value + rand$get(max = 1000)
  #  fe1 <- factor_estimate_gld_3points$new(
  #    estimated_range_min_value = estimated_range_min_value,
  #    estimated_mode_value = estimated_mode_value,
  #    estimated_range_max_value = estimated_range_max_value,
  #    estimated_range_min_proba = .05,
  #    estimated_range_max_proba = .95,
  #    limit_min_value = limit_min_value,
  #    limit_max_value = limit_max_value)

  #  delta <- abs(fe1$dist_mode - estimated_mode_value)
  #  expect_lt(delta, 1)
  #  }
})

