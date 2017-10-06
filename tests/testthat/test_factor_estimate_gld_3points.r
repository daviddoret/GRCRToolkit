if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit)

context("factor_estimate_gld_3points")

test_that("check a know example", {
  fe1 <- factor_estimate_gld_3points$new(
    estimated_range_min_value = 100,
    estimated_mode_value = 120,
    estimated_range_max_value = 200,
    estimated_range_min_proba = .05,
    estimated_range_max_proba = .95,
    limit_min_value = 10,
    limit_max_value = 10000)

  expect_equal(fn(fe1$dist_mode,0), "~120")
})

