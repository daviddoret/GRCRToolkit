require(testthat)
require(grctoolkit)
require(random)

context("factor_estimate_gld_3points")

test_that("a known simple example yields a precise mode", {
  fe1 <- factor_estimate_gld_3points$new(
    limit_min_value = 10,
    estimated_range_min_value = 100,
    estimated_mode_value = 120,
    estimated_range_max_value = 200,
    limit_max_value = 10000,
    estimated_range_size_proba = .9)
  expect_equal(fn(fe1$dist_mode,0), "~120")
})

test_that("a few pseudo random samples yield precise modes", {
  for (iteration in 1:4) {
    limit_min_value <- rand$get(max = 1000)
    print(paste0("limit_min_value: ", limit_min_value))
    estimated_range_min_value <- limit_min_value + rand$get(max = 1000)
    print(paste0("estimated_range_min_value: ", estimated_range_min_value))
    estimated_mode_value <- estimated_range_min_value + rand$get(max = 1000)
    print(paste0("estimated_mode_value: ", estimated_mode_value))
    estimated_range_max_value <- estimated_mode_value + rand$get(max = 1000)
    print(paste0("estimated_range_max_value: ", estimated_range_max_value))
    limit_max_value <- estimated_range_max_value + rand$get(max = 1000)
    print(paste0("limit_max_value: ", limit_max_value))
    fe1 <- factor_estimate_gld_3points$new(
      estimated_range_min_value = estimated_range_min_value,
      estimated_mode_value = estimated_mode_value,
      estimated_range_max_value = estimated_range_max_value,
      estimated_range_size_proba = .9,
      limit_min_value = limit_min_value,
      limit_max_value = limit_max_value,
      verbosity = 2)

    delta <- abs(fe1$dist_mode - estimated_mode_value)
    expect_lt(delta, 1)
    }
})

