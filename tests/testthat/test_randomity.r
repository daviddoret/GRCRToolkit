if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit)

context("randomity")

test_that("rand returns a vector of correct length", {
  expect_equal(length(rand$get(n = 32, min = 0, max = 10)), 32)
})

test_that("rand returns values within the expected range", {

  for (i in 1:32) {

    test_min <- runif(n = 1, min = 0, max = 100)
    test_max <- test_min + runif(n = 1, min = 0, max = 100)
    test_pseudo_random <- rand$get(n = 1, min = test_min, max = test_max)
    expect_gte(test_pseudo_random, test_min)
    expect_lte(test_pseudo_random, test_max)

  }

})
