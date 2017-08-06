# Test test 1

library(testthat)
library(GRCRToolkit)
context("fair_factor")

test_that("basic stuff", {
  f1 <- fair_factor$new(range_min=10, typical=500, range_max=900, range_size=.9)
  expect_equal(f1$range_size, .9)
  #f1$set_range_size <- .95
  #expect_equal(f1$range_size, .95)
})

