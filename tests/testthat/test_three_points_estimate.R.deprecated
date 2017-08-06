# Test test 1

library(GRCRToolkit)
context("three_points_estimate")

test_that("default range size is 90", {

  estim_1 <- three_points_estimate(range_min=10, typical=500, range_max=900)
  estim_1
  expect_equal(get_range_size(estim_1), 90)
  set_range_size(estim_1, 40)
  estim_1
  expect_equal(get_range_size(estim_1), 40)

})

