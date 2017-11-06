require(testthat)
require(grctoolkit)

context("get_gld_mode")

test_that("Mode = lambda1 when shape parameters are neutral and lambda2 does not approach 0", {
  expect_equal(get_gld_mode(lambda1 = 100, lambda2 = 100, lambda3 = -1, lambda4 = -1, search_range_start = -1000, search_range_end = 1000), 100)
  expect_equal(get_gld_mode(lambda1 = -500, lambda2 = 327, lambda3 = -1, lambda4 = -1, search_range_start = -1000, search_range_end = 1000), -500)
  expect_equal(get_gld_mode(lambda1 = 378, lambda2 = 17.345, lambda3 = -1, lambda4 = -1, search_range_start = -1000, search_range_end = 1000), 378)
})

test_that("Mode shifts to greater values than lambda1 when lambda3 reaches lower negative values", {
  expect_gt(get_gld_mode(lambda1 = 100, lambda2 = .1, lambda3 = -1000, lambda4 = -1, search_range_start = -1000000, search_range_end = 1000000), 100)
})

test_that("Mode shifts to lower values than lambda1 when lambda3 approaches 0", {
  expect_lt(get_gld_mode(lambda1 = 100, lambda2 = .1, lambda3 = -.001, lambda4 = -1, search_range_start = -1000000, search_range_end = 1000000), 100)
})

test_that("Mode shifts to lower values than lambda1 when lambda4 reaches lower negative values", {
  expect_lt(get_gld_mode(lambda1 = 100, lambda2 = .1, lambda3 = -1, lambda4 = -1000, search_range_start = -1000000, search_range_end = 1000000), 100)
})

test_that("Mode shifts to higher values than lambda1 when lambda4 approaches 0", {
  expect_gt(get_gld_mode(lambda1 = 100, lambda2 = .1, lambda3 = -1, lambda4 = -.001, search_range_start = -1000000, search_range_end = 1000000), 100)
})
