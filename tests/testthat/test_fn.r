require(testthat)
require(grctoolkit)

context("fn")

test_that("check a sample of typical values", {
  expect_equal(fn(12.345,2), "~12.35")
  expect_equal(fn(12.345,3), "12.345")
  expect_equal(fn(0,2), "0.00")
  expect_equal(fn(1,8), "1.00000000")
  expect_equal(fn(123456789,0), "123'456'789")
})

test_that("check vector support", {
  expect_equal(length(fn(c(1.11,2.22,3.33),1)), 3)
  expect_equal(fn(c(1.11,2.22,3.33),1)[2], "~2.2")
})
