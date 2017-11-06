require(testthat)

context("validate_parameter, vp")

test_that("Basic default values are applied", {
  expect_equal(vp(NULL, 2, "numeric"), 2)
  expect_equal(vp(NA, 54, "numeric"), 54)
  expect_equal(vp(NULL, "abc", "character"), "abc")
})

test_that("Error conditions raise errors", {
  expect_error(vp("hello", NULL, "numeric"))
})
