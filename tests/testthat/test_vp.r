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

test_that("Unacceptable values are unacceptable", {
  expect_error(vp("z", NULL, "character", 1, c("a", "b", "c")))
})

test_that("Unacceptable values in vectors are unacceptable", {
  expect_error(vp(c("a", "z"), NULL, "character", 1, c("a", "b", "c")))
})

test_that("Values below minimum limit raise error", {
  expect_error(vp(5, NULL, "numeric", 1, limit_min = 7))
})

test_that("Values above maximum limit raise error", {
  expect_error(vp(9, NULL, "numeric", 1, limit_max = 7))
})
