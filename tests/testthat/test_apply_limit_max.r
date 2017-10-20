if (!require(pacman)) install.packages(pacman)
pacman::p_load(testthat, GRCRToolkit)

context("apply_limit_max")

test_that("manipulations on vectors yield expected vector length", {
  expect_equal(length(apply_limit_max(x = 1:10)) == 10)
  expect_equal(length(apply_limit_max(x = 1:10, limit_value = 5)) == 10)
  expect_equal(length(apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Limit")) == 10)
  expect_equal(length(apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Discard")) == 5)
  })

test_that("manipulations on vectors yield expected max", {
  expect_equal(max(apply_limit_max(x = 1:10)) == 1)
  expect_equal(max(apply_limit_max(x = 1:10, limit_value = 5)) == 5)
  expect_equal(max(apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Limit")) == 5)
  expect_equal(max(apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Discard")) == 5)
  expect_equal(max(apply_limit_max(x = rnorm(n=20), limit_value = 0, limit_behavior = "Replace", replace_function = rnorm)) == 0)
  })

test_that("manipulations on dataframes yield expected number of rows", {
  expect_equal(length(apply_limit_max(
    x = data.frame(x = 1:10, y = 2, z = rnorm(n = 10)),
    limit_value = 5,
    limit_behavior = "limit",
    target_column = "x")[["x"]]) == 10)
  expect_equal(length(apply_limit_max(
    x = data.frame(x = 1:10, y = 2, z = rnorm(n=10)),
    limit_value = 5,
    limit_behavior = "discard",
    target_column = "x")[["x"]]) == 5)
})

test_that("replacement on dataframes yield expected number of rows", {
  rf <- function(x,...){return(data.frame(x = rnorm(x,...),y = "new individuals"))}
  df_original <- data.frame(x = rnorm(12), y = "old individuals")
  df_output <- apply_limit_max(x = df_original, limit_value = 0, limit_behavior = "replace", target_column = rf)
  expect_equal(length(df_output[["x"]]) == 12)
})
