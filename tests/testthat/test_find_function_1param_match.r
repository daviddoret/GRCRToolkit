require(testthat)

verbosity <- 0

context("find_function_1param_value_divideby2")

test_that("Solution is found with requested precision on random linear functions", {
  for (i in 1:32) {
    precision <- .0001
    y_target <- runif(n = 1, min = -1000000, max = +1000000)
    c <- runif(n = 1, min = -1000000, max = +1000000)
    m <- runif(n = 1, min = -1000000, max = +1000000)
    f <- function(x) { return(c + m * x) }
    x_found <- find_function_1param_value_divideby2(
      f = f,
      y_target_value = y_target,
      y_precision = precision,
      verbosity = verbosity)
    y_found <- f(x_found)
    delta <- abs(y_target - y_found)
    label <- paste0(
      "f(x): c + m * x",
      "\nc: ", fn(c,8),
      "\nm: ", fn(m,8),
      "\ny_target: ", fn(y_target,8),
      "\nx_found: ", fn(x_found,8),
      "\ny_found: ", fn(y_found,8),
      "\ndelta: ", fn(delta,8))
    #message(label)
    expect_lte(delta, precision, label)
  }})

test_that("Solution is found with requested precision on random cubic functions", {
  for (i in 1:32) {
    precision <- .0001
    y_target <- runif(n = 1, min = -1000000, max = +1000000)
    c <- runif(n = 1, min = -1000, max = +1000)
    m <- runif(n = 1, min = -1000, max = +1000)
    f <- function(x) { return((c + m * x) ^ 3) }
    x_found <- find_function_1param_value_divideby2(
      f = f,
      y_target_value = y_target,
      y_precision = precision,
      verbosity = verbosity)
    y_found <- f(x_found)
    delta <- abs(y_target - y_found)
    label <- paste0(
      "f(x): (c + m * x) ^ 3",
      "\nc: ", fn(c,8),
      "\nm: ", fn(m,8),
      "\ny_target: ", fn(y_target,8),
      "\nx_found: ", fn(x_found,8),
      "\ny_found: ", fn(y_found,8),
      "\ndelta: ", fn(delta,8))
    message(label)
    expect_lte(delta, precision, label)
  }})

