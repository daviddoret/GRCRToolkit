#' find_function_1param_value_divideby2
#'
#' Given \code{f(x) = y} where \code{f} is a monotonic function, find \code{x} for a given \code{y}.
#' \cr Use a naive iterative approach dividing and mulitplying steps by 2 until a solution is found within desired precision.
#' \cr Non-monotic functions are supported when the search range is limited to a monotonic subset of the function's range.
#' \cr WARNING: This algorithm is naive, i.e. performance-wise suboptimal.
#'
#' @param f A function receiving a numeric value as its first parameter followed by ...
#'
#' @param y_target_value The output of \code{f(x)} for which we want to find \code{x}.
#'
#' @param x_first_guess (Conditional) An initial best-effort first guess of what \code{x} could be.
#'
#' @param x_first_step (Conditional) An initial step length to find better better values.
#'
#' @param x_search_limit_min (Conditional) A strict limit below which \code{x} will not be searched.
#'
#' @param x_search_limit_max (Conditional) A strict limit above which \code{x} will not be searched.
#'
#' @param y_precision (Conditional) An acceptable degree of precision. Default: .01.
#'
#' @param max_iteration (Conditional) The maximum number of search loops performed by the function. This is a safeguard to avoid infinite loops. Default: 2048.
#'
#' @param verbosity (Conditional) When \code{verbosity > 0} detailed information will be output to the console.
#'
#' @return Numeric vector. \code{x} such that \code{f(x) = y_target_value}.
#'
#' @examples
#' precision <- .0001
#' y_target <- runif(n = 1, min = -1000000, max = +1000000)
#' c <- runif(n = 1, min = -1000000, max = +1000000)
#' m <- runif(n = 1, min = -1000000, max = +1000000)
#' f <- function(x) { return(c + m * x) }
#' x_found <- find_function_1param_value_divideby2(
#'   f = f,
#'   y_target_value = y_target,
#'   y_precision = precision,
#'   verbosity = verbosity)
#' y_found <- f(x_found)
#' delta <- abs(y_target - y_found)
#' label <- paste0(
#'   "f(x): c + m * x",
#'   "\nc: ", fn(c,8),
#'   "\nm: ", fn(m,8),
#'   "\ny_target: ", fn(y_target,8),
#'   "\nx_found: ", fn(x_found,8),
#'   "\ny_found: ", fn(y_found,8),
#'   "\ndelta: ", fn(delta,8))
#' message(label)
#'
#' @export
find_function_1param_value_divideby2 <- function(
  f,
  y_target_value,
  x_first_guess = NULL,
  x_first_step = NULL,
  x_search_limit_min = NULL,
  x_search_limit_max = NULL,
  y_precision = NULL,
  max_iteration = NULL,
  verbosity = NULL,
  ...) {

  # Default values
  if (is_void(y_precision)) { y_precision <- .01 }
  if (is_void(max_iteration)) { max_iteration <- 2048 }
  if (is_void(verbosity)) { verbosity <- 0 }
  if (is_void(x_first_guess)) { x_first_guess <- 0 }
  if (is_void(x_first_step)) { x_first_step <- 1 }
  y_precision <- abs(y_precision) # Precision can't be negative.

  if (verbosity > 0) { message(paste0("y_target_value: ", y_target_value)) }

  iteration <- 1
  best_x <- NULL
  best_y <- NULL
  best_delta <- NULL

  x1 <- mimax(x_first_guess - x_first_step, x_search_limit_min, x_search_limit_max)
  x2 <- mimax(x_first_guess, x_search_limit_min, x_search_limit_max)
  x3 <- mimax(x_first_guess + x_first_step, x_search_limit_min, x_search_limit_max)

  while (iteration <= max_iteration) {

    if (verbosity > 0) { message(paste0("Iteration: ", iteration)) }

    y1 <- f(x1, ...)
    y2 <- f(x2, ...)
    y3 <- f(x3, ...)

    delta1 <- abs(y1 - y_target_value)
    delta2 <- abs(y2 - y_target_value)
    delta3 <- abs(y3 - y_target_value)

    if (verbosity > 0) { message(paste0(" f(", x1, ") = ", y1,", delta = ", delta1)) }
    if (verbosity > 0) { message(paste0(" f(", x2, ") = ", y2,", delta = ", delta2)) }
    if (verbosity > 0) { message(paste0(" f(", x3, ") = ", y3,", delta = ", delta3)) }

    # Do we have a good enough match?
    if (delta1 <= y_precision) { return(x1) }
    if (delta2 <= y_precision) { return(x2) }
    if (delta3 <= y_precision) { return(x3) }

    # For first iteration only: our first point is necessarly the best point
    if (iteration == 1) {
      best_x <- x1
      best_y <- y1
      best_delta <- delta1
    }

    # Do we have a best match?
    if (delta1 <= best_delta) {
      best_x <- x1
      best_y <- y1
      best_delta <- delta1
      }
    if (delta2 <= best_delta) {
      best_x <- x2
      best_y <- y2
      best_delta <- delta2
    }
    if (delta3 <= best_delta) {
      best_x <- x3
      best_y <- y3
      best_delta <- delta3
    }

    if ((y1 < y_target_value & y_target_value < y2) |
        (y1 > y_target_value & y_target_value > y2)) {
      # y_target_value is within the first range
      if (verbosity > 0) { message("y_target_value is within the first range") }
      x1 <- mimax(x1, x_search_limit_min, x_search_limit_max)
      x3 <- mimax(x2, x_search_limit_min, x_search_limit_max)
      x2 <- mimax(x1 + (x3 - x1) / 2, x_search_limit_min, x_search_limit_max)
    } else if ((y2 < y_target_value & y_target_value < y3) |
               (y2 > y_target_value & y_target_value > y3)) {
      # y_target_value is within the second range
      if (verbosity > 0) { message("y_target_value is within the second range") }
      x1 <- mimax(x2, x_search_limit_min, x_search_limit_max)
      x3 <- mimax(x3, x_search_limit_min, x_search_limit_max)
      x2 <- mimax(x1 + (x3 - x1) / 2, x_search_limit_min, x_search_limit_max)
    } else if (y1 < y3) {
      # function is increasing
      if (verbosity > 0) { message("f is increasing") }
      if (y_target_value > y3) {
        if (verbosity > 0) { message("y_target_value is above current ranges") }
        step <- abs((x3 - x1)) * 2
        x1 <- mimax(x3, x_search_limit_min, x_search_limit_max)
        x3 <- mimax(x1 + step, x_search_limit_min, x_search_limit_max)
        x2 <- mimax(x1 + (x3 - x1) / 2, x_search_limit_min, x_search_limit_max)
      }
      else if (y_target_value < y1)
      {
        if (verbosity > 0) { message("y_target_value is below current ranges") }
        step <- abs((x3 - x1)) * 2
        x3 <- mimax(x1, x_search_limit_min, x_search_limit_max)
        x1 <- mimax(x1 - step, x_search_limit_min, x_search_limit_max)
        x2 <- mimax(x1 + (x3 - x1) / 2, x_search_limit_min, x_search_limit_max)
      }
      else {
        stop("Impossible condition: y1 < target_value < y3")
      }
    # function is decreasing
    } else if (y1 > y3) {
      if (verbosity > 0) { message("f is decreasing") }
      if (y_target_value > y1) {
        if (verbosity > 0) { message("y_target_value is above current ranges") }
        step <- abs((x3 - x1)) * 2
        x3 <- mimax(x1, x_search_limit_min, x_search_limit_max)
        x1 <- mimax(x3 - step, x_search_limit_min, x_search_limit_max)
        x2 <- mimax(x1 + (x3 - x1) / 2, x_search_limit_min, x_search_limit_max)
      } else if (y_target_value < y3) {
        if (verbosity > 0) { message("y_target_value is below current ranges") }
        step <- abs((x3 - x1)) * 2
        x1 <- mimax(x3, x_search_limit_min, x_search_limit_max)
        x3 <- mimax(x1 + step, x_search_limit_min, x_search_limit_max)
        x2 <- mimax(x1 + (x3 - x1) / 2, x_search_limit_min, x_search_limit_max)
      } else {
        stop("Impossible condition: y1 < y_target_value < y3")
      }
      # function is flat
    } else {
      stop("Impossible condition: y1 == y3")
    }

    iteration <- iteration + 1
  }

  warning(paste0(
    "No solution found within requested precision. Best solution returned instead.",
    " f(",
    best_x,
    ") = ",
    best_y,
    ". Target: ",
    y_target_value,
    ". Precision: ",
    best_delta
    ))
  return(best_x)
}
