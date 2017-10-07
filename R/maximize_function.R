if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

#' maximize_function
#'
#' A friendly wrapper to find a function's max.
#'
#' @param f a function accepting a single parameter.
#'
#' @param search_range_start the minimal value of the range where the mode will be searched
#'
#' @param search_range_end the maximal value of the range where the mode will be searched
#'
#' @param verbosity 0: no output (default), >0: more output
#'
#' @return the guessed parameter value that maximizes the function output
#'
#' @examples
#' maximize_function(f = function(x) { return(1 / (x - 17)) }, search_range_start = -100, search_range_end = 100, verbosity = 1)
#'
#' @export
maximize_function = function(
  f = NULL,
  search_range_start = NULL,
  search_range_end = NULL,
  tolerance = NULL,
  verbosity = NULL,
  ...) {

  if (is.null(tolerance)) { tolerance <- .Machine$double.eps ^ 0.25 }
  if (is.null(verbosity)) { verbosity <- 0 }

  optimize_wrapper <- NULL
  if (verbosity == 0) {
    optimize_wrapper <- function(...) {
      suppressWarnings(optimize(...))
    }
  }
  else
  {
    optimize_wrapper <- optimize
  }

  optimization <- optimize_wrapper(
    f = f,
    interval = c(search_range_start,search_range_end),
    maximum = TRUE,
    tol = tolerance,
    ...)

  if(is.null(optimization$maximum))
  {
    warning("Ooops, maximization failed miserably...")
  }

  return(optimization$maximum)
}
