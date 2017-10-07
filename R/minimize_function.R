if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

#' minimize_function
#'
#' A friendly wrapper to find a function's min
#'
#' @param f a function accepting a single parameter.
#'
#' @param search_range_start the minimal value of the range where the mode will be searched
#'
#' @param search_range_end the maximal value of the range where the mode will be searched
#'
#' @param verbosity 0: no output (default), >0: more output
#'
#' @return the guessed parameter value that minimizes the function output
#'
#' @examples
#' minimize_function(f = function(x) { return(1 / (x - 17)) }, search_range_start = -100, search_range_end = 100, verbosity = 1)
#'
#' @export
minimize_function = function(
  f = NULL,
  search_range_start = NULL,
  search_range_end = NULL,
  tolerance = NULL,
  verbosity = NULL,
  ...) {

  return(

    maximize_function(
      f = function(x, ...) {
        return(-f(x, ...))
      }
      ,search_range_start = search_range_start
      ,search_range_end = search_range_end
      ,tolerance = tolerance
      ,verbosity = verbosity
    )

  )
}
