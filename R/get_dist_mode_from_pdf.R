if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

#' get_dist_mode_from_pdf
#'
#' Shortcut function to find the mode of an arbitrary probability density function (PDF).
#' Uses optimization, i.e. may lead to imprecise results with exotic input functions.
#' It is helpful when an analytic solution is not handily available.
#'
#' @param pdf the input probability density function (PDF)
#'
#' @param search_range_start the minimal value of the range where the mode will be searched
#'
#' @param search_range_end the maximal value of the range where the mode will be searched
#'
#' @return the estimated mode
#'
#' @examples
#' get_dist_mode_from_pdf(function(x){return(dnorm(x = x, mean = 17, sd = 5))}, -100, 100)
#'
#' @export
get_dist_mode_from_pdf = function(
  pdf,
  search_range_start,
  search_range_end,
  precision = NULL,
  verbosity = NULL,
  ...) {

  if(is.null(precision)) { precision <- 1 }
  if(is.null(verbosity)) { verbosity <- 0 }

#  # Declare the minimization function
#  minimization_function <- function(x){
#    result <- pdf(x)
#    #if(result == 0) { return(Inf)  }
#    return(result)
#  }
#
#  # Run the optimization
#  optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128, print.level = verbosity)

  optimize_wrapper <- NULL
  if(verbosity == 0) {
    optimize_wrapper <- function(...) {
      suppressWarnings(optimize(...))
    }
  }
  else
  {
    optimize_wrapper <- optimize
  }

  optimization <- optimize_wrapper(
    f = pdf,
    interval = c(search_range_start,search_range_end),
    maximum = TRUE,
    tol = 1)

  if(is.null(optimization$maximum))
  {
    warning("Ooops, optimize() failed...")
  }

  return(optimization$maximum)
}
