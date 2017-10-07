if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

#' fit_poisson_mode
#'
#' Finds the lambda parameter of a poisson distribution that best matches the estimated mode.
#' This function is trivial but was written for consistency purposes with the other fitting functions.
#'
#' @param estimated_mode_value the estimated mode of the unkwown distribution
#'
#' @return the lambda parameter of the poisson distribution that yields the estimated mode
#'
#' @examples
#' fit_poisson_lambda(estimated_mode = 17.35)
#'
#' @export
fit_poisson_mode = function(
  estimated_mode_value = NULL,
  verbosity = NULL,
  ...) {

  return(estimated_mode_value)
}
