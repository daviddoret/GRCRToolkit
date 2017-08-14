#' probability_mass_function
#'
#' This is a PMF wrapper function.
#' It receives the distribution name as a parameter, making the function distribution-dynamic.
#' Distribution parameters passed in ... are simply transmitted to the underlying distribution function.
#'
#' @param x a vector of quantiles
#'
#' @return a vector of probabilities
#'
#' @examples
#' probability_mass_function('poissonpert',x=c(1,5,7), lambda=3)
#'
#' @export
probability_mass_function <- function(dist_name, x, ...) {
  if(dist_name == "poissonpert") {
    dpois(x = x, ...)
  }
}
