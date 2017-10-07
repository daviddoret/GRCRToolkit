if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

# REWRITE THIS FUNCTION TO FIT GENERALIZED POISSON
# FIRST FIND ADEQUATE R PACKAGE

#' fit_gpd_range
#'
#' Finds the lambda and theta parameters of a generalized poisson distribution
#' that best matches the desired range quantiles.
#'
#' @param search_range_start the minimal value of the range where the mode will be searched
#'
#' @param search_range_end the maximal value of the range where the mode will be searched
#'
#' @return the estimated parameters
#'
#' @examples
#' N/A
#'
#' @export
fit_gpd_range = function(
  estimated_mode = NULL,
  estimated_range_max_value = NULL,
  estimated_range_size_proba = NULL,
  verbosity = NULL,
  ...) {

  estimated_range_min_proba <- (1 - estimated_range_size_proba) / 2
  estimated_range_max_proba <- 1 - (1 - estimated_range_size_proba) / 2
  print(cat(estimated_range_min_proba, estimated_range_max_proba))

  # Prepare a log-Maximum Likelihood Estimation function
  log_mle <- function(lambda, ...) {
    #range_min_proba <- ppois(q = estimated_range_min_value, lambda = lambda)
    range_max_proba <- ppois(q = estimated_range_max_value, lambda = lambda)
    range_max_delta <- abs(range_max_proba - estimated_range_max_proba)
    print(cat(lambda,estimated_range_max_proba,range_max_proba,range_max_delta))
    return(
      range_max_delta #+ log(range_max_proba)
    )
  }

  # Minimize the log-MLE
  lambda <- minimize_function(
    f = log_mle,
    search_range_start = 0,
    search_range_end = 10,
    verbosity = verbosity)

  return(lambda)
}
