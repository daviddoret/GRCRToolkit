
#' fit_gld_3points_lambda1
#'
#' This function optimizes the lambda1 (location) parameter of a GLD distribution to further fit a 3 points estimate.
#' \cr This is an "internal" function called by fit_gld_3points.
#'
#' @inheritParams fit_gld_3points
#'
#' @return A numeric vector of size 1 representing an optimized lambda1 (location) parameter that further fits a 3 points estimate.
#'
#' @export
fit_gld_3points_lambda1 = function(
  lambda1,
  lambda2,
  lambda3,
  lambda4,
  estimated_range_min_value,
  estimated_mode_value,
  estimated_range_max_value,
  verbosity = NULL,
  ...) {
  # move the fitted distribution to the
  # position where its mode (peak) coincidate
  # with the expert estimated mode.

  if (is_nanull(verbosity)) { verbosity <- 0 }

  current_mode <- get_gld_mode(
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    lambda4 = lambda4,
    search_range_start = estimated_range_min_value,
    search_range_end = estimated_range_max_value,
    ...)

  # Find the difference between the mode (peak)
  # of the currently fitted distribution with
  # the desired mode coming from the expert
  # estimate
  delta <- estimated_mode_value - current_mode

  # Move the fitted distribution to compensate
  # for this difference
  new_lambda1 <- lambda1 + delta

  if (verbosity > 0) { message(paste0("lambda1: ", new_lambda1)) }

  return(new_lambda1)

}
