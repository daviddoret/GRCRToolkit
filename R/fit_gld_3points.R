require(gld)

#' fit_gld_3points
#'
#' Part of the fit_[distribution]_[method] function series.
#' Finds the lambda parameters of the generalized lambda distribution (aka Tukey distribution) that best matches the input 3 points estimate.
#'
#' @param estimated_range_min_value The lower value of the 3 points estimate.
#' @param estimated_mode_value The mode or "typical" value of the 3 points estimate.
#' @param estimated_range_max_value The upper value of the 3 points estimate.
#' @param estimated_range_size_proba Default: .9. Possible values: 0 < x < 1. The size of the probabilistic range estimate. The default .9 leaves .05 on both sides of the distribution.
#'
#' @return the lambda parameters of the generalized lambda distribution that yields the estimated 3 points.
#'
#' @examples
#' xxxx(estimated_mode = 17.35)
#'
#' @export
fit_gld_3points = function(
  estimated_range_min_value = NULL,
  estimated_mode_value = NULL,
  estimated_range_max_value = NULL,
  estimated_range_size_proba = NULL,
  verbosity = NULL,
  max_iteration = NULL,
  precision = NULL,
  ...) {

  # Default values
  if (is_nanull(estimated_range_size_proba)) {
    estimated_range_size_proba <- .9 # TODO: replace with a default configuration setting
  }
  if (is_nanull(verbosity)) { verbosity <- 0 }
  if (is_nanull(max_iteration)) { max_iteration <- 256 }
  if (is_nanull(precision)) { precision <- 1 } # Expressed in quantile value

  estimated_range_min_proba <- (1 - estimated_range_size_proba) / 2
  estimated_range_max_proba <- 1 - (1 - estimated_range_size_proba) / 2

  # Unfortunately, I couldn't find an out-of-the-box optimization
  # function that would solve this with an abritrary precision.
  # So I quickly developed here my own. But it is probably
  # not top-efficient so if anyone finds a cool and efficient
  # algorithm to get this right faster, all the best.

  # Some observations on GLD:
  # When we tweak lambda4, this makes the mode to naturally drift.
  # If we tweak it to make the right tail fatter, mode drifts to the left.
  # If we tweak it to make the right tail thinner, mode drifts to the right.
  # So when we tweak lambda4 searching for the right tail steepness,
  # we must simultaneously compensate the distribution location
  # to keep its mode at our estimated value (and other parameters stable).
  # Similarly lambda3 make our first quantile estimate to drift as well.

  # Conclusion:
  # So I decided to develop my ad hoc optimization function
  # using a roundtrip approach where I adapt location, scale,
  # left skew with lambda 3, right skew with lambda 4,
  # and turn around like this until I get a distribution
  # that matches my estimated parameters.

  # In practice it seems to work find, more extensive
  # testing would be desirable.

  # First, we start from a clean and blank page:
  lambda1 <- estimated_mode_value
  lambda2 <- 1
  lambda3 <- -1
  lambda4 <- -1

  lambda1 <- fit_gld_3points_lambda1(
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    lambda4 = lambda4,
    estimated_range_min_value = estimated_range_min_value,
    estimated_mode_value = estimated_mode_value,
    estimated_range_max_value = estimated_range_max_value,
    verbosity = verbosity - 1,
    ...)

  lambda2 <- fit_gld_3points_lambda2(
    estimated_range_min_value = estimated_range_min_value,
    estimated_mode_value = estimated_mode_value,
    estimated_range_max_value = estimated_range_max_value,
    estimated_range_size_proba = estimated_range_size_proba,
    verbosity = verbosity - 1,
    ...)

  # And then we apply our round trip approach
  for (iteration in c(1:max_iteration))
  {
    if (verbosity > 0) {message(paste0("iteration: ", iteration))}

    lambda1 <- fit_gld_3points_lambda1(
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4,
      estimated_range_min_value = estimated_range_min_value,
      estimated_mode_value = estimated_mode_value,
      estimated_range_max_value = estimated_range_max_value,
      verbosity = verbosity - 1,
      ...)

    lambda3 <- fit_gld_3points_lambda3(
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4,
      estimated_range_min_value = estimated_range_min_value,
      estimated_range_min_proba = estimated_range_min_proba,
      verbosity = verbosity - 1,
      ...)

    lambda1 <- fit_gld_3points_lambda1(
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4,
      estimated_range_min_value = estimated_range_min_value,
      estimated_mode_value = estimated_mode_value,
      estimated_range_max_value = estimated_range_max_value,
      verbosity = verbosity - 1,
      ...)

    lambda4 <- fit_gld_3points_lambda4(
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4,
      estimated_range_max_value = estimated_range_max_value,
      estimated_range_max_proba = estimated_range_max_proba,
      verbosity = verbosity - 1,
      ...)

    # Where do we stand now?

    iteration_quantile_1 <- qgl(
      p = estimated_range_min_proba,
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4)

    iteration_quantile_2 <- qgl(
      p = estimated_range_max_proba,
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4)

    iteration_mode <- get_gld_mode(
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = lambda4,
      search_range_start = estimated_range_min_value,
      search_range_end = estimated_range_max_value)

    # Where do we stand now in comparison to the 3 estimation points?

    iteration_quantile_1_delta <- abs(iteration_quantile_1 - estimated_range_min_value)
    iteration_quantile_2_delta <- abs(iteration_quantile_2 - estimated_range_max_value)
    iteration_mode_delta <- abs(iteration_mode - estimated_mode_value)

    # message(paste0("Iteration: ", iter))
    # message(paste0("Q1: target: ", self$estimated_range_min_value, ", attained: ", iteration_quantile_1, ", diff: ", iteration_quantile_1_delta))
    # message(paste0("Mode: target: ", self$estimated_mode_value, ", attained: ", iteration_mode, ", diff: ", iteration_mode_delta))
    # message(paste0("Q2: target: ", self$estimated_range_max_value, ", attained: ", iteration_quantile_2, ", diff: ", iteration_quantile_2_delta))

    if (
      iteration_quantile_1_delta < precision
      && iteration_mode_delta < precision
      && iteration_quantile_2_delta < precision
    )
    {
      if (verbosity > 0) { message("Fit GLD with 3 points estimation: success") }
      return(c(lambda1,lambda2,lambda3,lambda4))
    }
  }

  # At least, get the mode right:
  lambda1 <- fit_gld_3points_lambda1(
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    lambda4 = lambda4,
    estimated_range_min_value = estimated_range_min_value,
    estimated_mode_value = estimated_mode_value,
    estimated_range_max_value = estimated_range_max_value,
    verbosity = verbosity - 1,
    ...
  )
  if (verbosity > 0) { warning("Fit GLD with 3 points estimation: failure") }
  return(c(lambda1,lambda2,lambda3,lambda4))
}
