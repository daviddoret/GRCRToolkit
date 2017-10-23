if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

options(digits = 22)

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
  if (is.null(estimated_range_size_proba)) {
    estimated_range_size_proba <- .9 # TODO: replace with a default configuration setting
  }
  if (is.null(verbosity)) { verbosity <- 0 }
  if (is.null(max_iteration)) { max_iteration <- 256 }
  if (is.null(precision)) { precision <- 1 } # Expressed in quantile value

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

  self$fit_dist_location(verbosity = verbosity, ...)
  self$fit_dist_scale(verbosity = verbosity, ...)

  # And then we apply our round trip approach
  for(iter in c(1 : max_iteration))
  {
    self$fit_dist_location(verbosity = verbosity, ...)
    #self$fit_dist_scale(verbosity = verbosity, ...)
    self$fit_dist_left_skew(verbosity = verbosity, ...)

    self$fit_dist_location(verbosity = verbosity, ...)
    self$fit_dist_right_skew(verbosity = verbosity, ...)

    iter_q1 <- self$get_quantile(self$estimated_range_min_proba)
    iter_q2 <- self$get_quantile(self$estimated_range_max_proba)
    iter_mode <- self$dist_mode

    iter_q1_delta <- abs(iter_q1 - self$estimated_range_min_value)
    iter_q2_delta <- abs(iter_q2 - self$estimated_range_max_value)
    iter_mode_delta <- abs(self$dist_mode - self$estimated_mode_value)

    # message(paste0("Iteration: ", iter))
    # message(paste0("Q1: target: ", self$estimated_range_min_value, ", attained: ", iter_q1, ", diff: ", iter_q1_delta))
    # message(paste0("Mode: target: ", self$estimated_mode_value, ", attained: ", iter_mode, ", diff: ", iter_mode_delta))
    # message(paste0("Q2: target: ", self$estimated_range_max_value, ", attained: ", iter_q2, ", diff: ", iter_q2_delta))

    if (
      iter_q1_delta < precision
      && iter_mode_delta < precision
      && iter_q2_delta < precision
    )
    {
      # message("Mission accomplished!")
      return()
    }
  }
  # At least, get the mode right:
  self$fit_dist_location(verbosity = verbosity, ...)
  warning("Couldn't make it within desired precision, sorry!")
}

fit_dist_location = function(verbosity = NULL, ...) {
  # move the fitted distribution to the
  # position where its mode (peak) coincidate
  # with the expert estimated mode.

  if (is.null(verbosity)) { verbosity <- 0 }

  # first, we calculate lambda1 (PDF shape location).
  # self$lambda1 <- self$estimated_mode_value

  # Find the difference between the mode (peak)
  # of the currently fitted distribution with
  # the desired mode coming from the expert
  # estimate
  delta <- self$estimated_mode_value - self$dist_mode
  new_lambda1 <- self$lambda1 + delta

  if (verbosity > 0) {
    message(paste0("lambda1: ",self$lambda1, " --> ", new_lambda1,
                   " estimated_mode_value: ",self$estimated_mode_value,
                   " dist_mode: ", self$dist_mode))
  }

  # Move the fitted distribution to compensate
  # for this difference
  self$lambda1 <- new_lambda1

}

fit_dist_scale = function(scaling_side = NULL, verbosity = NULL, ...) {
  # after lambda1 (location), we calculate lambda2 (PDF shape size or scale).
  # at this point, we don't consider skewness and assume shape symmetry.
  # lambda2 is like a "zoom" for the PDF,
  # but this parameter's value is inversely proportional to the shape size or scale.
  # to find an initial best match, we follow this procedure:
  # 0). "Neutralize" skewness parameters lambda3 and lambda4 setting them to -1
  # 1). Take an arbitrary PDF centered around 0 with lambda2 = e (and lambda3 & 4 set at - 1),
  # 2). Find the ratio between lambda2 and the desired quantile.
  # 3). Apply this ratio to the size / scale of the target distribution (provided in the estimation parameters).

  # scaling_side: default: "small". other possible value: "big". this parameter
  # tells us if we initialize lambda2 from the "small" side of a skewed distribution
  # or the "big" side. if the distribution is symetric, this parameter will have
  # no meaningful influence.
  if (is.null(scaling_side)) { scaling_side <- "small" }
  # OBSERVATION: when fitting the distribution from the small size, I get
  # excellent and consistent results. when we approach fitting from the big side,
  # we tend to fail to fit the distribution properly if the skewness is too large.
  # it would be great to analyse the causes of this difference in greater details.
  # it may be caused by number precision issues whereby too steep a curve would
  # require hyper precise number optimization.

  # because the 3 points estimates may be skewed,
  # we choose one of the two sides.
  # here, we decide to work with the larger side,
  # which means we will need to skew the other side
  # afterward.

  if (is.null(verbosity)) { verbosity <- 0 }

  left_value_range <- self$estimated_mode_value - self$estimated_range_min_value
  #left_value_range
  right_value_range <- self$estimated_range_max_value - self$estimated_mode_value
  #right_value_range

  # which side should we use?
  side <- NULL
  target_proba <- NULL
  if (left_value_range > right_value_range) {
    if (scaling_side == "small") {
      side <- "right"
      target_proba <- 1 - self$estimated_range_max_proba
    }
    else
    {
      side <- "left"
      target_proba <- self$estimated_range_min_proba
    }
  } else {
    if (scaling_side == "small") {
      side <- "left"
      target_proba <- self$estimated_range_min_proba
    }
    else
    {
      side <- "right"
      target_proba <- 1 - self$estimated_range_max_proba
    }
  }

  # if distribution was zero-centered
  # and we wanted its size to fit,
  # what would be the quantile value for that probability ?
  target_value <- NULL
  if (side == "left")
  {
    target_value <- - left_value_range
  } else {
    target_value <- - right_value_range
  }

  # This is where the wizardry operates, abracadabra!!!
  magic_value <- qgl(p = target_proba, lambda1 = 0, lambda2 = exp(1), lambda3 = -1, lambda4 = -1)
  magic_ratio <- magic_value / target_value
  magic_lambda2 = abs(exp(1) * magic_ratio)

  # result <- qgl(p = target_proba, lambda1 = self$estimated_mode_value, lambda2 = magic_lambda2, lambda3 = -1, lambda4 = -1)
  # message(paste0("result: ",result))
  # TODO: Add some result quality check here.

  if (verbosity > 0) { message(paste0("lambda2: ",self$lambda2, " --> ", magic_lambda2)) }

  self$lambda2 <- magic_lambda2

}

fit_dist_left_skew = function(verbosity = NULL, ...) {

  if (is.null(verbosity)) { verbosity <- 0 }

  # pgl does not support vectors in the lambda3 parameter,
  # (which I must say is perfectly reasonable).
  # So I declare a flat scalar version and take this opportunity
  # to throw away positive values (nlm optimizer doesn't support bounds either).
  flat_function <- function(x){
    if (x >= 0)
    {
      if (verbosity > 0) { warning("x >= 0") }
      return(Inf)
    }
    return(pgl(
      q = self$estimated_range_min_value,
      lambda1 = self$lambda1,
      lambda2 = self$lambda2,
      lambda3 = x,
      lambda4 = self$lambda4))
  }

  # Then I declare a minimization function
  # that will vectorize the call to flat_function with vapply.
  minimization_function <- function(x, ...){
    return(
      abs(
        vapply(x, flat_function, 0)
        -
          self$estimated_range_min_proba
      )
      # nlm prefers to reduce high numbers
      # so I artificially increase the output
      # of my minimization function by a factor
      # that guarantees precise enough results.
      * 1000000
      # * 1000
    )
  }

  # Then we run the optimization.
  # TODO: Study nlm options in greater details.
  nlm_wrapper <- NULL
  if (verbosity == 0) {
    nlm_wrapper <- function(...) {
      suppressWarnings(nlm(...))
    }
  }
  else
  {
    nlm_wrapper <- nlm
  }
  optimization <- nlm_wrapper(
    minimization_function,
    -1,
    ndigit = 22,
    iterlim = 128,
    print.level = verbosity,
    verbosity = verbosity)

  # TODO: We should test the result against a tolerance threshold.
  #self$get_probability(self$estimated_range_max_value)
  #self$get_quantile(self$estimated_range_max_proba)

  if (!is.null(optimization$estimate))
  {
    new_lambda3 <- optimization$estimate
    if (verbosity > 0) { message(paste0("lambda3: ",self$lambda3, " --> ", new_lambda3)) }
    # And we retrieve its output.
    self$lambda3 <- new_lambda3
  }
  else
  {
    if (verbosity > 0)
    {
      warning("nlm returns NULL")
      self
    }
  }
}

fit_dist_right_skew = function(verbosity = NULL, ...) {

  if (is.null(verbosity)) { verbosity <- 0 }

  # pgl does not support vectors in the lambda4 parameter,
  # (which I must say is perfectly reasonable).
  # So I declare a flat scalar version and take this opportunity
  # to throw away positive values (nlm optimizer doesn't support bounds either).
  flat_function <- function(x, ...){
    if (x >= 0) {
      if (verbosity > 0) { warning("x >= 0") }
      return(Inf)
    }
    return(pgl(
      q = self$estimated_range_max_value,
      lambda1 = self$lambda1,
      lambda2 = self$lambda2,
      lambda3 = self$lambda3,
      lambda4 = x))
  }

  # Then I declare a minimization function
  # that will vectorize the call to flat_function with vapply.
  minimization_function <- function(x, verbosity = NULL, ...){
    return(
      abs(
        vapply(x, flat_function, 0)
        -
          self$estimated_range_max_proba
      )
      # nlm prefers to reduce high numbers
      # so I artificially increase the output
      # of my minimization function by a factor
      # that guarantees precise enough results.
      * 1000000
      # * 1000
    )
  }

  # Then we run the optimization.
  # TODO: Study nlm options in greater details.
  nlm_wrapper <- NULL
  if (verbosity == 0) {
    nlm_wrapper <- function(...) {
      suppressWarnings(nlm(...))
    }
  }
  else
  {
    nlm_wrapper <- nlm
  }
  optimization <- nlm_wrapper(
    minimization_function,
    -1,
    ndigit = 22,
    iterlim = 128,
    print.level = verbosity,
    verbosity = verbosity)

  # TODO: We should test the result against a tolerance threshold.
  #self$get_probability(self$estimated_range_max_value)
  #self$get_quantile(self$estimated_range_max_proba)

  if (!is.null(optimization$estimate))
  {
    new_lambda4 <- optimization$estimate
    if (verbosity > 0) { message(paste0("lambda4: ",self$lambda4, " --> ", new_lambda4)) }
    # And we retrieve its output.
    self$lambda4 <- new_lambda4
  }
  else
  {
    if (verbosity > 0) { warning("nlm returns NULL") }
  }
}
