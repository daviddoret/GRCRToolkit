if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

options(digits=22)

#' GLD 3 points estimate
#'
#' TODO:
#' - Make lambda parameters read-only for class users
#'   in such a way as to guarantee consistency between
#'   the fitted distribution and the estimation parameters.
#' - Store the simulation results in a proper local simulation
#'   variable + implement a simulation sample size parameter.
#' - Implement a cool summary vignette.
#' @export
factor_estimate_gld_3points <- R6Class(
  "factor_estimate_gld_3points",
  inherit = factor_estimate_gld,
  public = list(
    initialize = function(...) {
      super$initialize(
        estimation_method_name = "PERT-like 3 points estimate", ...)

      # Initialize lambda parameters
      # to avoid the presence of NULLs.
      self$lambda1 <- 0
      self$lambda2 <- 1
      self$lambda3 <- -1
      self$lambda4 <- -1
      },
    fit_dist = function(max_iteration = NULL, precision = NULL, ...) {

      if(is.null(max_iteration)) { max_iteration <- 256 }
      if(is.null(precision)) { precision <- 1 } # Expressed in quantile value.

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

      # First, we restart from a clean page:
      self$lambda1 <- 0
      self$lambda2 <- 1
      self$lambda3 <- -1
      self$lambda4 <- -1

      # And then we apply our round trip approach
      for(iter in c(1 : max_iteration))
      {
        self$fit_dist_location(...)
        self$fit_dist_scale(...)
        self$fit_dist_left_skew(...)
        self$fit_dist_right_skew(...)

        iter_q1 <- self$get_quantile(self$min_proba)
        iter_q2 <- self$get_quantile(self$max_proba)
        iter_mode <- self$dist_mode

        iter_q1_delta <- abs(iter_q1 - self$min_value)
        iter_q2_delta <- abs(iter_q2 - self$max_value)
        iter_mode_delta <- abs(self$dist_mode - self$mode_value)

        # message(paste0("Iteration: ", iter))
        # message(paste0("Q1: target: ", self$min_value, ", attained: ", iter_q1, ", diff: ", iter_q1_delta))
        # message(paste0("Mode: target: ", self$mode_value, ", attained: ", iter_mode, ", diff: ", iter_mode_delta))
        # message(paste0("Q2: target: ", self$max_value, ", attained: ", iter_q2, ", diff: ", iter_q2_delta))

        if(
          iter_q1_delta < precision
          && iter_mode_delta < precision
          && iter_q2_delta < precision
        )
        {
          # message("Mission accomplished!")
          return()
        }
      }
    },
    fit_dist_location = function(...) {
      # move the fitted distribution to the
      # position where its mode (peak) coincidate
      # with the expert estimated mode.

      # first, we calculate lambda1 (PDF shape location).
      # self$lambda1 <- self$mode_value

      # Find the difference between the mode (peak)
      # of the currently fitted distribution with
      # the desired mode coming from the expert
      # estimate
      delta <- self$mode_value - self$dist_mode

      # Move the fitted distribution to compensate
      # for this difference
      self$lambda1 <- self$lambda1 + delta
    },
    fit_dist_scale = function() {
      # after lambda1 (location), we calculate lambda2 (PDF shape size or scale).
      # at this point, we don't consider skewness and assume shape symmetry.
      # lambda2 is like a "zoom" for the PDF,
      # but this parameter's value is inversely proportional to the shape size or scale.
      # to find an initial best match, we follow this procedure:
      # 0). "Neutralize" skewness parameters lambda3 and lambda4 setting them to -1
      # 1). Take an arbitrary PDF centered around 0 with lambda2 = e (and lambda3 & 4 set at - 1),
      # 2). Find the ratio between lambda2 and the desired quantile.
      # 3). Apply this ratio to the size / scale of the target distribution (provided in the estimation parameters).

      # neutralize skewness
      # self$lambda3 <- -1
      # self$lambda4 <- -1

      # because the 3 points estimates may be skewed,
      # we choose one of the two sides.
      # here, we decide to work with the larger side,
      # which means we will need to skew the other side
      # afterward.
      left_value_range <- self$mode_value - self$min_value
      left_value_range
      right_value_range <- self$max_value - self$mode_value
      right_value_range

      # which side should we use?
      side <- NULL
      target_proba <- NULL
      if(left_value_range > right_value_range) {
        side <- "left"
        target_proba <- self$min_proba
      } else {
        side <- "right"
        target_proba <- 1 - self$max_proba
      }

      # if distribution was zero-centered
      # and we wanted its size to fit,
      # what would be the quantile value for that probability ?
      target_value <- NULL
      if(side == "left")
      {
        target_value <- - left_value_range
      } else {
        target_value <- - right_value_range
      }

      # This is where the wizardry operates, abracadabra!!!
      magic_value <- qgl(p = target_proba, lambda1 = 0, lambda2 = exp(1), lambda3 = -1, lambda4 = -1)
      magic_ratio <- magic_value / target_value
      magic_lambda2 = abs(exp(1) * magic_ratio)

      print(magic_lambda2)

      result <- qgl(p = target_proba, lambda1 = self$mode_value, lambda2 = magic_lambda2, lambda3 = -1, lambda4 = -1)
      message(paste0("result: ",result))

      self$lambda2 <- magic_lambda2

      },
    fit_dist_left_skew = function() {

      # pgl does not support vectors in the lambda3 parameter,
      # (which I must say is perfectly reasonable).
      # So I declare a flat scalar version and take this opportunity
      # to throw away positive values (nlm optimizer doesn't support bounds either).
      flat_function <- function(x){
        if(x >= 0) { return(Inf)  }
        return(pgl(
          q = self$min_value,
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
              fg3$min_proba
          )
          # nlm prefers to reduce high numbers
          # so I artificially increase the output
          # of my minimization function by a factor
          # that guarantees precise enough results.
          * 1000000
        )
      }

      # Then we run the optimization.
      # TODO: Study nlm options in greater details.
      optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128)

      # TODO: We should test the result against a tolerance threshold.
      #fg3$get_probability(fg3$max_value)
      #fg3$get_quantile(fg3$max_proba)

      # And we retrieve its output.
      fg3$lambda3 <- optimization$estimate

    },
    fit_dist_right_skew = function() {

      # pgl does not support vectors in the lambda4 parameter,
      # (which I must say is perfectly reasonable).
      # So I declare a flat scalar version and take this opportunity
      # to throw away positive values (nlm optimizer doesn't support bounds either).
      flat_function <- function(x){
        if(x >= 0) { return(Inf)  }
        return(pgl(
          q = self$max_value,
          lambda1 = self$lambda1,
          lambda2 = self$lambda2,
          lambda3 = self$lambda3,
          lambda4 = x))
      }

      # Then I declare a minimization function
      # that will vectorize the call to flat_function with vapply.
      minimization_function <- function(x, ...){
        return(
          abs(
            vapply(x, flat_function, 0)
            -
              fg3$max_proba
          )
          # nlm prefers to reduce high numbers
          # so I artificially increase the output
          # of my minimization function by a factor
          # that guarantees precise enough results.
          * 1000000
        )
      }

      # Then we run the optimization.
      # TODO: Study nlm options in greater details.
      optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128)

      # TODO: We should test the result against a tolerance threshold.
      #fg3$get_probability(fg3$max_value)
      #fg3$get_quantile(fg3$max_proba)

      # And we retrieve its output.
      fg3$lambda4 <- optimization$estimate

    },
    get_print_lines = function(...) {
      return(
        c(super$get_print_lines(),
               "Estimation parameters:",
               paste0(
                    " min = ", fn(self$min_value,2), " (", fn(self$min_proba,2), ")",
                    " ,mode = ", fn(self$mode_value,2),
                    " ,max = ", fn(self$max_value,2), " (", fn(self$max_proba,2), ")"),
               "Fitted quantiles:",
               paste0(
                    " min = ", fn(self$get_quantile(self$min_proba), 2), " (", fn(self$min_proba,2), ")",
                    " ,mode = ", fn(self$dist_mode, 2),
                    " ,max = ", fn(self$get_quantile(self$max_proba), 2), " (", fn(self$max_proba,2), ")"),
               "Fitted probabilities:",
               paste0(
                    " min = ", fn(self$min_value,2), " (", fn(self$get_probability(self$min_value), 2), ")",
                    " ,mode = ", fn(self$dist_mode, 2),
                    " ,max = ", fn(self$max_value,2), " (", fn(self$get_probability(self$max_value), 2), ")")
                    ))
    },
    reset_graph_limits = function() {
      # Set default scale margins containing all estimation parameters for pretty graph rendering.
      self$graph_value_start <- self$min_value #- (self$max_value - self$min_value) * .05
      self$graph_value_end <- self$max_value #+ (self$max_value - self$min_value) * .05
      self$graph_probability_start <- self$min_proba / 4
      self$graph_probability_end <- self$max_proba + (1 - self$max_proba) / 4
    },
    simulate = function(n = NULL) {
      if(is.null(n)) { n <- 3000 }
      factor_value <- self$get_random(n = n)
      self$simulation_data <- data.frame(factor_value)
    }
  ),
  active = list(
    min_value = function(value,...) {
      if(missing(value)) { return(private$private_min_value) }
      else {
        private$private_min_value <- value
        self$reset_graph_limits() }},
    mode_value = function(value,...) {
      if(missing(value)) { return(private$private_mode_value) }
      else {
        private$private_mode_value <- value
        self$reset_graph_limits() }},
    max_value = function(value,...) {
      if(missing(value)) { return(private$private_max_value) }
      else {
        private$private_max_value <- value
        self$reset_graph_limits() }},
    min_proba = function(value,...) {
      if(missing(value)) { return(private$private_min_proba) }
      else {
        private$private_min_proba <- value
        self$reset_graph_limits() }},
    #mode_proba = function(value,...) {
    #  if(missing(value)) { return(private$private_mode_proba) }
    #  else {
    #    private$private_mode_proba <- value
    #    self$reset_graph_limits() }},
    max_proba = function(value,...) {
      if(missing(value)) { return(private$private_max_proba) }
      else {
        private$private_max_proba <- value
        self$reset_graph_limits() }},
    range_size_proba = function(value,...) {
      if(missing(value)) { return(self$max_proba - self$min_proba) }
      else {
        self$min_proba <- (1 - value) / 2
        self$max_proba <- 1 - (1 - value) / 2
        self$reset_graph_limits() }}
  ),
  private = list(
    private_min_value = NULL,
    private_mode_value = NULL,
    private_max_value = NULL,
    private_min_proba = NULL,
    #private_mode_proba = NULL,
    private_max_proba = NULL
  )
)
