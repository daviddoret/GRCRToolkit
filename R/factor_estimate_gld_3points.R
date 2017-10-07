if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

options(digits = 22)

#' factor_estimate_gld_3points: GLD 3 points estimate
#'
#' For the time being, I only support GLD's FMKH parameterization.
#' If other parameterizations become necessary / interesting in the future,
#' these will require specifically dedicated R6 classes,
#' because the way we tweak lambda parameters here is strongly linked to FMKH logic.
#'
#' Future enhancements:
#' - Make lambda parameters read-only for class users
#'   in such a way as to guarantee consistency between
#'   the fitted distribution and the estimation parameters.
#' @export
factor_estimate_gld_3points <- R6Class(
  "factor_estimate_gld_3points",
  inherit = factor_estimate_gld,
  public = list(
    initialize = function(
      estimated_range_min_value = NULL,
      estimated_mode_value = NULL,
      estimated_range_max_value = NULL,
      estimated_range_size_proba = NULL,
      limit_min_value = NULL,
      limit_max_value = NULL,
      fit_dist = NULL, # Triggers distribution fitting immediately.
      simulate = NULL, # Triggers simulation immediately.
      ...) {
      super$initialize(
        estimation_method_name = "PERT-like 3 points estimate",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        ...)

      # Default values
      if (is.null(estimated_range_size_proba)) {
        estimated_range_size_proba <- .9 # TODO: replace with a default configuration setting
      }
      if (is.null(fit_dist)) { fit_dist <- TRUE }
      if (is.null(simulate)) { simulate <- TRUE }

      # Initialize lambda parameters
      # to avoid the presence of NULLs.
      self$lambda1 <- 0
      self$lambda2 <- 1
      self$lambda3 <- -1
      self$lambda4 <- -1

      self$estimated_range_min_value <- estimated_range_min_value
      self$estimated_mode_value <- estimated_mode_value
      self$estimated_range_max_value <- estimated_range_max_value

      self$estimated_range_size_proba <- estimated_range_size_proba

      if (fit_dist) { self$fit_dist(...) }
      if (simulate) { self$simulate(...) }
      },
    fit_dist = function(max_iteration = NULL, precision = NULL, verbosity = NULL, ...) {

      if (is.null(verbosity)) { verbosity <- 0 }
      if (is.null(max_iteration)) { max_iteration <- 256 }
      if (is.null(precision)) { precision <- 1 } # Expressed in quantile value.

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
      self$lambda1 <- self$estimated_mode_value
      self$lambda2 <- 1
      self$lambda3 <- -1
      self$lambda4 <- -1

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
    },
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

    },
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

      },
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
    },
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
    },
    get_print_lines = function(...) {
      return(
        c(super$get_print_lines(),
               "Estimation parameters:",
               paste0(
                    " min = ", fn(self$estimated_range_min_value,2), " (", fn(self$estimated_range_min_proba,2), ")",
                    " ,mode = ", fn(self$estimated_mode_value,2),
                    " ,max = ", fn(self$estimated_range_max_value,2), " (", fn(self$estimated_range_max_proba,2), ")"),
               "Fitted quantiles:",
               paste0(
                    " min = ", fn(self$get_quantile(self$estimated_range_min_proba), 2), " (", fn(self$estimated_range_min_proba,2), ")",
                    " ,mode = ", fn(self$dist_mode, 2),
                    " ,max = ", fn(self$get_quantile(self$estimated_range_max_proba), 2), " (", fn(self$estimated_range_max_proba,2), ")"),
               "Fitted probabilities:",
               paste0(
                    " min = ", fn(self$estimated_range_min_value,2), " (", fn(self$get_probability(self$estimated_range_min_value), 2), ")",
                    " ,mode = ", fn(self$dist_mode, 2),
                    " ,max = ", fn(self$estimated_range_max_value,2), " (", fn(self$get_probability(self$estimated_range_max_value), 2), ")")
                    ))
    },
    check_state_consistency = function(output_format = NULL,...) {
      # Informs us if the current parameters are consistent / logical.
      # This makes it possible to prevent useless calls to expensive functions
      # that may output multitude of warnings and errors when we know
      # from the beginning that this parameterization is doomed to failure.
      # Returns TRUE if parameters are consistent.
      # Returns a descriptive
      if (is.null(output_format)) { output_format = "boolean" }
      consistency_error_count <- super$check_state_consistency(output_format = "int")
      consistency_report <- super$check_state_consistency(output_format = "report")

      # Check if all mandatory parameters have been defined.
      if (is.na(self$estimated_range_min_value)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. range min value is missing."), sep = "\n")
      }
      if (is.na(self$estimated_mode_value)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. mode value is missing"), sep = "\n")
      }
      if (is.na(self$estimated_range_max_value)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. range max value is missing"), sep = "\n")
      }
      if (is.na(self$estimated_range_min_proba)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. range min proba. is missing"), sep = "\n")
      }
      if (is.na(self$estimated_range_max_proba)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. range max proba. is missing"), sep = "\n")
      }

      if (consistency_error_count == 0)
      {
        # If all parameters are present,
        # we can check consistency between parameters.
        if (self$estimated_range_min_value > self$estimated_mode_value) {
          consistency_error_count <- consistency_error_count + 1
          consistency_report <- paste0(c(consistency_report, "est. range min value > est. mode value"), sep = "\n")
        }
        if (self$estimated_mode_value > self$estimated_range_max_value) {
          consistency_error_count <- consistency_error_count + 1
          consistency_report <- paste0(c("est. mode value > est. range max value"), sep = "\n")
        }
        if (self$estimated_range_min_proba >= self$estimated_range_max_proba) {
          consistency_error_count <- consistency_error_count + 1
          consistency_report <- paste0(c("est. range min proba. >= est. range max proba."), sep = "\n")
        }
      }

      # And eventually output the conclusion in the desired format.
      if (output_format == "boolean")
      {
        return(consistency_error_count == 0)
      }
      else if (output_format == "int")
      {
        return(consistency_error_count)
      }
      else if (output_format == "report")
      {
        return(consistency_report)
      }
      else
      {
        stop("Sorry, this output format is not supported.")
      }
    },
    plot_density = function(x_start = NULL, x_end = NULL, ...) {
      # Override the super-class method to append the limits and point estimates.

      if (is.null(x_start) | is.null(x_end))
      {
        x_start <- min(self$estimated_range_min_value, self$limit_min_value)
        x_end <- max(self$estimated_range_max_value, self$limit_max_value)
        margin <- (x_end - x_start) * .1 # Add a visual 10% margin
        x_start <- x_start - margin
        x_end <- x_end + margin
      }

      # Get the original PDF plot
      plot_01 <- super$plot_density(x_start = x_start, x_end = x_end)

      # Prepare a vector with the 3 points estimates
      estimates <- c(self$estimated_range_min_value,
                           self$estimated_mode_value,
                           self$estimated_range_max_value)

      # Enrich the graph with the estimates represented as vertical lines
      plot_01 <- overplot_vertical_lines(plot_01, x_values = estimates, color = "blue", alpha = .2, ...)

      limits_values <- c()
      if (!is.na(self$limit_min_value))
      {
        limits_values <- c(limits_values, self$limit_min_value)
      }
      if (!is.na(self$limit_max_value))
      {
        limits_values <- c(limits_values, self$limit_max_value)
      }

      # Enrich the graph with the estimates represented as vertical lines
      if (!is.na(self$limit_min_value) | !is.na(self$limit_max_value))
      {
        plot_01 <- overplot_vertical_lines(plot_01, x_values = limits_values, x_labels = NULL, color = "red", alpha = .2, ...)
      }

      return(plot_01)
    },
    plot_probability = function(x_start = NULL, x_end = NULL, ...) {
      # Override the super-class method to append the limits and point estimates.

      if (is.null(x_start) | is.null(x_end))
      {
        x_start <- min(self$estimated_range_min_value, self$limit_min_value)
        x_end <- max(self$estimated_range_max_value, self$limit_max_value)
        margin <- (x_end - x_start) * .1 # Add a visual 10% margin
        x_start <- x_start - margin
        x_end <- x_end + margin
      }

      # Get the original CPF plot
      plot_01 <- super$plot_probability(x_start = x_start, x_end = x_end)

      # Prepare a vector with the 3 points estimates
      x_estimates <- c(self$estimated_range_min_value,
                     self$estimated_mode_value,
                     self$estimated_range_max_value)

      # Enrich the graph with the estimates represented as vertical lines
      plot_01 <- overplot_vertical_lines(plot_01, x_values = x_estimates, color = "blue", alpha = .2, ...)

      x_limits_values <- c()
      if (!is.na(self$limit_min_value))
      {
        x_limits_values <- c(x_limits_values, self$limit_min_value)
      }
      if (!is.na(self$limit_max_value))
      {
        x_limits_values <- c(x_limits_values, self$limit_max_value)
      }

      # Enrich the graph with the estimates represented as vertical lines
      if (!is.na(self$limit_min_value) | !is.na(self$limit_max_value))
      {
        plot_01 <- overplot_vertical_lines(plot_01, x_values = x_limits_values, x_labels = NULL, color = "red", alpha = .2, ...)
      }

      # Prepare a vector with the 3 points estimates
      y_estimates <- c(self$estimated_range_min_proba,
                       self$estimated_range_max_proba)

      # Enrich the graph with the estimates represented as vertical lines
      plot_01 <- overplot_horizontal_lines(plot_01, y_values = y_estimates, color = "blue", alpha = .2, ...)

      return(plot_01)
    },
    reset_plot_limits = function() {
      # Set default scale margins containing all estimation parameters for pretty graph rendering.
      self$plot_value_start <- self$estimated_range_min_value
      self$plot_value_end <- self$estimated_range_max_value
      self$plot_probability_start <- self$estimated_range_min_proba / 4
      self$plot_probability_end <- self$estimated_range_max_proba + (1 - self$estimated_range_max_proba) / 4
    }
  ),
  active = list(
    dist_mode = function(value,...) {
      if (missing(value))
      {
        return(get_dist_mode_from_pdf(
          pdf = self$get_density,
          search_range_start = self$estimated_range_min_value,
          search_range_end = self$estimated_range_max_value))
      }
      else { stop("This is a read-only attribute") }},
    estimated_range_min_value = function(value,...) {
      if (missing(value)) {
        if (is.null(private$private_estimated_range_min_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_min_value <- NA }
        return(private$private_estimated_range_min_value)
        }
      else {
        # We only do something if something changes... This is important for Shiny apps, etc. to avoid recomputing everything when recomputing is not required
        if (is.na(self$estimated_range_min_value) | value != self$estimated_range_min_value)
        {
        private$private_estimated_range_min_value <- value
        if (self$check_state_consistency()) { self$fit_dist() }
        self$reset_plot_limits() }}},
    estimated_mode_value = function(value,...) {
      if (missing(value)) {
        if (is.null(private$private_estimated_mode_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_mode_value <- NA }
        return(private$private_estimated_mode_value)
        }
      else {
        # We only do something if something changes... This is important for Shiny apps, etc. to avoid recomputing everything when recomputing is not required
        if (is.na(self$estimated_mode_value) | value != self$estimated_mode_value)
          {
          private$private_estimated_mode_value <- value
          if (self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits()
          }
        }
      },
    estimated_range_max_value = function(value,...) {
      if (missing(value)) {
        if (is.null(private$private_estimated_range_max_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_max_value <- NA }
        return(private$private_estimated_range_max_value) }
      else {
        # We only do something if something changes... This is important for Shiny apps, etc. to avoid recomputing everything when recomputing is not required
        if (is.na(self$estimated_range_max_value) | value != self$estimated_range_max_value)
        {
          private$private_estimated_range_max_value <- value
          if (self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits() }}},
    estimated_range_min_proba = function(value,...) {
      if (missing(value)) {
        if (is.null(private$private_estimated_range_min_proba)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_min_proba <- NA }
        return(private$private_estimated_range_min_proba) }
      else {
        if (is.na(self$estimated_range_min_proba) | value != self$estimated_range_min_proba)
        {
          private$private_estimated_range_min_proba <- value
          if (self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits() }}},
    estimated_range_max_proba = function(value,...) {
      if (missing(value)) {
        if (is.null(private$private_estimated_range_max_proba)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_max_proba <- NA }
          return(private$private_estimated_range_max_proba) }
      else {
        if (is.na(self$estimated_range_max_proba) | value != self$estimated_range_max_proba)
        {
          private$private_estimated_range_max_proba <- value
          if (self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits() }}},
    estimated_range_size_proba = function(value,...) {
      # This is a shortcut parameter to estimated range min / max.
      # It computes a centered estimated range.
      if (missing(value)) { return(self$estimated_range_max_proba - self$estimated_range_min_proba) }
      else {
        if (value <= 0){
          stop("estimated_range_size_proba <= 0")
        }
        self$estimated_range_min_proba <- (1 - value) / 2
        self$estimated_range_max_proba <- 1 - (1 - value) / 2
        self$reset_plot_limits() }}
  ),
  private = list(
    private_estimated_range_min_value = NA,
    private_estimated_mode_value = NA,
    private_estimated_range_max_value = NA,
    private_estimated_range_min_proba = NA,
    private_estimated_range_max_proba = NA
  )
)
