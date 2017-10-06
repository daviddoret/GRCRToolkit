if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,ggplot2)

#' The factor_estimate class provides an inheritance model
#' for effective factor estimation approaches.
#' This class should be inherited from but not instanciated from (ie it is abstract).
#'
#' TODO:
#' - Glue code with model_factor.
#' - Implement SAMPLE var, sd, median, etc.
#'   Re-implement mode from the SAMPLE this this in summary statistics.
#'
#' @export
factor_estimate <- R6Class(
  "factor_estimate",
  public = list(
    initialize = function(
      estimation_method_name = NULL,
      distribution_name = NULL,
      limit_min_value = NA,
      limit_max_value = NA,
      ...) {
      if(is.null(limit_min_value)) { limit_min_value <- NA }
      if(is.null(limit_max_value)) { limit_max_value <- NA }
      self$estimation_method_name <- estimation_method_name
      self$distribution_name <- distribution_name
      self$limit_min_value <- limit_min_value
      self$limit_max_value <- limit_max_value
      },
    check_state_consistency = function(output_format = NULL,...) {
      # Informs us if the object state is consistent / logical.
      if(is.null(output_format)) { output_format = "boolean" }
      consistency_error_count <- 0
      consistency_report <- NULL

      # Check if all mandatory parameters have been defined.
      # N/A

      # And eventually output the conclusion in the desired format.
      # And eventually output the conclusion in the desired format.
      if(output_format == "boolean")
      {
        return(consistency_error_count == 0)
      }
      else if(output_format == "int")
      {
        return(consistency_error_count)
      }
      else if(output_format == "report")
      {
        return(consistency_report)
      }
      else
      {
        stop("Sorry, this output format is not supported.")
      }
    },
    get_print_lines = function(...) {
      return
        c(
          paste0("Estimation method: ", self$estimation_method_name),
          paste0("Fitted distribution: ", self$distribution_name),
          paste0(
            " mode = ", fn(self$dist_mode,2),
            " ,γ1 = ", fn(self$dist_skewness,4),
            " ,κ = ", fn(self$dist_kurtosis,4)),
          "Simulation sample:",
          paste0(
            " n = ", fn(self$simulation_sample_size,0),
            " ,min = ", fn(self$simulation_sample_min,2),
            " ,max = ", fn(self$simulation_sample_max,2)),
          paste0(
            " μ = ", fn(self$simulation_sample_mean,2),
            " ,sd = ", fn(self$simulation_sample_sd,2),
            " ,var = ", fn(self$simulation_sample_variance,0)))
    },
    print = function(...) {
      cat(paste0(self$get_print_lines(), collapse = "\n"))
      invisible(self)
    },
    get_density = function(x, ...) { return(self$density_function(x, ...)) },
    get_probability = function(q, ...) { return(self$probability_function(q, ...)) },
    get_quantile = function(p, ...) { return(self$quantile_function(p, ...)) },
    get_random = function(n, ...)
      {
        random_sample <- self$random_function(n, ...)
        if(!is.na(self$limit_min_value)){
          random_sample <- pmax(random_sample, rep(self$limit_min_value, times = n))
        }
        if(!is.na(self$limit_max_value)){
          random_sample <- pmin(random_sample, rep(self$limit_max_value, times = n))
        }
      return(random_sample)
      },
    get_simulation_sample_head = function(n, ...) {
      extract <- head(self$simulation_sample[order(self$simulation_sample$factor_value), ], n = n)
      #rownames(extract) <- 1:n
      return(extract)
    },
    get_simulation_sample_tail = function(n, ...) {
      extract <- tail(self$simulation_sample[order(self$simulation_sample$factor_value), ], n = n)
      #rownames(extract) <- 1:n
      return(extract)
    },
    get_simulation_sample_random = function(n, ...) {
      # IDEA: First, sort the full simulation, store their relative positions
      #       and return the item respective positions.
      extract <- self$simulation_sample[sample(nrow(self$simulation_sample), n), ]
      #rownames(extract) <- 1:n
      return(extract)
    },
    plot_density = function(x_start = NULL, x_end = NULL)
      {
      if(self$check_state_consistency())
      {
        if(is.null(x_start)) { x_start <- self$plot_value_start }
        if(is.null(x_end)) { x_end <- self$plot_value_end }
        return(
          plot_probability_density_function(
            fun = self$density_function,
            x_start = x_start,
            x_end = x_end))
      }
      else
      {
        return(plot_vignette(title="Invalid parameters",text=self$check_state_consistency(output_format = "report")))
      }
      },
    plot_mass = function(x_start = NULL, x_end = NULL)
    {
      if(self$check_state_consistency())
      {
        if(is.null(x_start)) { x_start <- self$plot_value_start }
        if(is.null(x_end)) { x_end <- self$plot_value_end }
        return(
          plot_probability_mass_function(
            fun = self$density_function,
            x_start = x_start,
            x_end = x_end))
      }
      else
      {
        return(plot_vignette(title="Invalid parameters",text=self$check_state_consistency(output_format = "report")))
      }
    },
    plot_probability = function(x_start = NULL, x_end = NULL)
      {
      if(self$check_state_consistency())
      {

      if(is.null(x_start)) { x_start <- self$plot_value_start }
      if(is.null(x_end)) { x_end <- self$plot_value_end }
      return(
        plot_cumulative_distribution_function(
          fun = self$probability_function,
          x_start = x_start,
          x_end = x_end))
      }
      else
      {
        return(plot_vignette(title="Invalid parameters",text=self$check_state_consistency(output_format = "report")))
      }
      },
    plot_quantile = function(x_start = NULL, x_end = NULL)
      {
      if(self$check_state_consistency())
      {
      if(is.null(x_start)) { x_start <- 0 }
      if(is.null(x_end)) { x_end <- 1 }
      return(
        plot_quantile_function(
          fun = self$quantile_function,
          x_start = x_start,
          x_end = x_end))
      }
      else
      {
        return(plot_vignette(title="Invalid parameters",text=self$check_state_consistency(output_format = "report")))
      }
    },
    plot_simulation_sample = function(title = NULL)
    {
      if(self$check_state_consistency())
      {

      sample <- self$get_random(1000)
      return(
        plot_sample(
          sample = sample,
          title = "Sample histogram with outliers"))       }
      else
      {
        return(plot_vignette(title="Invalid parameters",text=self$check_state_consistency(output_format = "report")))
      }
    },
    plot_vignette = function(...) {
      # Plots a textual summary description of this factor.
      return(plot_vignette(title="Summary", text=self$get_print_lines()))
    },
    plot_all = function(x_start = NULL, x_end = NULL) {
      if(is.null(x_start)) { x_start <- self$plot_value_start }
      if(is.null(x_end)) { x_end <- self$plot_value_end }

      return(multiplot(
        self$plot_vignette(),
        self$plot_density(x_start = x_start, x_end = x_end),
        self$plot_probability(x_start = x_start, x_end = x_end),
        self$plot_quantile(),
        self$plot_simulation_sample(),
        #self$plot_sample_without_outliers(),
        layout = matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE)))
    },
    simulate = function(n = NULL) {
      # The simulate method may be overridden by a subclass.
      # This may be required to populate richer data frames
      # with complementary columns. I was thinking of this
      # approach to implement the frequency x impact factor
      # where the frequency factor generates a vector of
      # frequencies and where the impact factor will need to
      # call (frequecy number) times the random function
      # and sum the result. In this situation it is desirable
      # to keep the individual impacts in an "individual impacts"
      # column in the data frame and use the standard factor_value column
      # for the final factor results.
      if(is.null(n)) { n = 10000 }
      if(n <= 0) {
        stop("n <= 0")
      }
      factor_value <- self$get_random(n = n)
      private$private_simulation_sample <- data.frame(factor_value = factor_value)
    }
  ),
  active = list(
    estimation_method_name = function(value,...) {
      if(missing(value)) { return(private$private_estimation_method_name) }
      else { private$private_estimation_method_name <- value }},
    distribution_name = function(value,...) {
      if(missing(value)) { return(private$private_distribution_name) }
      else { private$private_distribution_name <- value }},
    density_function = function(value,...) {
      if(missing(value)) { return(private$private_density_function) }
      else { private$private_density_function <- value }},
    limit_min_value = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_limit_min_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_limit_min_value <- NA }
        return(private$private_limit_min_value) }
      else {
        if(is.null(value)) { value <- NA }
        if(
          ( is.na(value) & !is.na(self$limit_min_value) ) |
          ( !is.na(value) & is.na(self$limit_min_value) ) |
          ( !is.na(value) & !is.na(self$limit_min_value) & value != self$limit_min_value ) )
        {
          private$private_limit_min_value <- value
          # No need to re-fit the distribution.
          # TODO: Re-populate the simulation sample.
        }}},
    limit_max_value = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_limit_max_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_limit_max_value <- NA }
        return(private$private_limit_max_value) }
      else {
        if(is.null(value)) { value <- NA }
        if(
          ( is.na(value) & !is.na(self$limit_max_value) ) |
          ( !is.na(value) & is.na(self$limit_max_value) ) |
          ( !is.na(value) & !is.na(self$limit_max_value) & value != self$limit_max_value ) )
        {
          private$private_limit_max_value <- value
          # No need to re-fit the distribution.
          # TODO: Re-populate the simulation sample.
        }}},
    probability_function = function(value,...) {
      if(missing(value)) { return(private$private_probability_function) }
      else { private$private_probability_function <- value }},
    quantile_function = function(value,...) {
      if(missing(value)) { return(private$private_quantile_function) }
      else { private$private_quantile_function <- value }},
    random_function = function(value,...) {
      if(missing(value)) { return(private$private_random_function) }
      else { private$private_random_function <- value }},
    # Beautiful graph preferences
    plot_value_start = function(value,...) {
      if(missing(value)) { return(private$private_plot_value_start) }
      else { private$private_plot_value_start <- value }},
    plot_value_end = function(value,...) {
      if(missing(value)) { return(private$private_plot_value_end) }
      else { private$private_plot_value_end <- value }},
    plot_probability_start = function(value,...) {
      if(missing(value)) { return(private$private_plot_probability_start) }
      else { private$private_plot_probability_start <- value }},
    plot_probability_end = function(value,...) {
      if(missing(value)) { return(private$private_plot_probability_end) }
      else { private$private_plot_probability_end <- value }},
    # Standard moments of the fitted distribution
    # These are conditionnaly implemented by the subclasses
    # if analytical solutions are available.
    # At this level, we may only rely on optimization to
    # estimate solutions.
    dist_mode = function(value,...) {
      if(missing(value))
      {
        warning("Should be implemented by the subclass")
        return(NA)
        # The new approach relying on optimize require a range
        # to be searched to find the maxima in the PDF.
        # We may implement here a best effort but then
        # optimize() may or may not be the right solution.
        # return(get_dist_mode_from_pdf(pdf = self$get_density))
        }
      else { stop("This is a read-only attribute") }},
    simulation_sample_mean = function(value,...) {
      if(missing(value)) {
        return(mean(self$simulation_sample$factor_value))
      }
      else { stop("This is a read-only attribute") }},
    simulation_sample_sd = function(value,...) {
      if(missing(value)) {
        return(sd(self$simulation_sample$factor_value))
      }
      else { stop("This is a read-only attribute") }},
    simulation_sample_variance = function(value, ...) {
      if(missing(value)) {
        return(var(self$simulation_sample$factor_value))
      }
      else { stop("This is a read-only attribute") }},
    simulation_sample_size = function(value,...) {
      if(missing(value)) {
        return(length(self$simulation_sample$factor_value))
      }
      else { stop("This is a read-only attribute") }},
    simulation_sample_min = function(value,...) {
      if(missing(value)) {
        return(min(self$simulation_sample$factor_value))
      }
      else { stop("This is a read-only attribute") }},
    simulation_sample_max = function(value,...) {
      if(missing(value)) {
        return(max(self$simulation_sample$factor_value))
      }
      else { stop("This is a read-only attribute") }},
    dist_skewness = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    dist_kurtosis = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    simulation_sample = function(value,...) {
      # Returns a data frame with the simulation sample data.
      # The data frame mandatorily contains a column "factor_value" with
      # the resulting factor values.
      # The data frame may contain other columns with complementary information.
      if(missing(value)) { return(private$private_simulation_sample) }
      else { private$private_simulation_sample <- value }}
  ),
  private = list(
    private_estimation_method_name = NA,
    private_distribution_name = NA,
    private_density_function = NA,
    private_probability_function = NA,
    private_quantile_function = NA,
    private_random_function = NA,
    # Limits for good-looking graph rendering.
    # Sub-classes implementing estimation methods
    # have the responsibility to set their values.
    private_plot_value_start = NA,
    private_plot_value_end = NA,
    private_plot_probability_start = NA,
    private_plot_probability_end = NA,
    private_simulation_sample = NA,
    private_limit_min_value = NA,
    private_limit_max_value = NA
  )
)
