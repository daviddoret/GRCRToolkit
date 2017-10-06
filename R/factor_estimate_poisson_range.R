if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6)

options(digits=22)

#' Poisson Range (2 points) estimate
#'
#' @export
factor_estimate_poisson_range <- R6Class(
  "factor_estimate_poisson_range",
  inherit = factor_estimate_poisson,
  public = list(
    initialize = function(
      estimated_range_min_value = NULL,
      estimated_range_max_value = NULL,
      estimated_range_size = NULL,
      time_interval_friendly_name = NULL,
      fit_dist = NULL, # Triggers distribution fitting immediately.
      simulate = NULL, # Triggers simulation immediately.
      ...) {
      super$initialize(
        estimation_method_name = "Range 2 points estimate", ...)

      # Initialize lambda parameters
      # to avoid the presence of NULLs.
      self$lambda <- NA

      self$estimated_range_min_value <- estimated_range_min_value
      self$estimated_range_max_value <- estimated_range_max_value

      if(is.null(estimated_range_size)) {
        estimated_range_size <- .9 # TODO: replace with a default configuration setting
      }
      self$estimated_range_size <- estimated_range_size

      if(is.null(fit_dist)) { fit_dist <- TRUE }
      if(fit_dist) { self$fit_dist(...) }

      if(is.null(simulate)) { simulate <- TRUE }
      if(simulate) { self$simulate(...) }

      },
    fit_dist = function(max_iteration = NULL, precision = NULL, verbosity = NULL, ...) {

      if(is.null(verbosity)) { verbosity <- 0 }
      if(is.null(max_iteration)) { max_iteration <- 256 }
      if(is.null(precision)) { precision <- 1 } # Expressed in quantile value.

      # First, we restart from a clean page:
      # ...

      self$fit_dist_location(verbosity = verbosity, ...)
      self$fit_dist_scale(verbosity = verbosity, ...)

      },
    get_print_lines = function(...) {
      return(
        c(super$get_print_lines(),
               "Estimation parameters:",
               paste0(
                    " min = ", fn(self$estimated_range_min_value,2), " (", fn(self$estimated_range_min_proba,2), ")",
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
      if(is.null(output_format)) { output_format = "boolean" }
      consistency_error_count <- super$check_state_consistency(output_format = "int")
      consistency_report <- super$check_state_consistency(output_format = "report")

      # Check if all mandatory parameters have been defined.
      if(is.na(self$estimated_range_min_value)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. range min value is missing."), sep="\n")
      }
      if(is.na(self$estimated_range_max_value)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "est. range max value is missing"), sep="\n")
      }

      if(consistency_error_count == 0)
      {
        # If all parameters are present,
        # we can check consistency between parameters.
        if(self$estimated_range_min_proba >= self$estimated_range_max_proba) {
          consistency_error_count <- consistency_error_count + 1
          consistency_report <- paste0(c("est. range min proba. >= est. range max proba."), sep="\n")
        }
      }

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
    plot_density = function(x_start = NULL, x_end = NULL, ...) {
      # Override the super-class method to append the limits and point estimates.

      if(is.null(x_start) | is.null(x_end))
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
                           self$estimated_range_max_value)

      # Enrich the graph with the estimates represented as vertical lines
      plot_01 <- overplot_vertical_lines(plot_01, x_values = estimates, color = "blue", alpha = .2, ...)

      limits_values <- c()
      if(!is.na(self$limit_min_value))
      {
        limits_values <- c(limits_values, self$limit_min_value)
      }
      if(!is.na(self$limit_max_value))
      {
        limits_values <- c(limits_values, self$limit_max_value)
      }

      # Enrich the graph with the estimates represented as vertical lines
      if(!is.na(self$limit_min_value) | !is.na(self$limit_max_value))
      {
        plot_01 <- overplot_vertical_lines(plot_01, x_values = limits_values, x_labels = NULL, color = "red", alpha = .2, ...)
      }

      return(plot_01)
    },
    plot_probability = function(x_start = NULL, x_end = NULL, ...) {
      # Override the super-class method to append the limits and point estimates.

      if(is.null(x_start) | is.null(x_end))
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
                     self$estimated_range_max_value)

      # Enrich the graph with the estimates represented as vertical lines
      plot_01 <- overplot_vertical_lines(plot_01, x_values = x_estimates, color = "blue", alpha = .2, ...)

      x_limits_values <- c()
      if(!is.na(self$limit_min_value))
      {
        x_limits_values <- c(x_limits_values, self$limit_min_value)
      }
      if(!is.na(self$limit_max_value))
      {
        x_limits_values <- c(x_limits_values, self$limit_max_value)
      }

      # Enrich the graph with the estimates represented as vertical lines
      if(!is.na(self$limit_min_value) | !is.na(self$limit_max_value))
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
      if(missing(value))
      {
        return(get_dist_mode_from_pdf(
          pdf = self$get_density,
          search_range_start = self$estimated_range_min_value,
          search_range_end = self$estimated_range_max_value))
      }
      else { stop("This is a read-only attribute") }},
    estimated_range_min_value = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_estimated_range_min_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_min_value <- NA }
        return(private$private_estimated_range_min_value)
        }
      else {
        # We only do something if something changes... This is important for Shiny apps, etc. to avoid recomputing everything when recomputing is not required
        if(is.na(self$estimated_range_min_value) | value != self$estimated_range_min_value)
        {
        private$private_estimated_range_min_value <- value
        if(self$check_state_consistency()) { self$fit_dist() }
        self$reset_plot_limits() }}},
    estimated_mode_value = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_estimated_mode_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_mode_value <- NA }
        return(private$private_estimated_mode_value)
        }
      else {
        # We only do something if something changes... This is important for Shiny apps, etc. to avoid recomputing everything when recomputing is not required
        if(is.na(self$estimated_mode_value) | value != self$estimated_mode_value)
          {
          private$private_estimated_mode_value <- value
          if(self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits()
          }
        }
      },
    estimated_range_max_value = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_estimated_range_max_value)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_max_value <- NA }
        return(private$private_estimated_range_max_value) }
      else {
        # We only do something if something changes... This is important for Shiny apps, etc. to avoid recomputing everything when recomputing is not required
        if(is.na(self$estimated_range_max_value) | value != self$estimated_range_max_value)
        {
          private$private_estimated_range_max_value <- value
          if(self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits() }}},
    estimated_range_min_proba = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_estimated_range_min_proba)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_min_proba <- NA }
        return(private$private_estimated_range_min_proba) }
      else {
        if(is.na(self$estimated_range_min_proba) | value != self$estimated_range_min_proba)
        {
          private$private_estimated_range_min_proba <- value
          if(self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits() }}},
    estimated_range_max_proba = function(value,...) {
      if(missing(value)) {
        if(is.null(private$private_estimated_range_max_proba)) {
          # If the attribute does not exist, initialize it with NA to prevent errors accessing it.
          private$private_estimated_range_max_proba <- NA }
          return(private$private_estimated_range_max_proba) }
      else {
        if(is.na(self$estimated_range_max_proba) | value != self$estimated_range_max_proba)
        {
          private$private_estimated_range_max_proba <- value
          if(self$check_state_consistency()) { self$fit_dist() }
          self$reset_plot_limits() }}},
    estimated_range_size_proba = function(value,...) {
      # This is a shortcut parameter to estimated range min / max.
      # It computes a centered estimated range.
      if(missing(value)) { return(self$estimated_range_max_proba - self$estimated_range_min_proba) }
      else {
        if(value <= 0){
          stop("estimated_range_size_proba <= 0")
        }
        self$estimated_range_min_proba <- (1 - value) / 2
        self$estimated_range_max_proba <- 1 - (1 - value) / 2
        self$reset_plot_limits() }}
  ),
  private = list(
    private_estimated_range_min_value = NA,
    private_estimated_range_max_value = NA,
    private_estimated_range_min_proba = NA,
    private_estimated_range_max_proba = NA
  )
)
