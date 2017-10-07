if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6)

options(digits = 22)

#' Poisson Range (2 points) estimate
#'
#' @export
#'
#'
factor_estimate_poisson_mode <- R6Class(
  "factor_estimate_poisson_mode",
  inherit = factor_estimate_poisson,
  public = list(
    initialize = function(
      estimated_mode_value = NULL,
      time_interval_friendly_name = NULL,
      limit_min_value = NULL,
      limit_max_value = NULL,
      fit_dist = NULL, # Triggers distribution fitting immediately.
      simulate = NULL, # Triggers simulation immediately.
      ...) {

      # Default values
      if (is.null(estimated_mode_value)) { estimated_mode_value <- NA }
      if (is.null(fit_dist)) { fit_dist <- TRUE }
      if (is.null(simulate)) { simulate <- TRUE }

      super$initialize(
        estimation_method_name = "Mode estimate",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        ...)

      # Initialize lambda parameters
      # to avoid the presence of NULLs.
      self$lambda <- NA
      self$estimated_mode_value <- estimated_mode_value

      if (fit_dist) { self$fit_dist(...) }
      if (simulate) { self$simulate(...) }

      },
    fit_dist = function(...) {
      self$lambda <- fit_poisson_mode(estimated_mode_value = self$estimated_mode_value, ...)
      },
    get_print_lines = function(...) {
      return(
        c(super$get_print_lines(),
               "Estimation parameters:",
               paste0(
                    " mode = ", fn(self$estimated_mode_value,2)),
                    "Fitted quantiles:",
               paste0(
                    " mode = ", fn(self$dist_mode, 2)),
               "Fitted probabilities:",
               paste0(
                    " ,mode = ", fn(self$dist_mode, 2))
                    ))
    },
    check_state_consistency = function(output_format = NULL, ...) {
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
      if (is.na(self$estimated_mode_value)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "estimated mode value is missing."), sep = "\n")
      }

      if (consistency_error_count == 0)
      {
        # If all parameters are present,
        # we can check consistency between parameters.
        # N/A
      }

      # And eventually output the conclusion in the desired format.
      if ( output_format == "boolean")
      {
        return(consistency_error_count == 0)
      }
      else if ( output_format == "int")
      {
        return(consistency_error_count)
      }
      else if ( output_format == "report")
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

      # Prepare a vector with the estimation parameters
      estimates <- c(self$estimated_mode_value)

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
    }
  ),
  active = list(
    dist_mode = function(value,...) {
      if (missing(value))
      {
        # Estimated mode = lambda = fitted distribution mode
        # So this function is ridiculously trivial
        return(self$estimated_mode_value)
      }
      else {stop("This is a read-only attribute")}},
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
          }
        }
      }
  ),
  private = list(
    private_estimated_mode_value = NA
  )
)
