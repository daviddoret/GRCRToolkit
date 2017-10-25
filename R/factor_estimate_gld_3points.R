require(R6)
require(gld)

#' factor_estimate_gld_3points
#'
#' A PERT-like factor estimate based on the Generalized Lambda (aka Tukey) Distribution.\cr
#'
#' For the time being, I only support GLD's FMKH parameterization.
#' If other parameterizations become necessary / interesting in the future,
#' these will require specifically dedicated R6 classes,
#' because the way we tweak lambda parameters here is strongly linked to FMKH logic.\cr
#'
#' Future enhancements:
#' - Make lambda parameters read-only for class users
#'   in such a way as to guarantee consistency between
#'   the fitted distribution and the estimation parameters.
#'
#' @docType class
#' @export
#' @keywords data
#' @return An instance of the \code{factor_estimate_gld_3points} \code{\link{R6Class}}.
#' @examples
#' fe1 <- factor_estimate_gld_3points$new(
#'   estimated_range_min_value = 10,
#'   estimated_mode_value = 90,
#'   estimated_range_max_value = 100,
#'   estimated_range_size_proba = .9,
#'   limit_min_value = 0,
#'   limit_max_value = 200)
#'fe1$plot_density()
#' @field estimated_range_min_value The lower value of the 3 points estimate.
#' @field estimated_mode_value The "typical" value of the 3 points estimate.
#' @field estimated_range_max_value The upper value of the 3 points estimate.
#' @field estimated_range_size_proba Default: .9. Possible values: 0 < x < 1. The size of the probabilistic range estimate. The default .9 leaves .05 on both sides of the distribution.
#' @field limit_min_value A strict lower limit. 0 if you don't consider positive risks.
#' @field limit_max_value A strict upper limit. May be the company stock value if you don't consider externalities.
#' @section Inherits:
#' \describe{
#'   \item{\code{\link{factor_estimate_gld}}}{}
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(estimated_range_min_value, estimated_mode_value, estimated_range_max_value, estimated_range_size_proba, limit_min_value, limit_max_value)}}{Create a new object of this class.}
#'   \item{\code{plot_density()}}{Plot the PDF.}
#' }
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
      verbosity = NULL,
      ...) {

      if (is_nanull(verbosity)) { verbosity <- 0 }

      super$initialize(
        estimation_method_name = "PERT-like 3 points estimate",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        verbosity = verbosity - 1,
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

      if (fit_dist) { self$fit_dist(verbosity = verbosity, ...) }
      if (simulate) { self$simulate(verbosity = verbosity, ...) }
      },
    fit_dist = function(
      max_iteration = NULL,
      precision = NULL,
      verbosity = NULL, ...) {

      if (is.null(verbosity)) { verbosity <- 0 }
      if (is.null(max_iteration)) { max_iteration <- 256 }
      if (is.null(precision)) { precision <- 1 } # Expressed in quantile value.

      lambdas <- fit_gld_3points(
        estimated_range_min_value = self$estimated_range_min_value,
        estimated_mode_value = self$estimated_mode_value,
        estimated_range_max_value = self$estimated_range_max_value,
        estimated_range_size_proba = self$estimated_range_size_proba,
        verbosity = verbosity - 1,
        max_iteration = max_iteration,
        precision = precision,
        ...)

      self$lambda1 <- lambdas[1]
      self$lambda2 <- lambdas[2]
      self$lambda3 <- lambdas[3]
      self$lambda4 <- lambdas[4]
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
        return(get_gld_mode(
          lambda1 = self$lambda1,
          lambda2 = self$lambda2,
          lambda3 = self$lambda3,
          lambda4 = self$lambda4,
          search_range_start = self$estimated_range_min_value,
          search_range_end = self$estimated_range_max_value,
          ...))
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
