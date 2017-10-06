if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6)

#' An abstract class for a Poisson-based factor estimates.
#' Subclasses may inherit from it to implement various estimation techniques,
#' such as the 3 points estimate which is the first planned implementation.
#'
#' @export
factor_estimate_poisson <- R6Class(
  "factor_estimate_poisson",
  inherit = factor_estimate,
  public = list(
    initialize = function(
      lambda = NULL,
      time_interval_friendly_name = NULL,
      limit_min_value = NA,
      limit_max_value = NA,
      ...) {
      super$initialize(
        distribution_name = "Poisson",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        ...)

      if(is.null(lambda)) { lambda <- NA }
      if(is.null(time_interval_friendly_name)) { time_interval_friendly_name <- NA }

      self$lambda <- lambda
      self$time_interval_friendly_name <- time_interval_friendly_name

      self$density_function <- function(x){return(dpois(x = x, lambda = self$lambda))}
      self$probability_function <- function(q){return(ppois(q = q, lambda = self$lambda))}
      self$quantile_function <- function(p){return(qpois(p = p, lambda = self$lambda))}
      self$random_function <- function(n){return(rpois(n = n, lambda = self$lambda))}
    },
    check_state_consistency = function(output_format = NULL,...) {
      # Informs us if the object state is consistent / logical.
      # This makes it possible to prevent useless calls to expensive functions
      # that may output multitude of warnings and errors when we know
      # from the beginning that this parameterization is doomed to failure.
      # Returns TRUE if parameters are consistent.
      # Returns a descriptive
      if(is.null(output_format)) { output_format = "boolean" }
      consistency_error_count <- super$check_state_consistency(output_format = "int")
      consistency_report <- super$check_state_consistency(output_format = "report")

      # Check if all mandatory parameters have been defined.
      if(is.na(self$lambda)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb is missing."), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }
      else
      {
        # Lambda must be greater than 0.
        if(self$lambda <= 0) {
          consistency_error_count <- consistency_error_count + 1
          consistency_report <- paste0(c(consistency_report, "\U3bb <= 0"), sep = "\n") # Unicode 3bb = 	greek small letter lamda
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
    get_print_lines = function(...) {
      return(
          c(super$get_print_lines(),
          "Distribution parameters:",
          paste0(" \U3bb = ", fn(self$lambda,4)),  # Unicode 3bb = 	greek small letter lamda
          "Complementary parameters:",
          paste0(" time interval = ", self$time_interval_friendly_name)))
    }
  ),
  active = list(
    lambda = function(value,...) {
      if(missing(value)) { return(private$private_lambda) }
      else { private$private_lambda <- value }},
    time_interval_friendly_name = function(value,...) {
      if(missing(value)) { return(private$private_time_interval_friendly_name) }
      else { private$private_time_interval_friendly_name <- value }}
  ),
  private = list(
    private_lambda = NULL,
    private_time_interval_friendly_name = NULL
  )
)
