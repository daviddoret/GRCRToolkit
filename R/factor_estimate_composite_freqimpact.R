if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

#' factor_estimate_composite_freqimpact
#'
#' This class represents a risk model "composite factor estimate based on frequency x impact".\cr
#' This estimation method that is based on:\cr
#'  \itemize{
#'   \item a \code{frequency_factor} modeling the number of times the anticipated event will happen per period of time,
#'   \item an \code{impact_factor} modeling the impact (size, severity) of realized events.
#'   }
#' The impact per period of time is given by \deqn{\sum{i=1}^frequency impact}.\cr
#'
#' @docType class
#' @export
#' @keywords data
#' @return An instance of the factor_estimate_composite \code{\link{R6Class}}.
#' @examples
#' fe1 <- factor_estimate_composite_custom$new()
#' @section Inherits:
#' \describe{
#'   \item{\code{\link{factor_estimate}}}{}
#' }
factor_estimate_composite_freqimpact <- R6Class(
  "factor_estimate_composite_freqimpact",
  inherit = factor_estimate_composite,
  public = list(
    initialize = function(
      limit_min_value = NULL,
      limit_max_value = NULL,
      frequency_factor = NULL,
      impact_factor = NULL,
      ...) {
      # Default values

      # Call the constructor of the parent class
      super$initialize(
        estimation_method_name = "Frequency x Impact",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        ...)

      self$frequency_factor <- frequency_factor
      self$impact_factor <- impact_factor

      self$random_function <- function(
        n = NULL,
        output_class = NULL,
        ...)
        {
        if (is_nanull(output_class)) {output_class <- "vector"}
        if (is_nanull(self$frequency_factor)) { stop("frequency_factor is missing") }
        if (is_nanull(self$impact_factor)) { stop("impact_factor is missing") }
        return(
          freqimpact(
            n = n,
            frequency_function = self$frequency_factor$get_random,
            impact_function = self$impact_factor$get_random,
            output_class = output_class
          )
        )
      }
    },
    check_state_consistency = function(output_format = NULL, ...) {
      # Informs us if the object state is consistent / logical.
      if (is.null(output_format)) { output_format = "boolean" }
      consistency_error_count <- 0
      consistency_report <- NULL

      # Check if all mandatory parameters have been defined.
      if (is_nanull(self$frequency_factor)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "Frequency factor is missing."), sep = "\n")
        }
      if (is_nanull(self$impact_factor)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "Impact factor is missing."), sep = "\n")
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
    get_print_lines = function(...) {
      return(
          c(super$get_print_lines()))
    }
  ),
  active = list(
    frequency_factor = function(value, ...) {
      if (missing(value)) {
        return(private$private_frequency_factor)
      }
      else {
        private$private_frequency_factor <- value
      }
    },
    impact_factor = function(value, ...) {
      if (missing(value)) {
        return(private$private_impact_factor)
      }
      else {
        private$private_impact_factor <- value
      }
    }
  ),
  private = list(
    private_frequency_factor = NA,
    private_impact_factor = NA
  )
)
