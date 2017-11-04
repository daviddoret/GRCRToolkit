if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

#' factor_estimate_composite_custom
#'
#' This class represents a risk model "composite factor estimate with custom logic".\cr
#' A "composite factor estimate with custom logic" is a factor estimation method that is based on:\cr
#'  \itemize{
#'   \item a set of \code{dependent_factors} (aka sub-factors),
#'   \item a \code{random_function} that applies arbitrary logic on the input \code{dependent_factors} to model the factor's random variable.
#'   }
#' The \code{dependent_factors} may in their turn be estimated with arbitrary estimation methods.\cr
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
factor_estimate_composite_custsom <- R6Class(
  "factor_estimate_composite_custom",
  inherit = factor_estimate_composite,
  public = list(
    initialize = function(
      limit_min_value = NULL,
      limit_max_value = NULL,
      random_function = NULL,
      ...) {
      # Default values
      if(is_void(random_function)) {
        random_function <- function(n, ...){
          warning("Sorry, this function is not available.")
          return(NA)
        }
      }
      # Call the constructor of the parent class
      super$initialize(
        estimation_method_name = "Custom Logic",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        ...)
      self$random_function <- random_function
    },
    check_state_consistency = function(output_format = NULL, ...) {
        return(stop("Not implemented"))
    },
    get_print_lines = function(...) {
      return(
          c(super$get_print_lines()))
    }
  ),
  active = list(
  ),
  private = list(
  )
)
