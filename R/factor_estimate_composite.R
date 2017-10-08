if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

#' factor_estimate_composite
#'
#' A risk model "composite factor estimate".
#' A "composite factor estimate" is a factor estimation method that is based on \code{n} sub-factors.
#' The sub-factors may in their turn be estimated with arbitrary estimation methods.
#' \code{factor_estimate_composite} is an "abstract" class that shouldn't be used directly.
#' Instead, use the specialized classes that inherits from it.
#'
#' @docType class
#' @export
#' @keywords data
#' @return An instance of the factor_estimate_composite \code{\link{R6Class}}.
#' @examples
#' fe1 <- factor_estimate_composite$new()
#' @section Inherits:
#' \describe{
#'   \item{\code{\link{factor_estimate}}}{}
#' }
factor_estimate_composite <- R6Class(
  "factor_estimate_composite",
  inherit = factor_estimate,
  public = list(
    initialize = function(
      limit_min_value = NULL,
      limit_max_value = NULL,
      ...) {
      super$initialize(
        distribution_name = "Composite",
        limit_min_value = limit_min_value,
        limit_max_value = limit_max_value,
        ...)
      self$density_function <- function(
        x, ...){
        return(stop("Not implemented"))}
      self$probability_function <- function(
        q, ...){
        return(stop("Not implemented"))}
      self$quantile_function <- function(
        p, ...){
        return(stop("Not implemented"))}
      self$random_function <- function(
        n, ...){
        return(stop("Not implemented"))}
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
