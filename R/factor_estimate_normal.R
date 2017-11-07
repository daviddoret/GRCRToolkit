require(R6)

#' factor_estimate_normal
#'
#' A class for factor estimates based on the normal distribution ("Gaussian").
#' \cr It may be inherited by subclasses, e.g. to implement complementary estimation techniques.
#'
#' @docType class
#' @export
#' @keywords data
#' @return An instance of the \code{factor_estimate_normal} \code{\link{R6Class}}.
#' @examples
#' f1 <- factor_estimate_normal$new(sd = 5, mean = 100)
#' @field sd (numeric, scalar) The standard deviation parameter of the normal distribution.
#' @field mean (numeric, scalar) The mean parameter of the normal distribution.
#' @field limit_min_value A strict lower bound applied to the factor simulation values. If NULL or NA, no lower bound will be applied.
#' @field limit_max_value A strict upper bound applied to the factor simulation values. If NULL or NA, no upper bound will be applied.
#' @field limit_min_behavior One of the following options determining how values will be maintained within \code{limit_min_value}: \code{"Limit"} (default), \code{"Replace"}, \code{"Discard"}. \code{"Limit"}: When an out of bound value is drawn, apply \code{min}/\code{max} functions to force it within bounds. \code{"Replace"}: When an out of bound value is drawn, we replace it until it is within bound. \code{"Discard"}: When an out of bound value is drawn, remove it from the sample.
#' @field limit_max_behavior One of the following options determining how values will be maintained within \code{limit_max_value}: \code{"Limit"} (default), \code{"Replace"}, \code{"Discard"}. \code{"Limit"}: When an out of bound value is drawn, apply \code{min}/\code{max} functions to force it within bounds. \code{"Replace"}: When an out of bound value is drawn, we replace it until it is within bound. \code{"Discard"}: When an out of bound value is drawn, remove it from the sample.
#' @section Inherits:
#' \describe{
#'   \item{\code{\link{factor_estimate}}}{}
#' }
#' @section Methods:
#' \describe{
#'   \item{get_random(n = 1, output_class = "vector")}{ Returns a random sample of size \code{n}. Returns a vector by default. If \code{output_class} = "data.frame", returns a data.frame with a column "factor_value". This second parameterization may be enriched by R6 subclasses to provide additional columns with complementary information. }
#' }
#' @export
factor_estimate_normal <- R6Class(
  "factor_estimate_normal",
  inherit = factor_estimate,
  public = list(
    initialize = function(
      sd = NULL,
      mean = NULL,
      limit_min_value = NULL,
      limit_min_behavior = NULL,
      limit_max_value = NULL,
      limit_max_behavior = NULL,
      verbosity = NULL,
      ...) {

      # Parameters validation
      verbosity <- vp(verbosity, 1, "numeric", 1)
      sd <- vp(sd, 1, "numeric", 1)
      mean <- vp(mean, 0, "numeric", 1)

      # Superclass initialisation
      super$initialize(
        distribution_name = "Normal",
        distribution_type = "Continuous",
        limit_min_value = limit_min_value,
        limit_min_behavior = limit_min_behavior,
        limit_max_value = limit_max_value,
        limit_max_behavior = limit_max_behavior,
        verbosity = verbosity - 1,
        ...)

      # Properties initialisation
      self$sd <- sd
      self$mean <- mean

      self$density_function <- function(x, mean = NULL, sd = NULL, ...){
        # Parameters Validation
        mean <- vp(mean, self$mean, "numeric", 1)
        sd <- vp(sd, self$sd, "numeric", 1)
        # Result
        return(dnorm(x = x, mean = mean, sd = sd, ...))}

      self$probability_function <- function(q, mean = NULL, sd = NULL, ...){
        # Parameters Validation
        mean <- vp(mean, self$mean, "numeric", 1)
        sd <- vp(sd, self$sd, "numeric", 1)
        # Result
        return(pnorm(q = q, mean = mean, sd = sd, ...))}

      self$quantile_function <- function(p, mean = NULL, sd = NULL, ...){
        # Parameters Validation
        mean <- vp(mean, self$mean, "numeric", 1)
        sd <- vp(sd, self$sd, "numeric", 1)
        # Result
        return(qnorm(p = p, mean = mean, sd = sd, ...))}

      self$random_function <- function(n, mean = NULL, sd = NULL, ...){
        # Parameters Validation
        mean <- vp(mean, self$mean, "numeric", 1)
        sd <- vp(sd, self$sd, "numeric", 1)
        # Result
        return(rnorm(n = n, mean = mean, sd = sd, ...))}

    },

    check_state_consistency = function(output_format = NULL, ...) {
      stop("Review the way this is implemented")
      },

    get_print_lines = function(...) {
      return(
          c(super$get_print_lines(),
          "Distribution parameters:",
          paste0(
            " mean = ", fn(self$mean,4),
            " ,sd = ", fn(self$sd,4)
            )))
    }
  ),
  active = list(
    mean = function(value,...) {
      if (missing(value)) { return(private$private_mean) }
      else {private$private_mean <- value }},
    sd = function(value,...) {
      if (missing(value)) { return(private$private_sd) }
      else {private$private_sd <- value }}
  ),
  private = list(
    private_mean = NULL,
    private_sd = NULL
  )
)
