if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

#' An abstract class for a GLD-based factor estimates.
#' Subclasses may inherit from it to implement various estimation techniques,
#' such as the 3 points estimate which is going to be the first
#' implementation.
#'
#' For the time being, I only support the FMKH parameterization. If
#' other parameterizations become necessary in the future, these will
#' require specifically dedicated R6 classes because the way we tweak
#' lambda parameters here is strongly linked to FMKH logic.
#'
#' TODO:
#' - data validation and general help to friendly manage lambda parameters
#'
#' @export
factor_estimate_gld <- R6Class(
  "factor_estimate_gld",
  inherit = factor_estimate,
  public = list(
    initialize = function(...) {
      super$initialize(
        distribution_name = "Generalized Lambda (aka Tukey Lambda)", ...)
      #super$distribution_name <- "GLD"
      self$density_function <- function(x){return(dgl(x = x, lambda1 = self$lambda1, lambda2 = self$lambda2, lambda3 = self$lambda3, lambda4 = self$lambda4))}
      self$probability_function <- function(q){return(pgl(q = q, lambda1 = self$lambda1, lambda2 = self$lambda2, lambda3 = self$lambda3, lambda4 = self$lambda4))}
      self$quantile_function <- function(p){return(qgl(p = p, lambda1 = self$lambda1, lambda2 = self$lambda2, lambda3 = self$lambda3, lambda4 = self$lambda4))}
      self$random_function <- function(n){return(rgl(n = n, lambda1 = self$lambda1, lambda2 = self$lambda2, lambda3 = self$lambda3, lambda4 = self$lambda4))}
    },
    get_print_lines = function(...) {
      return(
          c(super$get_print_lines(),
          "Fitted distribution parameters:",
          paste0(
            " λ1 = ", fn(self$lambda1,4),
            " ,λ2 = ", fn(self$lambda2,4),
            " ,λ3 = ", fn(self$lambda3,4),
            " ,λ4 = ", fn(self$lambda4,4)
            )))
    }
  ),
  active = list(
    # lambda1: location parameter, or α for the gpd parameterisation
    lambda1 = function(value,...) {
      if(missing(value)) { return(private$private_lambda1) }
      else { private$private_lambda1 <- value }},
    lambda2 = function(value,...) {
      if(missing(value)) { return(private$private_lambda2) }
      else { private$private_lambda2 <- value }},
    lambda3 = function(value,...) {
      if(missing(value)) { return(private$private_lambda3) }
      else { private$private_lambda3 <- value }},
    lambda4 = function(value,...) {
      if(missing(value)) { return(private$private_lambda4) }
      else { private$private_lambda4 <- value }}
  ),
  private = list(
    private_lambda1 = NULL,
    private_lambda2 = NULL,
    private_lambda3 = NULL,
    private_lambda4 = NULL
  )
)
