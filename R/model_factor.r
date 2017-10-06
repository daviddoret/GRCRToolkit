if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6)

#' model_factor
#'
#' Represent a component of a \code{\link{model}}
#'
#' @param name the unique name used to identify this factor within its model
#'
#' @param dist the theoretical probability distribution of this factor
#'
#' @return an instance of a mode factor
#'
#' @examples
#' f1 <- model_factor$new(name = "x", dist = "lnorm")
#'
#' @export
model_factor <- R6Class("model_factor",
                 public = list(
                   initialize = function(name, ...) {
                     self$name <- name
                   },
                   print = function(...) {
                     cat("Model factor (estim probas:", self$estim_probas,
                         ", estim quantiles:", self$estim_quantiles,
                         ", estim weights:", self$estim_weights,
                         ", dist:", self$dist,
                         "(", self$dist_fitted_params, ")",
                         " )\n",
                         sep = "")
                     invisible(self)
                   },

                  get_random = function(n, ...) {
                    stop("function was not initialized")
                  },
                  get_density = function(x, ...) {

                  }
                 ),
                 active = list(
                   dist = function(value,...) {
                     if(missing(value)) {
                       return(private$private_dist) }
                     else {
                       private$private_dist <- value }
                   },
                   dist_fitted_params = function(value, ...) {
                     if(missing(value)) return(private$private_dist_fitted_params)
                     else {
                       private$private_dist_fitted_params <- value
                     }
                   }
                 ),
                 private = list(
                   private_dist = NULL,
                   private_dist_fitted_params = NULL
                 )
)
