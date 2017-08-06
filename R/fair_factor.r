library(R6)
library(rriskDistributions)

#' FAIR factors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' plot_crayons()
#'
#' @export
fair_factor <- R6Class("fair_factor",
                 public = list(
                   initialize = function(...) {
                   },
                   print = function(...) {
                     cat("FAIR Factor (estim min:", self$estimate_min,
                         ", estim typical:", self$estimate_typical,
                         ", estim max:", self$estimate_max,
                         ", range size:", self$range_size,
                         " )\n",
                         sep = "")
                     invisible(self)
                   },
                   estimate_3_points = function(estimate_min = NULL,
                                         estimate_typical = NULL,
                                         estimate_max = NULL,
                                         range_size = .9, ...) {
                     self$estimate_min <- estimate_min
                     self$estimate_typical <- estimate_typical
                     self$estimate_max <- estimate_max
                     self$range_size <- range_size
                   },
                   fit_dist_lnorm = function() {
                     fitted <- get.lnorm.par(p = self$get_dist_fit_probabilities(),
                                            q = self$get_dist_fit_quantiles(),
                                            show.output=TRUE,
                                            plot=TRUE,
                                            tol = 0.01,
                                            fit.weights = c(1, 1, 1)
                    )
                   },
                   #' get_dist_fit_probabilities
                   #'
                   #' Returns a vector of probabilities for distribution fitting.
                   #' In RRiskDistribution package, this typically correspond to
                   #' the 'p' parameter.
                   #'
                   #' @return A vector of probabilities
                   #'
                   #' @examples
                   #' foo$get_dist_fit_probabilities()
                   #'
                   #' @export
                   get_dist_fit_probabilities = function(){
                     p1 <- self$range_min
                     p2 <- self$range_typical
                     p3 <- self$range_max
                     return(c(p1,p2,p3))
                   },
                   #' get_dist_fit_quantiles
                   #'
                   #' Returns a vector of quantiles for distribution fitting.
                   #' In RRiskDistribution package, this typically correspond to
                   #' the 'q' parameter.
                   #'
                   #' @return A vector of quantiles
                   #'
                   #' @examples
                   #' foo$get_dist_fit_quantiles()
                   #'
                   #' @export
                  get_dist_fit_quantiles = function(){
                    q1 <- self$estimate_min
                    q2 <- self$estimate_typical
                    q3 <- self$estimate_max
                     return(c(q1,q2,q3))
                   }
                 ),
                 active = list(
                   estimate_max = function(value) {
                     if (missing(value)) return(private$private_estimate_max)
                     else {
                       stopifnot(is.numeric(value),
                                 is.null(self$estimate_min) | self$estimate_min < value,
                                 is.null(self$typical) | self$typical < value)
                       private$private_estimate_max <- value
                     }
                   }
                   ,estimate_min = function(value) {
                     if (missing(value)) return(private$private_estimate_min)
                     else {
                       stopifnot(is.numeric(value))
                       private$private_estimate_min <- value
                     }
                   }
                   ,range_max = function(value) {
                     if (missing(value)) {
                       return( 1 - (1 - self$range_size) / 2 )
                     }
                     else {
                       stop("range_max cannot be set. please set range_size instead.")
                     }
                   }
                   ,range_min = function(value) {
                     if (missing(value)) {
                       return( (1 - self$range_size) / 2 )
                     }
                     else {
                       warning("range_min cannot be set. please set range_size instead.")
                     }
                   }
                   ,range_typical = function(value) {
                     if (missing(value)) {
                       return( .5 )
                     }
                     else {
                       warning("range_typical cannot be set. please set range_size instead.")
                     }
                   }
                   ,range_size = function(value) {
                     if (missing(value)) return(private$private_range_size)
                     else {
                       stopifnot(is.numeric(value),value > 0, value < 1)
                       private$private_range_size <- value
                     }
                   }
                   ,estimate_typical = function(value) {
                     if (missing(value)) return(private$private_estimate_typical)
                     else private$private_estimate_typical <- value
                   }
                 ),
                 private = list(
                   private_estimate_min = NULL,
                   private_estimate_typical = NULL,
                   private_estimate_max = NULL,
                   private_range_size = NULL
                 )
)
