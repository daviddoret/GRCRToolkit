library(R6)
library(rriskDistributions)
library(fitdistrplus)

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
                   initialize = function(name, dist = "lnorm",...) {
                     self$dist <- dist
                   },
                   print = function(...) {
                     cat("Model factor (estim min:", self$estim_min,
                         ", estim typical:", self$estim_typical,
                         ", estim max:", self$estim_max,
                         ", range size:", self$range_size,
                         ", dist:", self$dist,
                         "(", self$dist_fitted_params, ")",
                         " )\n",
                         sep = "")
                     invisible(self)
                   },
                   estim_3_points = function(estim_min = NULL,
                                         estim_typical = NULL,
                                         estim_max = NULL,
                                         range_size = .9, ...) {
                     self$estim_min <- estim_min
                     self$estim_typical <- estim_typical
                     self$estim_max <- estim_max
                     self$range_size <- range_size
                   },
                   fit_dist = function() {
                     #fitted <- fitdist(
                      # data = get_dist_fit_quantiles(),
                      # )
                     if(self$dist == "lnorm")
                     {
                       self$fit_dist_lnorm()
                     }
                     else if(self$dist == "norm")
                     {
                       self$fit_dist_norm()
                     }
                     else if(self$dist == "beta")
                     {
                       self$fit_dist_beta()
                     }
                     else
                     {
                       stop("unknown or not yet implemented distribution, sorry")
                     }
                   },
                   fit_dist_norm = function() {
                     fitted <- get.norm.par(p = self$get_dist_fit_probabilities(),
                                             q = self$get_dist_fit_quantiles(),
                                             show.output = TRUE,
                                             plot = FALSE,
                                             tol = model_config_get_option("dist_fit", "tol"),
                                             fit.weights = self$get_dist_fit_probabilities())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   fit_dist_lnorm = function() {
                     fitted <- get.lnorm.par(p = self$get_dist_fit_probabilities(),
                                            q = self$get_dist_fit_quantiles(),
                                            show.output = TRUE,
                                            plot = FALSE,
                                            tol = model_config_get_option("dist_fit", "tol"),
                                            fit.weights = self$get_dist_fit_probabilities())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   fit_dist_beta = function() {
                     fitted <- get.beta.par(p = self$get_dist_fit_probabilities(),
                                             q = self$get_dist_fit_quantiles(),
                                             show.output = TRUE,
                                             plot = FALSE,
                                             tol = model_config_get_option("dist_fit", "tol"),
                                             fit.weights = self$get_dist_fit_probabilities())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   # Returns a vector of probabilities for distribution fitting.
                   # In RRiskDistribution package, this typically correspond to
                   # the 'p' parameter.
                   get_dist_fit_probabilities = function(){
                     p1 <- self$range_min
                     p2 <- self$range_typical
                     p3 <- self$range_max
                     return(c(p1,p2,p3))
                   },
                   # Returns a vector of quantiles for distribution fitting.
                   # In RRiskDistribution package, this typically correspond to
                   # the 'q' parameter.
                  get_dist_fit_quantiles = function(){
                    q1 <- self$estim_min
                    q2 <- self$estim_typical
                    q3 <- self$estim_max
                     return(c(q1,q2,q3))
                  },
                  get_dist_fit_weights = function(){
                    return( model_config_get_option("dist_fit", "weights", self$dist) )
                  },
                  set_dist_fitted_params = function(...) {
                      private$private_dist_fitted_params <- c(...)
                  },
                  get_random = function(n, ...) {
                    if(self$dist == "lnorm") {
                      return(
                        rlnorm(n=n,
                               meanlog=self$dist_fitted_params["meanlog"],
                               sdlog=self$dist_fitted_params["sdlog"]))
                    }
                    else if(self$dist == "norm") {
                      return(
                        rnorm(n=n,
                               mean=self$dist_fitted_params["mean"],
                               sd=self$dist_fitted_params["sd"]))
                    }
                    else if(self$dist == "beta") {
                      return(
                        rbeta(n=n,
                              shape1=self$dist_fitted_params["shape1"],
                              shape2=self$dist_fitted_params["shape2"]))
                    }
                    else {
                      stop("distribution not supported, sorry")
                    }
                  },
                  get_proba_density = function(x) {
                    if( self$dist == "lnorm" ) {
                      return(
                        dlnorm(
                          x = x,
                          meanlog = self$dist_fitted_params["meanlog"],
                          sdlog = self$dist_fitted_params["sdlog"],
                          log = FALSE ))}
                    else if( self$dist == "norm" ){
                      return(
                        dnorm(
                          x,
                          mean = f1$dist_fitted_params["mean"],
                          sd = f1$dist_fitted_params["sd"],
                          log = FALSE ))}
                    else {
                      stop("distribution not implemented, sorry")}},
                  get_proba_density_plot = function() {

                    # Prepare the data
                    df <- data.frame(x=c(self$estim_min - (self$estim_max - self$estim_min) * .2,
                                         self$estim_max + (self$estim_max - self$estim_min) * .2))

                    # Configure the graph
                    p <- ggplot(df, aes(x)) +

                      # Give a little bit of margin on the graph sides
                      xlim(self$estim_min - (self$estim_max - self$estim_min) * .2
                           ,self$estim_max + (self$estim_max - self$estim_min) * .2) +
                      #ylim: let it scale automatically

                      # Axis titles
                      ylab("Relative likelihood")  +
                      xlab("Factor value")  +

                      # Limit the number of digits on the vertical axis
                      scale_y_continuous(label = function(x) { round(x,3) }) +

                      # Display 3 vertical bars to highlight the 3 points of the estimate
                      geom_vline(xintercept = self$get_dist_fit_quantiles()[1]) +
                      geom_vline(xintercept = self$get_dist_fit_quantiles()[2]) +
                      geom_vline(xintercept = self$get_dist_fit_quantiles()[3]) +

                      # Area plot the PDF function with a neutral background
                      stat_function(fun = prob_density_fun,
                                    colour = "#555555",
                                    geom = "area",
                                    fill = "white",
                                    alpha = 0.5 ) +

                      # Area plot of PDF function within the estimation range with a vivid background
                      stat_function(
                        colour = "#00aa00",
                        fun = prob_density_fun,
                        geom = 'area',
                        fill = 'green',
                        alpha = 0.1,
                        size = 1.1,
                        xlim = c(self$get_dist_fit_quantiles()[1],self$get_dist_fit_quantiles()[3])) +

                      # On top of the rest, label the 3 vertical bars
                      annotate(geom = "text", x = self$get_dist_fit_quantiles()[2], y = 0, label = "Typical", angle = 90, hjust = -1, vjust = -.2) +
                      annotate(geom = "text", x = self$get_dist_fit_quantiles()[1], y = 0, label = "Min", angle = 90, hjust = -1, vjust = -.2) +
                      annotate(geom = "text", x = self$get_dist_fit_quantiles()[3], y = 0, label = "Max", angle = 90, hjust = -1, vjust = -.2) +

                      # And put a title on top of it
                      ggtitle("Probability density function")

                    return(p)
                  }
                 ),
                 active = list(
                   dist = function(value,...) {
                     if(missing(value)) return(private$private_dist)
                     else {
                       private$private_dist <- value
                     }
                   },
                   dist_fitted_params = function(value, ...) {
                     if(missing(value)) return(private$private_dist_fitted_params)
                     else {
                       private$private_dist_fitted_params <- value
                     }
                   },
                   estim_max = function(value) {
                     if (missing(value)) return(private$private_estim_max)
                     else {
                       stopifnot(is.numeric(value),
                                 is.null(self$estim_min) | self$estim_min < value,
                                 is.null(self$typical) | self$typical < value)
                       private$private_estim_max <- value
                     }
                   },
                   estim_min = function(value) {
                     if (missing(value)) return(private$private_estim_min)
                     else {
                       stopifnot(is.numeric(value))
                       private$private_estim_min <- value
                     }
                   },
                   estim_typical = function(value) {
                     if (missing(value)) return(private$private_estim_typical)
                     else private$private_estim_typical <- value
                   },
                   range_max = function(value) {
                     if (missing(value)) {
                       return( 1 - (1 - self$range_size) / 2 )
                     }
                     else {
                       stop("range_max cannot be set. please set range_size instead.")
                     }
                   },
                   range_min = function(value) {
                     if (missing(value)) {
                       return( (1 - self$range_size) / 2 )
                     }
                     else {
                       warning("range_min cannot be set. please set range_size instead.")
                     }
                   },
                   range_typical = function(value) {
                     if (missing(value)) {
                       return( .5 )
                     }
                     else {
                       warning("range_typical cannot be set. please set range_size instead.")
                     }
                   },
                   range_size = function(value) {
                     if (missing(value)) return(private$private_range_size)
                     else {
                       stopifnot(is.numeric(value),value > 0, value < 1)
                       private$private_range_size <- value
                     }
                   }
                 ),
                 private = list(
                   private_dist = NULL,
                   private_dist_fitted_params = NULL,
                   private_estim_min = NULL,
                   private_estim_typical = NULL,
                   private_estim_max = NULL,
                   private_range_size = NULL
                 )
)
