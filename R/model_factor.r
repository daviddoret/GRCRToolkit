library(R6)
library(rriskDistributions)
library(fitdistrplus)
#library(GRCRToolkit)
library(ggplot2)

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
                   initialize = function(name, dist, ...) {
                     self$dist <- dist
                   },
                   estim_probas = NULL,
                   estim_labels = NULL,
                   estim_quantiles = NULL,
                   estim_weights = NULL,
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
                   estim_3_points_betapert = function(estim_min = NULL,
                                         estim_typical = NULL,
                                         estim_max = NULL,
                                         range_size = NULL,
                                         lambda = NULL,
                                         ...) {
                     # This is a helper function to facilitate
                     # 3 points simplistic expert estimates
                     # fitted to the beta-PERT distribution.

                     if(is.null(range_size)) {
                       range_size = model_config_get_option("dist_fit", "3_points_pert", "range_size")
                     }

                     if(is.null(lambda)) {
                       lambda = model_config_get_option("dist_fit", "3_points_pert", "lambda")
                     }

                     self$dist <- "betapert"
                     self$dist_fitted_params["lambda"] <- lambda

                     estim_quantiles <- c(estim_min, estim_typical, estim_max)
                     #estim_probas <- c((1 - range_size) / 2,
                      #                 .5,
                      #                 1 - (1 - range_size) / 2)
                     estim_weights <- c(1, 1, 1)
                     estim_labels <- c("Min.", "Typical", "Max.")

                     # Stores the vectors locally
                     self$estim_probas <- NULL
                     self$estim_quantiles <- estim_quantiles
                     self$estim_labels <- estim_labels
                     self$estim_weights <- estim_weights
                   },
                   estim_n_points = function(
                     estim_probas,
                     estim_quantiles,
                     estim_labels = NULL,
                     estim_weights = NULL,
                     ...) {

                     # Data consistency checks
                     if(is.null(estim_probas) || length(estim_probas) == 0) { stop("estim_probas is required") }
                     if(is.null(estim_quantiles) || length(estim_quantiles) == 0) { stop("estim_quantiles is required") }
                     if(length(estim_probas) != length(estim_quantiles)) { stop("lengths of estim_probas and estim_quantiles are not identical") }
                     if(is.null(estim_weights)) {estim_weights <- rep(x = 1, times = length(estim_probas)) }
                     if(length(estim_probas) != length(estim_weights)) { stop("lengths of estim_probas and estim_weights are not identical") }
                     # QUESTION: if NULL, should we populate empty labels anyway? Take a decision later once graphs will be adapted.
                     # QUESTION: input vector items are not necessarily ordered by quantiles, any consequence?
                     # QUESTION: vector of quantiles may contain duplicates, any issue? I guess no, i.e. we could have several estimates with multiple weights for the same quantile. To be tested...
                     # TODO: Check the cumulative consistency of probas

                     # Stores the vectors locally
                     self$estim_probas <- estim_probas
                     self$estim_quantiles <- estim_quantiles
                     self$estim_labels <- estim_labels
                     self$estim_weights <- estim_weights
                   },
                   fit_dist = function() {
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
                     else if(self$dist == "pert") {
                       # When named "pert", we use the rriskdistributions
                       # package for fitting.
                       self$fit_dist_pert()
                     }
                     else if(self$dist == "betapert") {
                       # When named "betapert", we use our custom
                       # fitting implementation. See the function
                       # for more details.
                       self$fit_dist_betapert()
                     }
                     else
                     {
                       stop("unknown or not yet implemented distribution, sorry")
                     }
                   },
                   fit_dist_norm = function() {
                     fitted <- get.norm.par(
                       p = self$estim_probas, #get_dist_fit_probabilities(),
                       q = self$estim_quantiles, #get_dist_fit_quantiles(),
                       show.output = TRUE,
                       plot = FALSE,
                       tol = model_config_get_option("dist_fit", "tol"),
                       fit.weights = self$estim_weights) #self$get_dist_fit_weights())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   fit_dist_lnorm = function() {
                     fitted <- get.lnorm.par(
                       p = self$estim_probas, #get_dist_fit_probabilities(),
                       q = self$estim_quantiles, #get_dist_fit_quantiles(),
                       show.output = TRUE,
                       plot = FALSE,
                       tol = model_config_get_option("dist_fit", "tol"),
                       fit.weights = self$estim_weights) #self$get_dist_fit_weights())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   fit_dist_beta = function() {
                     fitted <- get.beta.par(
                       p = self$estim_probas, #get_dist_fit_probabilities(),
                       q = self$estim_quantiles, #get_dist_fit_quantiles(),
                       show.output = TRUE,
                       plot = FALSE,
                       tol = model_config_get_option("dist_fit", "tol"),
                       fit.weights = self$estim_weights) #self$get_dist_fit_weights())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   fit_dist_pert = function() {
                     fitted <- get.pert.par(
                       p = self$estim_probas, #get_dist_fit_probabilities(),
                       q = self$estim_quantiles, #get_dist_fit_quantiles(),
                       show.output = TRUE,
                       plot = FALSE,
                       tol = model_config_get_option("dist_fit", "tol"),
                       fit.weights = self$estim_weights) #self$get_dist_fit_weights())
                     #meanlog <- fitted["meanlog"]
                     #sdlog <- fitted["sdlog"]
                     self$dist_fitted_params <- fitted
                   },
                   fit_dist_betapert = function() {
                     # Curiously, I encountered a number of practical issues
                     # with beta-pert fitting in the rriskdistributions package.
                     # To overcome these, I distinguish 2 pert distributions here:
                     # - "pert" that is mapped to the rriskdistributions package
                     #   implementation
                     # - "betapert" that is mapped to my custom implementation
                     #   based on the excellent article "beta-pert distribution"
                     #   that can be found here: https://www.riskamp.com/beta-pert

                    x_min <- self$estim_quantiles[1]
                    x_mode <- self$estim_quantiles[2]
                    x_max <- self$estim_quantiles[3]
                    lambda <- self$dist_fitted_params["lambda"]

                     mu <- ( x_min + x_max + lambda * x_mode ) / ( lambda + 2 );

                     # Approximate the standard deviation
                     # Reference: http://www.rmcapability.com/resources/Capability+Guidance+Sheet+-+Three+point+estimates.pdf
                     # sd <- (x_max - x_min) / ( lambda + 2 )

                     # special case if mu == mode
                     if( mu == x_mode ){
                       v <- ( lambda / 2 ) + 1
                     }
                     else {
                       v <- (( mu - x_min ) * ( 2 * x_mode - x_min - x_max )) /
                         (( x_mode - mu ) * ( x_max - x_min ));
                     }

                     w <- ( v * ( x_max - mu )) / ( mu - x_min );

                     self$dist_fitted_params["mu"] <- mu
                     self$dist_fitted_params["v"] <- v
                     self$dist_fitted_params["w"] <- w
                   },
                   #TODO: The following method looks useless,
                   # do we need to remove it?
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
                    else if(self$dist == "pert") {
                      return(
                        rpert(n=n,
                              min=self$dist_fitted_params["min"],
                              mode=self$dist_fitted_params["mode"],
                              max=self$dist_fitted_params["max"],
                              shape=self$dist_fitted_params["shape"]))
                    }
                    else if(self$dist == "betapert") {
                      #message(cat(self$dist,
                      #            n,
                      #            self$dist_fitted_params["v"],
                      #            self$dist_fitted_params["w"],
                      #            sep="|"))
                      return(
                        rbeta(n=n,
                              shape1=self$dist_fitted_params["v"],
                              shape2=self$dist_fitted_params["w"])
                              #TODO: pass the ncp parameter only if it is not null, otherwise this fails.
                              #ncp=self$dist_fitted_params["ncp"]))
                          # Denormalize to proper scale
                          * (self$estim_quantiles[3] - self$estim_quantiles[1])
                          + self$estim_quantiles[1]
                      )
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
                          mean = self$dist_fitted_params["mean"],
                          sd = self$dist_fitted_params["sd"],
                          log = FALSE ))}
                    else if (self$dist == "betapert"){
                      #message(cat(self$dist,
                      #            x,
                      #            self$dist_fitted_params["v"],
                      #            self$dist_fitted_params["w"],
                      #            sep="|"))

                      # normalize x
                      normalized_x <- (x - self$estim_quantiles[1]) /
                        (self$estim_quantiles[3] - self$estim_quantiles[1])

                      return(
                        dbeta(
                          x = normalized_x,
                          shape1 = self$dist_fitted_params["v"],
                          shape2 = self$dist_fitted_params["w"]
                          #TODO: pass the ncp parameter only if it is not null, otherwise this fails.
                          # ncp = self$dist_fitted_params["ncp"]
                        )
                      )
                    }
                    else {
                      stop("distribution not implemented, sorry")}},
                  get_proba_density_plot = function() {

                    # Define the plot x axis
                    # TODO: Replace this with a function that computes
                    # automatically a more meaningful axis than that,
                    # taking into account situations where the point
                    # estimates are all stuffed on the left or the right
                    # of the distribution.
                    x_start <- min(self$estim_quantiles) * .8
                    x_end <- max(self$estim_quantiles) * 1.2

                    # Prepare the data
                    df <- data.frame(x=c(x_start, x_end)) #self$estim_min - (self$estim_max - self$estim_min) * .2,
                                         #self$estim_max + (self$estim_max - self$estim_min) * .2))

                    # Configure the graph
                    g_density <- ggplot(df, aes(x)) +

                      # Give a little bit of margin on the graph sides
                      xlim(x_start, #self$estim_min - (self$estim_max - self$estim_min) * .2
                           x_end) + #,self$estim_max + (self$estim_max - self$estim_min) * .2) +
                      #ylim: let it scale automatically

                      # Axis titles
                      ylab("Relative likelihood")  +
                      xlab("Factor value")  +

                      # Limit the number of digits on the vertical axis
                      scale_y_continuous(label = function(x) { round(x,3) }) +

                      # Display 3 vertical bars to highlight the 3 points of the estimate
                      # TODO: Re-implement with vectors
                      #geom_vline(xintercept = self$get_dist_fit_quantiles()[1]) +
                      #geom_vline(xintercept = self$get_dist_fit_quantiles()[2]) +
                      #geom_vline(xintercept = self$get_dist_fit_quantiles()[3]) +

                      # Area plot the PDF function with a neutral background
                      stat_function(fun = self$get_proba_density, #prob_density_fun,
                                    colour = "#555555",
                                    geom = "area",
                                    fill = "white",
                                    alpha = 0.5 ) +

                      # Area plot of PDF function within the estimation range with a vivid background
                      stat_function(
                        colour = "#00aa00",
                        fun = self$get_proba_density, #prob_density_fun,
                        geom = 'area',
                        fill = 'green',
                        alpha = 0.1,
                        size = 1.1,
                        xlim = c(x_start, x_end)) + #c(self$get_dist_fit_quantiles()[1],self$get_dist_fit_quantiles()[3])) +

                      # On top of the rest, label the 3 vertical bars
                      # TODO: Re-implement with vectors
                      #annotate(geom = "text", x = self$get_dist_fit_quantiles()[2], y = 0, label = "Typical", angle = 90, hjust = -1, vjust = -.2) +
                      #annotate(geom = "text", x = self$get_dist_fit_quantiles()[1], y = 0, label = "Min", angle = 90, hjust = -1, vjust = -.2) +
                      #annotate(geom = "text", x = self$get_dist_fit_quantiles()[3], y = 0, label = "Max", angle = 90, hjust = -1, vjust = -.2) +

                      # And put a title on top of it
                      ggtitle("Probability density function")

                    return(g_density)
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
                   }
                   #estim_max = function(value) {
                    # if (missing(value)) return(private$private_estim_max)
                    # else {
                    #   stopifnot(is.numeric(value),
                    #             is.null(self$estim_min) | self$estim_min < value,
                    #             is.null(self$typical) | self$typical < value)
                    #   private$private_estim_max <- value
                    # }
                   #},
                   #estim_min = function(value) {
                    # if (missing(value)) return(private$private_estim_min)
                    # else {
                    #   stopifnot(is.numeric(value))
                    #   private$private_estim_min <- value
                    # }
                   #},
                   #estim_typical = function(value) {
                    # if (missing(value)) return(private$private_estim_typical)
                    # else private$private_estim_typical <- value
                   #},
                   #range_max = function(value) {
                    # if (missing(value)) {
                    #   return( 1 - (1 - self$range_size) / 2 )
                    # }
                    # else {
                    #   stop("range_max cannot be set. please set range_size instead.")
                    # }
                   #},
                   #range_min = function(value) {
                    # if (missing(value)) {
                    #   return( (1 - self$range_size) / 2 )
                    # }
                    # else {
                    #   warning("range_min cannot be set. please set range_size instead.")
                    # }
                   #},
                   #range_typical = function(value) {
                    # if (missing(value)) {
                    #   return( .5 )
                    # }
                    # else {
                    #   warning("range_typical cannot be set. please set range_size instead.")
                    # }
                   #},
                   #range_size = function(value) {
                    # if (missing(value)) return(private$private_range_size)
                    # else {
                    #   stopifnot(is.numeric(value),value > 0, value < 1)
                    #   private$private_range_size <- value
                    # }
                   #}
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
