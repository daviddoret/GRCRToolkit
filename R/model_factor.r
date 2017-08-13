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
                   estim_3_points_poissonpert = function(estim_min = NULL,
                                                      estim_typical = NULL,
                                                      estim_max = NULL,
                                                      range_size = NULL,
                                                      lambda = NULL,
                                                      ...) {
                     # This is a helper function to facilitate
                     # 3 points simplistic expert estimates
                     # fitted to the poisson-PERT distribution.

                     if(is.null(range_size)) {
                       range_size = model_config_get_option("dist_fit", "3_points_poissonpert", "range_size")
                     }

                     self$dist <- "poissonpert"

                     estim_quantiles <- c(estim_min, estim_typical, estim_max)
                     estim_probas <- c((1 - range_size) / 2,
                                       .5,
                                       1 - (1 - range_size) / 2)
                     estim_weights <- c(1, 1, 1)
                     estim_labels <- c("Min.", "Typical", "Max.")

                     # Stores the vectors locally
                     self$estim_probas <- estim_probas
                     self$estim_quantiles <- estim_quantiles
                     self$estim_labels <- estim_labels
                     self$estim_weights <- estim_weights

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
                       range_size = model_config_get_option("dist_fit", "3_points_betapert", "range_size")
                     }

                     if(is.null(lambda)) {
                       lambda = model_config_get_option("dist_fit", "3_points_betapert", "lambda")
                     }

                     self$dist <- "betapert"
                     self$dist_fitted_params["lambda"] <- lambda

                     estim_quantiles <- c(estim_min, estim_typical, estim_max)
                     estim_probas <- c((1 - range_size) / 2,
                                       .5,
                                       1 - (1 - range_size) / 2)
                     estim_weights <- c(1, 1, 1)
                     estim_labels <- c("Min.", "Typical", "Max.")

                     # Stores the vectors locally
                     self$estim_probas <- estim_probas
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
                     else if(self$dist == "poissonpert") {
                       # When named "betapert", we use our custom
                       # fitting implementation. See the function
                       # for more details.
                       self$fit_dist_poissonpert()
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
                   fit_dist_poissonpert = function() {
                     fitted <- get.poisson.par(
                       p = self$estim_probas,
                       q = self$estim_quantiles,
                       show.output = TRUE,
                       plot = FALSE,
                       tol = model_config_get_option("dist_fit", "tol"),
                       fit.weights = self$estim_weights)
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
                    else if(self$dist == "poissonpert") {
                      return(
                        rpert(n=n,
                              lambda=self$dist_fitted_params["lambda"]))
                    }
                    else if(self$dist == "betapert") {
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
                  cumulative_distribution_function = function(x) {
                    if( self$dist == "lnorm" ) {
                      return(
                        plnorm(
                          q = x,
                          meanlog = self$dist_fitted_params["meanlog"],
                          sdlog = self$dist_fitted_params["sdlog"],
                          log.p = FALSE ))}
                    else if( self$dist == "norm" ){
                      return(
                        pnorm(
                          q = x,
                          mean = self$dist_fitted_params["mean"],
                          sd = self$dist_fitted_params["sd"],
                          log.p = FALSE ))}
                    else if (self$dist == "betapert"){
                      # re-normalize x
                      normalized_q <- (x - self$estim_quantiles[1]) /
                        (self$estim_quantiles[3] - self$estim_quantiles[1])

                      return(
                        pbeta(
                          q = normalized_q,
                          shape1 = self$dist_fitted_params["v"],
                          shape2 = self$dist_fitted_params["w"]
                          #TODO: pass the ncp parameter only if it is not null, otherwise this fails.
                          # ncp = self$dist_fitted_params["ncp"]
                        )
                      )
                    }
                    else {
                      stop("distribution not implemented, sorry")}
                  },
                  probability_density_function = function(x) {
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
                      # re-normalize x
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
                  probability_density_function_plot = function() {

                    # Define the plot x axis
                    # TODO: Replace this with a function that computes
                    # automatically a more meaningful axis than that,
                    # taking into account situations where the point
                    # estimates are all stuffed on the left or the right
                    # of the distribution.
                    x_start <- min(self$estim_quantiles) * .8
                    x_end <- max(self$estim_quantiles) * 1.2

                    # Prepare the data
                    df <- data.frame(x=c(x_start, x_end))

                    # Configure the graph
                    g_density <- ggplot(df, aes(x)) +

                    # Give a little bit of margin on the graph sides
                    xlim(x_start, x_end) +
                    #ylim: let it scale automatically

                    # Axis titles
                    ylab("Relative likelihood")  +
                    xlab("Factor value")  +

                    # Limit the number of digits on the vertical axis
                    scale_y_continuous(label = function(x) { round(x,3) }) +

                    # Display 3 vertical bars to highlight the 3 points of the estimate
                    geom_vline(xintercept = self$estim_quantiles,
                               color = model_config_get_option("plot", "pdf", "estim_interecept", "color"),
                               size = model_config_get_option("plot", "pdf", "estim_interecept", "size")) +

                    # Area plot the function
                    stat_function(
                      colour = model_config_get_option("plot", "pdf", "area", "color"),
                      fun = self$probability_density_function,
                      geom = 'area',
                      fill = model_config_get_option("plot", "pdf", "area", "fill"),
                      alpha = model_config_get_option("plot", "pdf", "area", "alpha"),
                      size = model_config_get_option("plot", "pdf", "area", "size"),
                      xlim = c(x_start, x_end)) +

                    # On top of the rest, label the vertical bars
                      # TODO: Show transparently the difference
                      # between the original estimate and the
                      # corresponding quantile in the distribution.
                      # Depending on the shape of the selected
                      # distribution and the estimates, the
                      # difference may be very important, e.g.
                      # if estimates are skewed to the right
                      # and the distribution is log normal.
                    annotate(geom = "text",
                             x = self$estim_quantiles,
                             y = 0,
                             label = self$estim_labels,
                             angle = 90,
                             hjust = -1,
                             vjust = -.2) +

                    # And put a title on top of it
                    ggtitle("Probability density function")

                    return(g_density)
                  },
                  cumulative_distribution_function_plot = function() {

                    # Define the plot x axis
                    # TODO: Replace this with a function that computes
                    # automatically a more meaningful axis than that,
                    # taking into account situations where the point
                    # estimates are all stuffed on the left or the right
                    # of the distribution.
                    x_start <- min(self$estim_quantiles) * .8
                    x_end <- max(self$estim_quantiles) * 1.2

                    # Prepare the data
                    df <- data.frame(x=c(x_start, x_end))

                    # Configure the graph
                    g1 <- ggplot(df, aes(x)) +

                    # Give a little bit of margin on the graph sides
                    xlim(x_start, x_end) +
                    #ylim: let it scale automatically

                    # Axis titles
                    ylab("Probability")  +
                    xlab("Factor value")  +

                    # Limit the number of digits on the vertical axis
                    scale_y_continuous(label = function(x) { round(x,3) }) +

                    # Display 3 vertical bars to highlight the 3 points of the estimate
                    geom_vline(xintercept = self$estim_quantiles,
                               color = model_config_get_option("plot", "cdf", "estim_interecept", "color"),
                               size = model_config_get_option("plot", "cdf", "estim_interecept", "size")) +

                    # Area plot the function
                    stat_function(
                      colour = model_config_get_option("plot", "cdf", "area", "color"),
                      fun = self$cumulative_distribution_function,
                      geom = 'area',
                      fill = model_config_get_option("plot", "cdf", "area", "fill"),
                      alpha = model_config_get_option("plot", "cdf", "area", "alpha"),
                      size = model_config_get_option("plot", "cdf", "area", "size"),
                      xlim = c(x_start, x_end)) +

                    # On top of the rest, label the vertical bars
                      # TODO: Show transparently the difference
                      # between the original estimate and the
                      # corresponding quantile in the distribution.
                      # Depending on the shape of the selected
                      # distribution and the estimates, the
                      # difference may be very important, e.g.
                      # if estimates are skewed to the right
                      # and the distribution is log normal.
                    annotate(geom = "text",
                             x = self$estim_quantiles,
                             y = 0,
                             label = self$estim_labels,
                             angle = 90,
                             hjust = -1,
                             vjust = -.2) +

                    # And put a title on top of it
                    ggtitle("Cumulative distribution function")

                    return(g1)
                  }
                  ,plot = function() {
                    g1 <- self$probability_density_function_plot()
                    g2 <- self$cumulative_distribution_function_plot()
                    multiplot(g1,
                              g2)
                              #cols=2)
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
                   #private_estim_min = NULL,
                   #private_estim_typical = NULL,
                   #private_estim_max = NULL,
                   #private_range_size = NULL
                 )
)
