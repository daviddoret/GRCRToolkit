if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

options(digits=22)

#' GLD 3 points estimate
#'
#' @export
factor_estimate_gld_3points <- R6Class(
  "factor_estimate_gld_3points",
  inherit = factor_estimate_gld,
  public = list(
    initialize = function(...) {
      super$initialize(
        estimation_method_name = "PERT-like 3 points estimate", ...)
    },
    fit_distribution = function(...) {
      self$optimize_lambda1()

    },
    optimize_lambda1 = function(...) {
      # this parameter is obvious
      self$lambda1 <- self$mode_value
    },
    optimize_lambda3 = function() {

      # pgl does not support vectors in the lambda3 parameter,
      # (which I must say is perfectly reasonable).
      # So I declare a flat scalar version and take this opportunity
      # to throw away positive values (nlm optimizer doesn't support bounds either).
      flat_function <- function(x){
        if(x >= 0) { return(Inf)  }
        return(pgl(
          q = self$min_value,
          lambda1 = self$lambda1,
          lambda2 = self$lambda2,
          lambda3 = x,
          lambda4 = self$lambda4))
      }

      # Then I declare a minimization function
      # that will vectorize the call to flat_function with vapply.
      minimization_function <- function(x, ...){
        return(
          abs(
            vapply(x, flat_function, 0)
            -
              fg3$min_proba
          )
          # nlm prefers to reduce high numbers
          # so I artificially increase the output
          # of my minimization function by a factor
          # that guarantees precise enough results.
          * 1000000
        )
      }

      # Then we run the optimization.
      # TODO: Study nlm options in greater details.
      optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128)

      # TODO: We should test the result against a tolerance threshold.
      #fg3$get_probability(fg3$max_value)
      #fg3$get_quantile(fg3$max_proba)

      # And we retrieve its output.
      fg3$lambda3 <- optimization$estimate

    },
    optimize_lambda4 = function() {

      # pgl does not support vectors in the lambda4 parameter,
      # (which I must say is perfectly reasonable).
      # So I declare a flat scalar version and take this opportunity
      # to throw away positive values (nlm optimizer doesn't support bounds either).
      flat_function <- function(x){
        if(x >= 0) { return(Inf)  }
        return(pgl(
          q = self$max_value,
          lambda1 = self$lambda1,
          lambda2 = self$lambda2,
          lambda3 = self$lambda3,
          lambda4 = x))
      }

      # Then I declare a minimization function
      # that will vectorize the call to flat_function with vapply.
      minimization_function <- function(x, ...){
        return(
          abs(
            vapply(x, flat_function, 0)
            -
              fg3$max_proba
          )
          # nlm prefers to reduce high numbers
          # so I artificially increase the output
          # of my minimization function by a factor
          # that guarantees precise enough results.
          * 1000000
        )
      }

      # Then we run the optimization.
      # TODO: Study nlm options in greater details.
      optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128)

      # TODO: We should test the result against a tolerance threshold.
      #fg3$get_probability(fg3$max_value)
      #fg3$get_quantile(fg3$max_proba)

      # And we retrieve its output.
      fg3$lambda4 <- optimization$estimate

    },
    get_print = function(...) {
      return(paste0(super$get_print(),
                    "\nEstimation parameters:",
                    "\n\tmin = ", self$min_value, " (", self$min_proba, ")",
                    " ,mode = ", self$mode_value, " (", self$mode_proba, ")",
                    " ,max = ", self$max_value, " (", self$max_proba, ")",
                    "\nFitted quantiles:",
                    "\n\tmin = ", round(self$get_quantile(self$min_proba),2), " (", self$min_proba, ")",
                    " ,mode = ", round(self$get_quantile(self$mode_proba),2), " (", self$mode_proba, ")",
                    " ,max = ", round(self$get_quantile(self$max_proba),2), " (", self$max_proba, ")",
                    "\nFitted probabilities:",
                    "\n\tmin = ", self$min_value, " (", round(self$get_probability(self$min_value),2), ")",
                    " ,mode = ", self$mode_value, " (", round(self$get_probability(self$mode_value),2), ")",
                    " ,max = ", self$max_value, " (", round(self$get_probability(self$max_value),2), ")"
                    ))},
    reset_graph_limits = function() {
      # Set default scale margins containing all estimation parameters for pretty graph rendering.
      self$graph_value_start <- self$min_value #- (self$max_value - self$min_value) * .05
      self$graph_value_end <- self$max_value #+ (self$max_value - self$min_value) * .05
      self$graph_probability_start <- self$min_proba / 4
      self$graph_probability_end <- self$max_proba + (1 - self$max_proba) / 4
    }

  ),
  active = list(
    min_value = function(value,...) {
      if(missing(value)) { return(private$private_min_value) }
      else {
        private$private_min_value <- value
        self$reset_graph_limits() }},
    mode_value = function(value,...) {
      if(missing(value)) { return(private$private_mode_value) }
      else {
        private$private_mode_value <- value
        self$reset_graph_limits() }},
    max_value = function(value,...) {
      if(missing(value)) { return(private$private_max_value) }
      else {
        private$private_max_value <- value
        self$reset_graph_limits() }},
    min_proba = function(value,...) {
      if(missing(value)) { return(private$private_min_proba) }
      else {
        private$private_min_proba <- value
        self$reset_graph_limits() }},
    mode_proba = function(value,...) {
      if(missing(value)) { return(private$private_mode_proba) }
      else {
        private$private_mode_proba <- value
        self$reset_graph_limits() }},
    max_proba = function(value,...) {
      if(missing(value)) { return(private$private_max_proba) }
      else {
        private$private_max_proba <- value
        self$reset_graph_limits() }},
    range_size_proba = function(value,...) {
      if(missing(value)) { return(self$max_proba - self$min_proba) }
      else {
        self$min_proba <- (1 - value) / 2
        self$max_proba <- 1 - (1 - value) / 2
        self$reset_graph_limits() }}
  ),
  private = list(
    private_min_value = NULL,
    private_mode_value = NULL,
    private_max_value = NULL,
    private_min_proba = NULL,
    private_mode_proba = NULL,
    private_max_proba = NULL
  )
)
