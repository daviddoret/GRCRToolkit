if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,ggplot)

#' The factor_estimate class provides an inheritance model
#' for effective factor estimation approaches.
#' This class should be inherited from but not instanciated from (ie it is abstract).
#'
#' TODO:
#' - Graph output
#' - Clean print output
#' - Glue code with model_factor
#'
#' @export
factor_estimate <- R6Class(
  "factor_estimate",
  public = list(
    initialize = function(
      estimation_method_name = NULL,
      distribution_name = NULL, ...) {
      self$estimation_method_name <- estimation_method_name
      self$distribution_name <- distribution_name
      },
    print = function(...) {
      print(self$get_print())
      invisible(self)
      },
    get_print = function(...) {
      return(
        c(
          paste0("Estimation method: ", self$estimation_method_name),
          paste0("Distribution fit: ", self$distribution_name),
          "Moments of the fitted distribution:",
          paste0(
            " μ = ", fn(self$dist_mean,4),
            " ,σ = ", fn(self$dist_sd,4),
            " ,σ² = ", fn(self$dist_variance,4),
            " ,γ1 = ", fn(self$dist_skewness,4),
            " ,κ = ", fn(self$dist_kurtosis,4))
          ))
      },
    get_density = function(x, ...) { return(self$density_function(x, ...)) },
    get_probability = function(q, ...) { return(self$probability_function(q, ...)) },
    get_quantile = function(p, ...) { return(self$quantile_function(p, ...)) },
    get_random = function(n, ...) { return(self$random_function(n, ...)) },
    graph_density = function(x_start = NULL, x_end = NULL)
      {
      if(is.null(x_start)) { x_start <- self$graph_value_start }
      if(is.null(x_end)) { x_end <- self$graph_value_end }
      return(
        plot_probability_density_function(
          fun = self$density_function,
          x_start = x_start,
          x_end = x_end)) },
    graph_probability = function(x_start = NULL, x_end = NULL)
      {
      if(is.null(x_start)) { x_start <- self$graph_value_start }
      if(is.null(x_end)) { x_end <- self$graph_value_end }
      return(
        plot_cumulative_distribution_function(
          fun = self$probability_function,
          x_start = x_start,
          x_end = x_end)) },
    graph_quantile = function(y_start = NULL, y_end = NULL)
      {
      if(is.null(y_start)) { y_start <- self$graph_probability_start }
      if(is.null(y_end)) { x_end <- self$graph_probability_end }
      return(
        plot_quantile_function(
          fun = self$quantile_function,
          y_start = y_start,
          y_end = y_end)) },
    graph_all = function(x_start = NULL, x_end = NULL) {
      if(is.null(x_start)) { x_start <- self$graph_value_start }
      if(is.null(x_end)) { x_end <- self$graph_value_end }

      vignette <- ggplot() +
        theme_void() +
        theme(plot.margin = unit(c(1, 1, .5, .5), "cm")) +
        theme(plot.title = element_text(size = 16, face = "bold")) +
        labs(
          title = "Factor Estimate") #, caption = self$get_print())
          #subtitle =

      return(multiplot(
        vignette,
        self$graph_density(x_start = x_start, x_end = x_end),
        self$graph_probability(x_start = x_start, x_end = x_end),
        self$graph_quantile(),
        layout = matrix(c(1,2,3,4), nrow=4, byrow=TRUE)))
        #layout = matrix(c(1,1,1,2,3,4,2,3,4,2,3,4,2,3,4), nrow=5, byrow=TRUE)))
        #cols = 3))
    },
    simulate = function(n = NULL) {
        stop("This is an abstract method, it should be implemented and overriden by the subclass")
    }
  ),
  active = list(
    estimation_method_name = function(value,...) {
      if(missing(value)) { return(private$private_estimation_method_name) }
      else { private$private_estimation_method_name <- value }},
    distribution_name = function(value,...) {
      if(missing(value)) { return(private$private_distribution_name) }
      else { private$private_distribution_name <- value }},
    density_function = function(value,...) {
      if(missing(value)) { return(private$private_density_function) }
      else { private$private_density_function <- value }},
    probability_function = function(value,...) {
      if(missing(value)) { return(private$private_probability_function) }
      else { private$private_probability_function <- value }},
    quantile_function = function(value,...) {
      if(missing(value)) { return(private$private_quantile_function) }
      else { private$private_quantile_function <- value }},
    random_function = function(value,...) {
      if(missing(value)) { return(private$private_random_function) }
      else { private$private_random_function <- value }},
    # Beautiful graph preferences
    graph_value_start = function(value,...) {
      if(missing(value)) { return(private$private_graph_value_start) }
      else { private$private_graph_value_start <- value }},
    graph_value_end = function(value,...) {
      if(missing(value)) { return(private$private_graph_value_end) }
      else { private$private_graph_value_end <- value }},
    graph_probability_start = function(value,...) {
      if(missing(value)) { return(private$private_graph_probability_start) }
      else { private$private_graph_probability_start <- value }},
    graph_probability_end = function(value,...) {
      if(missing(value)) { return(private$private_graph_probability_end) }
      else { private$private_graph_probability_end <- value }},
    # Standard moments of the fitted distribution
    # These are conditionnaly implemented by the subclasses
    dist_mean = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    dist_sd = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    dist_variance = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    dist_skewness = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    dist_kurtosis = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    simulation_data = function(value,...) {
      if(missing(value)) { return(private$private_simulation_data) }
      else { private$private_simulation_data <- value }}
  ),
  private = list(
    private_estimation_method_name = NA,
    private_distribution_name = NA,
    private_density_function = NA,
    private_probability_function = NA,
    private_quantile_function = NA,
    private_random_function = NA,
    # Limits for good-looking graph rendering.
    # Sub-classes implementing estimation methods
    # have the responsibility to set their values.
    private_graph_value_start = NA,
    private_graph_value_end = NA,
    private_graph_probability_start = NA,
    private_graph_probability_end = NA,
    private_simulation_data = NA
  )
)
