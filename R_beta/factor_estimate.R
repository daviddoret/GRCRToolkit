if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,ggplot2)

#' The factor_estimate class provides an inheritance model
#' for effective factor estimation approaches.
#' This class should be inherited from but not instanciated from (ie it is abstract).
#'
#' TODO:
#' - Glue code with model_factor.
#' - Implement SAMPLE var, sd, median, etc.
#'   Re-implement mode from the SAMPLE this this in summary statistics.
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
    get_print_lines = function(...) {
      return
        c(
          paste0("Estimation method: ", self$estimation_method_name),
          paste0("Fitted distribution: ", self$distribution_name),
          "Simulation sample:",
          paste0(
            " mode = ", fn(self$dist_mode,4),
            " μ = ", fn(self$dist_mean,4),
            " ,σ = ", fn(self$dist_sd,4)),
          paste0(
            " ,σ² = ", fn(self$dist_variance,4),
            " ,γ1 = ", fn(self$dist_skewness,4),
            " ,κ = ", fn(self$dist_kurtosis,4))
          )
    },
    print = function(...) {
      cat(paste0(self$get_print_lines(), collapse = "\n"))
      invisible(self)
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
    graph_quantile = function(x_start = NULL, x_end = NULL)
      {
      if(is.null(x_start)) { x_start <- 0 }
      if(is.null(x_end)) { x_end <- 1 }
      return(
        plot_quantile_function(
          fun = self$quantile_function,
          x_start = x_start,
          x_end = x_end)) },
    graph_sample_without_outliers = function(x_start = NULL, x_end = NULL, title = NULL)
    {
      sample <- self$get_random(1000)
      if(is.null(x_start)) { x_start <- self$graph_value_start }
      if(is.null(x_end)) { x_end <- self$graph_value_end }
      return(
        plot_sample(
          sample = sample,
          title = "Sample histogram without outliers",
          x_start = x_start,
          x_end = x_end)) },
    graph_sample_with_outliers = function(title = NULL)
    {
      sample <- self$get_random(1000)
      return(
        plot_sample(
          sample = sample,
          title = "Sample histogram with outliers")) },
    plot_vignette = function(...) {
      # Plots a textual summary description of this factor.
      return(plot_vignette(title="Summary", text=self$get_print_lines()))
    },
    graph_all = function(x_start = NULL, x_end = NULL) {
      if(is.null(x_start)) { x_start <- self$graph_value_start }
      if(is.null(x_end)) { x_end <- self$graph_value_end }

      return(multiplot(
        self$plot_vignette(),
        self$graph_density(x_start = x_start, x_end = x_end),
        self$graph_probability(x_start = x_start, x_end = x_end),
        self$graph_quantile(),
        self$graph_sample_with_outliers(),
        self$graph_sample_without_outliers(),
        layout = matrix(c(1,2,3,4,5,6), nrow=2, byrow=TRUE)))
    },
    simulate = function(n = NULL) {
      # The simulate method may be overridden by a subclass.
      # This may be required to populate richer data frames
      # with complementary columns. I was thinking of this
      # approach to implement the frequency x impact factor
      # where the frequency factor generates a vector of
      # frequencies and where the impact factor will need to
      # call (frequecy number) times the random function
      # and sum the result. In this situation it is desirable
      # to keep the individual impacts in an "individual impacts"
      # column in the data frame and use the standard factor_value column
      # for the final factor results.
      factor_value <- self$get_random(n = n)
      private$private_simulation_sample <- data.frame(factor_value = factor_value)
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
    # if analytical solutions are available.
    # At this level, we may only rely on optimization to
    # estimate solutions.
    dist_mode = function(value,...) {
      if(missing(value)) { return(get_dist_mode_from_pdf(pdf = self$get_density)) }
      else { stop("This is a read-only attribute") }},
    dist_mean = function(value,...) {
      if(missing(value)) {
        return(mean(x = self$get_random(n = 1000)))
      }
      else { stop("This is a read-only attribute") }},
    dist_sd = function(value,...) {
      if(missing(value)) {
        return(sd(x = self$get_random(n = 1000)))
      }
      else { stop("This is a read-only attribute") }},
    dist_variance = function(value, ...) {
      if(missing(value)) {
        return(var(x = self$get_random(n = 1000)))
      }
      else { stop("This is a read-only attribute") }},
    dist_skewness = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    dist_kurtosis = function(value,...) {
      if(missing(value)) { return(NA) }
      else { stop("This is an abstract attribute, it must be implemented by a subclass") }},
    simulation_sample = function(value,...) {
      # Returns a data frame with the simulation sample data.
      # The data frame mandatorily contains a column "factor_value" with
      # the resulting factor values.
      # The data frame may contain other columns with complementary information.
      if(missing(value)) { return(private$private_simulation_sample) }
      else { private$private_simulation_sample <- value }}
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
    private_simulation_sample = NA
  )
)
