if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6)

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
      return(paste0("Estimation method: ", self$estimation_method_name,
                    "\nDistribution fit: ", self$distribution_name))},
    get_density = function(x, ...) { return(self$density_function(x, ...)) },
    get_probability = function(q, ...) { return(self$probability_function(q, ...)) },
    get_quantile = function(p, ...) { return(self$quantile_function(p, ...)) },
    get_random = function(n, ...) { return(self$random_function(n, ...)) },
    graph_density = function(x_start, x_end) { return(plot_probability_density_function(fun = self$density_function, x_start = x_start, x_end = x_end)) },
    graph_probability = function(x_start, x_end) { return(plot_cumulative_distribution_function(fun = self$probability_function, x_start = x_start, x_end = x_end)) },
    graph_quantile = function(x_start = NULL, x_end = NULL) { return(plot_quantile_function(fun = self$quantile_function, x_start = x_start, x_end = x_end)) },
    graph_all = function(x_start, x_end) {

      vignette <- ggplot() +
        theme_void() +
        theme(plot.margin = unit(c(1, 1, .5, .5), "cm")) +
        theme(plot.title = element_text(size = 16, face = "bold")) +
        labs(
          title = "Factor Estimate",
          subtitle = self$get_print()) #,
          #caption = "Time, etc. informations")

      return(multiplot(
        vignette,
        self$graph_density(x_start = x_start, x_end = x_end),
        self$graph_probability(x_start = x_start, x_end = x_end),
        self$graph_quantile(),
        layout = matrix(c(1,1,1,2,3,4,2,3,4,2,3,4,2,3,4), nrow=5, byrow=TRUE)))
        #cols = 3))
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
      else { private$private_random_function <- value }}
  ),
  private = list(
    private_estimation_method_name = NULL,
    private_distribution_name = NULL,
    private_density_function = NULL,
    private_probability_function = NULL,
    private_quantile_function = NULL,
    private_random_function = NULL
  )
)
