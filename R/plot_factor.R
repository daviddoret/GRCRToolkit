if (!require(pacman)) install.packages(pacman)
pacman::p_load(colorspace, ggplot2, labeling)

#' plot_factor
#'
#' Produces a good looking multi-graph visual representation of a model factor.
#'
#' @param factor an instance of the model_factor R6 class
#'
#' @return an good looking graph enriched with visual representations for the estimates
#'
#' @examples
#'f1 <- model_factor$new(name = "impact sample")
#'f1$estim_3_points_betapert(
#'  estim_min = 200,
#'  estim_typical = 1300,
#'  estim_max = 1500,
#'  range_size = .9)
#'plot_factor(f1)
#'

plot_factor <- function(factor, ...) {

  # Define the plot x axis
  # TODO: Replace this with a function that computes
  # automatically a more meaningful axis than that,
  # taking into account situations where the point
  # estimates are all stuffed on the left or the right
  # of the distribution.
  x_start <- min(factor$estim_quantiles) * .8
  x_end <- max(factor$estim_quantiles) * 1.2

  g1 <- NULL
  g2 <- overplot_estimates(
    graph = plot_cumulative_distribution_function(
      fun = factor$cumulative_distribution_function,
      x_start = x_start,
      x_end = x_end),
    estim_quantiles = factor$estim_quantiles,
    estim_labels = factor$estim_labels)

  if(get_dist_type(factor$dist) == "discrete")
  {
    g1 <- overplot_estimates(
        graph = plot_probability_mass_function(
          fun = factor$probability_mass_function,
          x_start = x_start,
          x_end = x_end),
        estim_quantiles = factor$estim_quantiles,
        estim_labels = factor$estim_labels)
  }
  else if(get_dist_type(factor$dist) == "continuous")
  {
    g1 <- overplot_estimates(
      graph = plot_probability_density_function(
        fun = factor$probability_density_function,
        x_start = x_start,
        x_end = x_end),
      estim_quantiles = factor$estim_quantiles,
      estim_labels = factor$estim_labels)
  }
  multiplot(g1,
            g2)
  #cols=2)
}
