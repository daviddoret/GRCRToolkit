# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(ggplot2)

#' overplot_estimates
#'
#' Enriches a graph with good looking vertical interects representing estimates.
#'
#' @param graph a ggplot graph, typically a PDF or CDF
#'
#' @param estim_quantiles a vector of estimated quantiles (aka x values)
#'
#' @param estim_labels meaningful labels to describe the estimated quantiles
#'
#' @return an good looking graph enriched with visual representations for the estimates
#'
#' @examples
#' fun <- function(x, ...) { return(dlnorm(x = x, meanlog = 100, sdlog = 20, log = FALSE))  }
#' graph <- plot_probability_density_function(fun = fun, x_start = 10, x_end = 150)
#' graph_enriched <- overplot_estimates(graph=graph, estim_quantiles = c(50,90), estim_labels = c("a","b"))
#' graph_enriched
#'
overplot_estimates <- function(graph, estim_quantiles, estim_labels, color, ...) {
  if(is.null(color)){ color = model_config_get_option("plot", "estimates", "xintercept", "color") }
  return(
    graph +
    # Display 3 vertical bars to highlight the 3 points of the estimate
    geom_vline(
      xintercept = estim_quantiles,
      color = color,
      size = model_config_get_option("plot", "estimates", "xintercept", "size")) +

    # On top of the rest, label the vertical bars
    # TODO: Show transparently the difference
    # between the original estimate and the
    # corresponding quantile in the distribution.
    # Depending on the shape of the selected
    # distribution and the estimates, the
    # difference may be very important, e.g.
    # if estimates are skewed to the right
    # and the distribution is log normal.
    annotate(
      geom = "text",
      x = estim_quantiles,
      y = 0,
      label = estim_labels,
      angle = 90,
      hjust = -1,
      vjust = -.2))}
