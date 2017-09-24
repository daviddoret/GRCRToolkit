# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(ggplot2)

#' overplot_vertical_lines
#'
#' Enriches a graph with good looking vertical lines.
#' For instance, this may be used to represend point estimates and limits in a PDF.
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
#' graph_enriched <- overplot_vertical_lines(graph=graph, x_values = c(50,90), x_labels = c("a","b"))
#' graph_enriched
#'
overplot_vertical_lines <- function(graph, x_values, x_labels = NULL, color, alpha, ...) {

  if(is.null(color)){ color = model_config_get_option("plot", "estimates", "xintercept", "color") }

  graph <- graph +

    # Enrich the plot with the vertical bars
    geom_vline(
      xintercept = x_values,
      color = color,
      linetype = "solid",
      show_guide = TRUE,
      alpha = alpha,
      size = 1.05) + #model_config_get_option("plot", "estimates", "xintercept", "size")) +

  if(!is.null(x_labels)) {
    graph <- graph +

      # Enrich the plot with the vertical bar labels
      annotate(
        color = "darkgrey",
        geom = "text",
        x = x_values,
        y = 0,
        label = x_labels,
        angle = 90,
        hjust = .5,
        vjust = 0,
        size = 6)

  }

  return(graph)

  }
