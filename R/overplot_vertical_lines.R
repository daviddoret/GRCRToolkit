require(ggplot2)

#' overplot_vertical_lines
#'
#' Enriches a graph with good looking vertical lines.
#' For instance, this may be used to represend point estimates and limits in a PDF.
#'
#' @param estim_quantiles a vector of estimated quantiles (aka x values)
#'
#' @param estim_labels meaningful labels to describe the estimated quantiles
#'
#' @param color A GGPlot color value.
#'
#' @param alpha A GGPlot alpha value.
#'
#' @param plot_addition Complementary plot objets to be added to the new plot object for enrichment purposes.
#'
#' @param verbosity 0: no messages. > 0 more and more verbose messages.
#'
#' @return A good looking geom object that may be used to enrich plots.
#'
#' @examples
#' fun <- function(x, ...) { return(dlnorm(x = x, meanlog = 100, sdlog = 20, log = FALSE))  }
#' graph <- plot_probability_density_function(fun = fun, x_start = 10, x_end = 150)
#' graph_enriched <- overplot_vertical_lines(graph=graph, x_values = c(50,90), x_labels = c("a","b"))
#' graph_enriched
#'
#' @export
overplot_vertical_lines <- function(
  x_values,
  x_labels = NULL,
  color = NULL,
  alpha = NULL,
  plot_addition = NULL,
  verbosity = NULL,
  ...) {

  if (is_void(color)) { color <- "red" }
  if (is_void(alpha)) { alpha <- .75 }
  if (is_void(verbosity)) { verbosity <- 0 }

  plot_01 <-
    # Enrich the plot with the vertical bars
    geom_vline(
      xintercept = x_values,
      color = color,
      linetype = "solid",
      show.legend = TRUE,
      alpha = alpha,
      size = 1.05) + #model_config_get_option("plot", "estimates", "xintercept", "size")) +

  if (!is_void(x_labels)) {
    plot_01 <- plot_01 +
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

  if (!is_void(plot_addition)) {
    plot_01 <- plot_01 + plot_addition
  }

  return(plot_01)

  }
