require(ggplot2)

#' overplot_vertical_lines
#'
#' Enriches a graph with good looking vertical lines.
#' For instance, this may be used to represend point estimates and limits in a PDF.
#'
#' @param x_values a vector of estimated quantiles (aka x values)
#'
#' @param x_labels meaningful labels to describe the estimated quantiles
#'
#' @param color A GGPlot color value.
#'
#' @param alpha A GGPlot alpha value.
#'
#' @param plot_addition Complementary plot objets to be added to the new plot object for enrichment purposes.
#'
#' @param line_type GGPlot2 line type. Legal values are the strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash". Alternatively, the numbers 0 to 6 can be used (0 for "blank", 1 for "solid", ...). Moreover, one can also use strings that define the linetype with up to 8 hexadecimal digits (each digit specifying the length of interleaved lines and gaps). Reference: http://sape.inf.usi.ch/quick-reference/ggplot2/linetype.
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
  line_type = NULL,
  plot_addition = NULL,
  verbosity = NULL,
  ...) {

  color <- vp(color, "red", "character", 1)
  alpha <- vp(alpha, .9, "numeric", 1, limit_min = 0, limit_max = 1)
  line_type <- vp(line_type, "solid", "character", 1)
  verbosity <- vp(verbosity, "numeric", 1, limit_min = 0)

  plot_01 <-
    # Enrich the plot with the vertical bars
    geom_vline(
      xintercept = x_values,
      color = color,
      linetype = line_type,
      show.legend = TRUE,
      alpha = alpha,
      size = 1.05) #model_config_get_option("plot", "estimates", "xintercept", "size")) +

  if (!is_void(x_labels)) {
    labels <-
      # Enrich the plot with the vertical bar labels
      annotate(
        color = "darkgrey",
        geom = "text",
        x = x_values,
        y = Inf,
        label = x_labels,
        angle = 90,
        hjust = 1,
        vjust = -.35,
        size = 4)
    plot_01 <- c(plot_01, labels)
  }

  if (!is_void(plot_addition)) {
    plot_01 <- c(plot_01, plot_addition)
  }

  return(plot_01)

  }
