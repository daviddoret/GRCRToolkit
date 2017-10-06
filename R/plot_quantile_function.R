# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(colorspace, ggplot2, labeling)


# FUTURE ENHANCEMENT:
# - Add a nice cat and whisker plot
#   showing the key moments,
#   get inspiration from here:
#   https://en.wikipedia.org/wiki/Probability_density_function

#' plot_quantile_function
#'
#' Produces a good looking graph of a quantile function.
#'
#' @param fun a parameterized quantile function receiving a single x parameter
#'
#' @param x_start the left most position that will be displayed on the x axis
#'
#' @param x_end the right most position that will be displayed on the x axis
#'
#' @return a good looking graph
#'
#' @examples
#' fun <- function(x, ...) { return(dlnorm(x = x, meanlog = 100, sdlog = 20, log = FALSE))  }
#' graph <- plot_quantile_function(fun = fun, x_start = 10, x_end = 150)
#' graph
#'
#' @export
plot_quantile_function = function(
  fun,
  x_start,
  x_end,
  title = NULL,
  subtitle = NULL,
  caption = NULL, ...) {

  # And put a title on top of it
  if(is.null(title)){ title <- "Probability Quantile Function" }

  # Prepare the data
  df <- data.frame(x=c(x_start, x_end))

  # Configure the graph
  graph <- ggplot(df, aes(x)) +

    # Give a little bit of margin on the graph sides
    xlim(x_start, x_end) +

    # Limit the number of digits on the vertical axis
    scale_y_continuous(label = function(x) { round(x,3) }) +

    # coord_trans(x="log2") +

    # Area plot the function
    stat_function(
      colour = model_config_get_option("plot", "qdf", "area", "color"),
      fun = fun,
      geom = 'line',
      #fill = model_config_get_option("plot", "qdf", "area", "fill"),
      #alpha = model_config_get_option("plot", "qdf", "area", "alpha"),
      size = model_config_get_option("plot", "qdf", "area", "size"),
      xlim = c(x_start, x_end)) +

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Probability",
      y = "Factor value") +
    #ggtitle(title)

    theme(plot.title = element_text(size = 12, face = "bold"))

  return(graph)
}
