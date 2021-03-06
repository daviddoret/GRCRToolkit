require(colorspace)
require(ggplot2)
require(labeling)

#' plot_cumulative_distribution_function
#'
#' Produces a good looking graph of a cumulative distribution function.
#'
#' @param fun a pre-parameterized cumulative distribution function receiving a single x parameter
#'
#' @param x_start the left most position that will be displayed on the x axis
#'
#' @param x_end the right most position that will be displayed on the x axis
#'
#' @return a good looking graph
#'
#' @examples
#' fun <- function(x, ...) { return(dlnorm(x = x, meanlog = 100, sdlog = 20, log = FALSE))  }
#' graph <- plot_cumulative_distribution_function(fun = fun, x_start = 10, x_end = 150)
#' graph
#'
#' @export
plot_cumulative_distribution_function <- function(
  fun,
  x_start,
  x_end,
  title = NULL,
  subtitle = NULL,
  caption = NULL, ...) {

  # Default title
  if (is_void(title)){ title <- "Cumulative Distribution Function" }

  # Prepare the plot data frame
  df <- data.frame(x = c(x_start, x_end))

  # Configure the graph
  graph <- ggplot(df, aes(x)) +

    # Give a little bit of margin on the graph sides
    xlim(x_start, x_end) +
    #ylim: let it scale automatically

    # Limit the number of digits on the vertical axis
    scale_y_continuous(label = function(x) { round(x,3) }) +

    # Area plot the function
    stat_function(
      colour = model_config_get_option("plot", "cdf", "area", "color"),
      fun = fun,
      geom = 'line',
      size = model_config_get_option("plot", "cdf", "area", "size"),
      xlim = c(x_start, x_end)) +

    # Add title, substitle, caption and scale titles
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Factor value",
      y = "Cumulative probability") +

    # Apply some general theme to make it good looking
    theme(plot.title = element_text(size = 12, face = "bold"))

  return(graph)
}
