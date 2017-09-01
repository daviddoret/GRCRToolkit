# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(ggplot2,labeling)

# Package tweaking to get github version of ggplot2,
# in such a way as to benefit from the new labs function with subtitles.
#install.packages("devtools")
#pacman::p_load(devtools,colorspace,assertthat)
#install.packages(c("RColorBrewer", "stringr", "dichromat", "munsell", "plyr", "colorspace"))
#install.packages("scales")
#dev_mode(TRUE)
#install_github("hadley/scales")
#install_github("tidyverse/ggplot2")


# FUTURE ENHANCEMENT:
# - Add a nice cat and whisker plot
#   showing the key moments,
#   get inspiration from here:
#   https://en.wikipedia.org/wiki/Probability_density_function

#' plot_probability_density_function
#'
#' Produces a good looking graph of a probability density function.
#'
#' @param fun a parameterized probability density function receiving a single x parameter
#'
#' @param x_start the left most position that will be displayed on the x axis
#'
#' @param x_end the right most position that will be displayed on the x axis
#'
#' @return a good looking graph
#'
#' @examples
#' fun <- function(x, ...) { return(dlnorm(x = x, meanlog = 100, sdlog = 20, log = FALSE))  }
#' graph <- plot_probability_density_function(fun = fun, x_start = 10, x_end = 150)
#' graph
#'
#' @export
plot_probability_density_function = function(
  fun,
  x_start,
  x_end,
  title = NULL,
  subtitle = NULL,
  caption = NULL, ...) {

  # And put a title on top of it
  if(is.null(title)){ title <- "Probability Density Function" }

  # Prepare the data
  df <- data.frame(x=c(x_start, x_end))

  # Configure the graph
  graph <- ggplot(df, aes(x)) +

    # Give a little bit of margin on the graph sides
    xlim(x_start, x_end) +

    # Limit the number of digits on the vertical axis
    scale_y_continuous(label = function(x) { round(x,3) }) +

    # Area plot the function
    stat_function(
      colour = model_config_get_option("plot", "pdf", "area", "color"),
      fun = fun,
      geom = 'area',
      fill = model_config_get_option("plot", "pdf", "area", "fill"),
      alpha = model_config_get_option("plot", "pdf", "area", "alpha"),
      size = model_config_get_option("plot", "pdf", "area", "size"),
      xlim = c(x_start, x_end)) +

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Factor value",
      y = "Relative likelihood") +
    #ggtitle(title)

    theme(plot.title = element_text(size = 12, face = "bold"))

  return(graph)
}
