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


#' plot_quantile_function
#'
#' Produces a good looking graph of a probability density function.
#'
#' @param fun a parameterized probability density function receiving a single x parameter
#'
#' @param y_start the left most position that will be displayed on the x axis
#'
#' @param y_end the right most position that will be displayed on the x axis
#'
#' @return a good looking graph
#'
#' @examples
#' fun <- function(x, ...) { return(qlnorm(p = x, meanlog = 100, sdlog = 20, log = FALSE))  }
#' graph <- plot_quantile_function(fun = fun, y_start = 10, y_end = 150)
#' graph
#'
#' @export
plot_quantile_function = function(
  fun,
  y_start,
  y_end,
  title = NULL,
  subtitle = NULL,
  caption = NULL, ...) {

  # And put a title on top of it
  if(is.null(title)){ title <- "Quantile Function" }

  # Prepare the data
  df <- data.frame(x=c(y_start, y_end))

  # Configure the graph
  graph <- ggplot(df, aes(x)) +

    # Give a little bit of margin on the graph sides
    xlim(y_start, y_end) +
    # ylim: let it scale automatically
    # scale_y_continuous(trans = "log") +

    # Limit the number of digits on the vertical axis
    scale_y_continuous(label = function(x) { round(x,3) }) +

    # Area plot the function
    stat_function(
      colour = model_config_get_option("plot", "qdf", "area", "color"),
      fun = fun,
      geom = 'line',
      size = model_config_get_option("plot", "qdf", "area", "size"),
      xlim = c(y_start, y_end)) +

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Cumulative probability",
      y = "Factor value") + # (logarithmic scale)") +
    #ggtitle(title)

    theme(plot.title = element_text(size = 12, face = "bold"))

  return(graph)
}
