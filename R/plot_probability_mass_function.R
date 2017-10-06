# Package preparation
#if (!require(pacman)) install.packages(pacman)
#pacman::p_load(colorpsace, ggplot2,labeling)
if (!require(pacman)) install.packages(pacman)
pacman::p_load(colorspace, ggplot2, labeling)


#' plot_probability_mass_function
#'
#' Generates a good looking graph representing a probability mass function.
#'
#' @param fun a parameterized probability mass function receiving a single x parameter
#'
#' @param x_start the left most position that will be displayed on the x axis
#'
#' @param x_end the right most position that will be displayed on the x axis
#'
#' @return a good looking graph
#'
#' @examples
#' fun <- function(x, ...) { return(dpois(x = x, lambda = 5, log = FALSE))  }
#' graph <- plot_probability_mass_function(fun = fun, x_start = 0, x_end = 20)
#' graph
#'
#' @export
plot_probability_mass_function <- function(
  fun,
  x_start,
  x_end,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...) {

  # And put a title on top of it
  if(is.null(title)){ title <- "Probability Mass Function" }

  # Prepare the data
  df <- data.frame(x=c(x_start:x_end))

  # Configure the graph
  graph <- ggplot(df, aes(x, y=fun(x))) +

    # X-axis limits are inclusive (in my mind)
    xlim(x_start, x_end) +

    # Limit the number of digits on the vertical axis
    scale_y_continuous(label = function(x) { round(x,3) }) +

    # scale_x_discrete(labels = df$x) +

    # Area plot the function
    #geom_bar(
    geom_histogram(
      alpha = model_config_get_option("plot", "mdf", "bar", "alpha"),
      colour = model_config_get_option("plot", "mdf", "bar", "color"),
      fill = model_config_get_option("plot", "mdf", "bar", "fill")
      #statbin = 50,
      #stat = "identity") +
    )

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Factor value",
      y = "Probability") +

    theme(plot.title = element_text(size = 12, face = "bold"))

  return(graph)

}
