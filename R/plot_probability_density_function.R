require(colorspace)
require(ggplot2)
require(labeling)

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
#' @param plot_addition Complementary plot objets to be added to the new plot object for enrichment purposes.
#' \cr This parameter was originally introduced because ggMarginal function from ggExtra made it easy to enrich the plot with a boxplot on top of it, but the resulting plot could no longer be further enriched with additions. To overcome this limitation, I simply open the plot to arbitrary additions via this new parameter.
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
  caption = NULL,
  x_scale_type = NULL,
  y_scale_type = NULL,
  line_color = NULL,
  fill_color = NULL,
  fill_alpha = NULL,
  plot_addition = NULL,
  ...) {

  # Parameters Validation
  title <- vp(title, "Probability Density Function", "character", 1)
  x_scale_type <- vp(x_scale_type, "default", "character", 1, acceptable_values = c("default", "log10"))
  y_scale_type <- vp(y_scale_type, "default", "character", 1, acceptable_values = c("default", "log10"))
  if (is_void(line_color)) { line_color <- model_config_get_option("plot", "pdf", "area", "color") }
  if (is_void(fill_color)) { fill_color <- model_config_get_option("plot", "pdf", "area", "fill") }
  fill_alpha = vp(fill_alpha, .5, "numeric", 1, limit_min = 0, limit_max = 1)

  # GGPlot bug correction
  if (tolower(y_scale_type) == "log10") { fill_color <- NULL }

  # Prepare a data frame for the GGPlot plot
  df <- data.frame(x = c(x_start, x_end))

  # Configure the graph
  graph <- ggplot(df, aes(x)) +

    # Give a little bit of margin on the graph sides
    xlim(x_start, x_end) +

    # Area plot the function
    stat_function(
      colour = line_color,
      fun = fun,
      geom = 'area',
      fill = fill_color,
      alpha = fill_alpha,
      size = model_config_get_option("plot", "pdf", "area", "size"),
      xlim = c(x_start, x_end)) +

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Factor value",
      y = "Relative likelihood") +

    theme(plot.title = element_text(size = 12, face = "bold"))

  if (tolower(x_scale_type) == "log10" ) {
    graph <- graph + scale_x_log10()
  }

  if (tolower(y_scale_type) == "log10" ) {
    graph <- graph + scale_y_log10()
  }
  else {
    # Limit the number of digits on the vertical axis
    scale_y_continuous(labels = function(x) {return(fn(x))} )
  }

  if (!is_void(plot_addition)) {
    graph <- graph + plot_addition
  }

  return(graph)
}
