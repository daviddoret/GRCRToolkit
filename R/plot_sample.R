require(colorspace)
require(ggplot2)
require(labeling)
require(cowplot)
require(ggExtra)

#' plot_sample
#'
#' A friendly shortcut function that produces a good looking graph of a population sample.
#' The main and lower part is composed of an histogram.
#' The upper part is enriched with a marginal box plot.
#'
#' @param sample A vector of the population sample.
#'
#' @param title A title for the plot.
#'
#' @param subtitle A subtitle for the plot. Unfortunately, this is not yet supported with the non-dev version of GGPlot2, but should come soon.
#'
#' @param caption A caption for the plot. Unfortunately, this is not yet supported with the non-dev version of GGPlot2, but should come soon.
#'
#' @param x_start The left most position that will be displayed on the x axis. Overflowing values will be ignored.
#'
#' @param x_end The right most position that will be displayed on the x axis. Overflowing values will be ignored.
#'
#' @param bins The number of bins in the histogram.
#'
#' @param x_scale_type "Normal" (default) or "Log10".
#'
#' @param y_scale_type "Normal" (default) or "Log10".
#'
#' @param variable_type "Discrete" (default) or "Continuous".
#'
#' @param verbosity 0: no messages. > 0 more and more verbose messages.
#'
#' @param plot_addition Complementary plot objets to be added to the new plot object for enrichment purposes.
#' \cr This parameter was originally introduced because ggMarginal function from ggExtra made it easy to enrich the plot with a boxplot on top of it, but the resulting plot could no longer be further enriched with additions. To overcome this limitation, I simply open the plot to arbitrary additions via this new parameter.
#'
#' @return A good looking graph.
#'
#' @examples
#' plot_sample(sample = rnorm(n = 10000), title = "Normal sample")
#' plot_sample(sample = rpois(n = 10000, lambda = exp(1)), title = "Poisson sample", variable_type = "Discrete")
#' plot_sample(sample = rgeom(n = 1000, p = .2), title = "Geometric sample", variable_type = "Discrete")
#'
#' @export
plot_sample = function(
  sample,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x_start = NULL,
  x_end = NULL,
  bins = NULL,
  x_scale_type = NULL,
  y_scale_type = NULL,
  variable_type = NULL,
  plot_addition = NULL,
  ...)
{

  # Default values
  if (is_void(title)) { title <- "Sample Histogram" }
  if (is_void(caption)) { caption <- paste0("n = ", fn(length(sample),0)) }
  if (is_void(bins)) { bins <- 100 }
  if (is_void(x_scale_type)) { x_scale_type <- "default" }
  if (is_void(y_scale_type)) { y_scale_type <- "default" }
  if (is_void(variable_type)) { variable_type <- "Continuous" }

  # Prepare the data
  df <- data.frame(x = sample)

  # Configure the graph
  histo <- ggplot(df, aes(x))

  if (variable_type == "Continuous") {
    histo <- histo + stat_bin(
      alpha = .8,
      bins = bins,
      colour = "black",
      fill = "lightblue2")
  }
  else if(variable_type == "Discrete") {
    histo <- histo + geom_histogram(
        alpha = .8,
        colour = "black",
        fill = "lightblue2",
        binwidth = 1)
  }

  histo <- histo + theme(plot.title = element_text(size = 12, face = "bold"))

  histo <- histo +
    labs(
      title = paste0(title, "\n "), # Ugly hack to assure a margin between the title and the marginal boxplot
      subtitle = subtitle, # Unfortunately, not yet supported with CRAN version of GGPlot2, but should come soon
      caption = caption, # Unfortunately, not yet supported with CRAN version of GGPlot2, but should come soon
      x = "Factor value",
      y = "Count")

#  whisker <- ggplot(df, aes(x)) +
#    geom_boxplot(outlier.shape = 1) +  coord_flip()

  if (!is.null(x_start) & !is.null(x_end))
  {
    histo <- histo + xlim(x_start, x_end)
  }

  if (x_scale_type == "log10" ) {
    histo <- histo + scale_x_log10()
#    whisker <- whisker + scale_y_log10()
  }

  if (y_scale_type == "log10" ) {
    histo <- histo + scale_y_log10()
#    whisker <- whisker + scale_y_log10()
  }

#  multi <- plot_grid(
#    risk_plot,
#    graph,
#    align="v",
#    nrow = 2,
#    ncol = 1,
#    rel_heights = c(1,.2))

  if (!is_void(plot_addition)) {
    histo <- histo + plot_addition
  }

  output <- ggMarginal(
    histo,
    type = "boxplot",
    fill = "plum1",
    colour = "grey35",
    alpha = .5,
    size = 15,
    outlier.color = "plum1",
    outlier.shape = 3,
    outlier.size = 3,
    outlier.stroke = 1,
    margins = c("x"))

  return(output)

}
