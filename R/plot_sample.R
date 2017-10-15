# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(colorspace, ggplot2, labeling, cowplot, ggExtra)

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
  ...)
{

  # Default values
  if (is_nanull(title)) { title <- "Sample Histogram" }
  if (is_nanull(subtitle)) { subtitle <- paste0("n = ", fn(length(sample),0)) }
  if (is_nanull(bins)) { bins <- 100 }
  if (is_nanull(x_scale_type)) { x_scale_type <- "default" }
  if (is_nanull(y_scale_type)) { y_scale_type <- "default" }
  if (is_nanull(variable_type)) { variable_type <- "Continuous" }

  # Prepare the data
  df <- data.frame(x = sample)

  # Configure the graph
  histo <- ggplot(df, aes(x))

  if (variable_type == "Continuous") {
    histo <- histo + stat_bin(
      alpha = .8,
      bins = bins,
      colour = "black",
      fill = "skyblue2")
  }
  else if(variable_type == "Discrete") {
    histo <- histo + geom_histogram(
        alpha = .8,
        colour = "black",
        fill = "skyblue2",
        binwidth = 1)
  }

  histo <- histo + theme(plot.title = element_text(size = 12, face = "bold"))

  histo <- histo +
    labs(
      title = paste0(title, "\n "), # Ugly hack to assure a margin between the title and the marginal boxplot
      subtitle = subtitle,
      caption = caption,
      x = "Factor value",
      y = "Number")


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

  output <- ggMarginal(
    histo,
    type = "boxplot",
    fill = "plum2",
    colour = "grey35",
    alpha = .5,
    size = 15,
    margins = c("x"))

  return(output)

}
