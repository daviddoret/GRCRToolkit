# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(colorspace, ggplot2, labeling)

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
  ...) {

  # Default values
  if (is_nanull(title)) { title <- "Sample Histogram" }
  if (is_nanull(bins)) { bins <- 100 }
  if (is_nanull(x_scale_type)) { x_scale_type <- "default" }
  if (is_nanull(y_scale_type)) { y_scale_type <- "default" }

  # Prepare the data
  df <- data.frame(x = sample)

  # Configure the graph
  graph <- ggplot(df, aes(x)) +

  # Limit the number of digits on the vertical axis
  # scale_y_continuous(label = function(x) { round(x,3) }) +

    # coord_trans(x="log2") +
    #geom_histogram(bins=1000, colour="black", fill="white") +
    stat_bin(bins = bins, colour="black", fill="skyblue2") +
    #geom_dotplot(
    #  binwidth = 100)
      #dotsize = 1.25,
      #position = "dodge") +

    #scale_x_log10(breaks=c(-1000,-100,-10,-1,1,10,100,1000,10000,30000)) +
    #scale_x_log10()+
    #scale_x_sqrt() +

    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = "Factor value",
      y = "Number") +
    #ggtitle(title)
    theme(plot.title = element_text(size = 12, face = "bold"))

  if (!is.null(x_start) & !is.null(x_end))
  {
    graph <- graph + xlim(x_start, x_end)
  }

  if (x_scale_type == "log10" ) {
    graph <- graph + scale_x_log10()
  }

  if (y_scale_type == "log10" ) {
    graph <- graph + scale_y_log10()
  }

  return(graph)
}
