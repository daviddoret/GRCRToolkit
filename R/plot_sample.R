# Package preparation
if (!require(pacman)) install.packages(pacman)
pacman::p_load(colorpsace, ggplot2,labeling)

plot_sample = function(
  sample,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  x_start = NULL,
  x_end = NULL,
  bins = NULL,
  ...) {

  # And put a title on top of it
  if(is.null(title)){ title <- "Sample Histogram" }

  if(is.null(bins)){ bins <- 100 }

  # Prepare the data
  df <- data.frame(x=sample)

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

  if(!is.null(x_start) & !is.null(x_end))
  {
    graph <- graph + xlim(x_start, x_end)
  }

  return(graph)
}
