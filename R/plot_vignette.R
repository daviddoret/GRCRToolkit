require(colorspace)
require(ggplot2)
require(labeling)

#' plot_vignette
#'
#' Produces a good looking plot of a summary text.
#' May be nicely integrated in a plot grid to provide summary stats.
#'
#' @param text_lines a vector of text lines to be included in the plot.
#'
#' @param title the title of the plot
#'
#' @return a good looking graph
#'
#' @examples
#' N/A
#'
#' @export
plot_vignette = function(
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  text = NULL...) {

  # And put a title on top of it
  if (is_void(title)){ title <- "Vignette" }

  vignette <- ggplot() +
    geom_blank() +
    theme(
      line=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()) +
  labs(
      title = title,
      subtitle = subtitle,
      caption = caption) +
  theme(plot.title = element_text(size = 12, face = "bold"))

  for(line in 1:length(text)){
    y <- 1 + length(text) - line
    vignette <- vignette +
      annotate(
        "text",
        label = text[line],
        x=1,
        y=y,
        fontface="plain",
        size=4,
        lineheight=.8)
  }

  return(vignette)
}
