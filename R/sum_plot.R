
#' sum_plot
#'
#' Add together n plots.
#' \cr GGPlot plots may be enriched by adding together multiple geoms.
#' \cr But \code{NULL} and/or \code{NA} can't be added with geoms.
#' \cr This function makes it easier to add together a series of geoms, getting rid of \code{NULL} and \code{NA} in the process.
#'
#' @param verbosity
#'
#' @param ... A series of n plots to be added together.
#'
#' @return An enriched plot.
#'
#' @examples
#' sum_plot(p1, p2, p3)
#'
#' @export
sum_plot <- function(verbosity = NULL, ...) {

  if (is_void(verbosity)) { verbosity = 0 }

  s <- NULL
  for (p in list(...)) {
    if (is(p, "ggplot")) {
      if (is_void(s)) {
        # First valid item in the series
        s <- p
      } else {
        s <- s + p
      }
    } else {
      if (verbosity > 0) {
        message("Discard non-ggplot object:")
        message(str(p))
      }
    }
  }
  return(s)
}
