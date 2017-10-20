if (!require(pacman)) install.packages(pacman)

#' apply_limit_max
#'
#' A shortcut function to apply various types of maximum limits on a vector of values.
#'
#' @param x a vector of numbers
#'
#' @field limit_value A strict (inclusive) upper bound applied to the vector values. If NULL or NA, no limit will be applied.
#'
#' @field limit_behavior One of the following options determining how values will be maintained within \code{limit_max_value}: \code{"Limit"} (default), \code{"Redraw"}, \code{"Discard"}. \code{"Limit"}: When an out of bound value is drawn, apply \code{min}/\code{max} functions to force it within bounds. \code{"Redraw"}: When an out of bound value is drawn, we redraw it until it is within bound. \code{"Discard"}: When an out of bound value is drawn, remove it from the sample.
#'
#' @field redraw_function If \code{limit_behavior} equals \code{"Redraw"}, the function to be called to draw new individuals from the population. It is expected that the function receives first an integer parameter corresponding to the number of individuals to be drawn, followed by ... WARNING: \code{redraw_function} will be called until the population reach its original size, which may end up in numerous calls or an infinite loop if \code{redraw_function} does not return enough individuals within limit bounds.
#'
#' @return a vector whose values are within the limit bounds
#'
#' @examples
#' apply_limit_max(x = 1:10)
#' # Output: [1]  1  2  3  4  5  6  7  8  9 10
#' apply_limit_max(x = 1:10, limit_value = 5)
#' # Output: [1] 1 2 3 4 5 5 5 5 5 5
#' apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Limit")
#' # Output: [1] 1 2 3 4 5 5 5 5 5 5
#' apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Discard")
#' # Output: [1] 1 2 3 4 5
#' apply_limit_max(x = rnorm(n=20), limit_value = 0, limit_behavior = "Redraw", redraw_function = rnorm)
#' # Output: a default normal sample of size 20 whose values are all <= 0
#'
#' @export
apply_limit_max <- function(
  x,
  limit_value = NULL,
  limit_behavior = NULL,
  redraw_function = NULL,
  ...) {

  # Default values
  if (is_nanull(limit_behavior)) { limit_behavior = "Limit" }

  if (!is_nanull(limit_value)) {
    if (tolower(limit_behavior) == "limit") {
      # All values higher than the limit are replaced by the limit
      x <- pmin(x, limit_value)
    } else if (tolower(limit_behavior) == "discard") {
      # All values strictly higher than the limit are discarded
      x <- x[x <= limit_value]
    } else if (tolower(limit_behavior) == "redraw") {
      target_length <- length(x)
      # Discard the individuals that are outside bound
      x <- x[x <= limit_value]
      while (length(x) < target_length) {
        # How many individuals are we missing?
        missing <- target_length - length(x)
        # Redraw the individuals we miss
        x <- c(x, redraw_function(missing, ...))
        # Discard the individuals that are outside bound
        x <- x[x <= limit_value]
      }
    }
  }

  return(x)

}
