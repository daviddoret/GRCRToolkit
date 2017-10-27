
#' mimax
#'
#' A shortcut function that limits a value between an inclusive minimum and an inclusive maximum.
#'
#' @param x A vector of numeric values.
#'
#' @param minimum The minimal allowed value for \code{x}. If \code{NULL}, no minimum will be applied.
#'
#' @param maximum The maximal allowed value for \code{x}. If \code{NULL}, no maximum will be applied.
#'
#' @return Numeric vector. Bounded x.
#'
#' @examples
#' mimax(seq(-10,10,.1),-2,2)
#'
#' @export
mimax <- function(
  x,
  minimum = NULL,
  maximum = NULL,
  ...) {
  return(min(max(x, minimum), maximum))
}
