if (!require(pacman)) install.packages(pacman)

#' is_nanull
#'
#' A shortcut function that test if a variable \code{x} is \code{NULL} or \code{NA}.
#' If \code{x} is a vector, \code{is_nanull} does not test the elements of the vector,
#' but returns \code{FALSE} because a vector is an object that is neither \code{NULL} nor \code{NA}.
#'
#' @param x a variable
#'
#' @return boolean
#'
#' @examples
#' is_nanull(NULL)
#' is_nanull(NA)
#' is_nanull(0)
#' is_nanull(c(NA,NA,NA))
#'
#' @export
is_nanull <- function(x, ...) {
  if (is.null(x)) return(TRUE)
  # If it contains > 1 element, I consider x a real object
  # even if it only contains NAs.
  if (length(x) > 1) return(FALSE)
  if (any(is.na(x))) return(TRUE)
  return(FALSE)
}
