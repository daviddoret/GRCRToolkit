
#' is_void
#'
#' Systematically testing for variable "emptiness" in R is a bit of a pain.
#' This is a shortcut function that tests if a variable \code{x} is \code{NULL} or \code{NA}.
#' If \code{x} is a vector, \code{is_void} does not test the elements of the vector,
#' but returns \code{FALSE} because a vector is an object that is neither \code{NULL} nor \code{NA}.
#'
#' @param x a variable
#'
#' @return boolean
#'
#' @examples
#' is_void(NULL)
#' is_void(NA)
#' is_void(0)
#' is_void(c(NA,NA,NA))
#'
#' @export
is_void <- function(x, ...) {
  if (is.null(x)) return(TRUE)
  # If it contains > 1 element, I consider x a real object
  # even if it only contains NAs.
  if (length(x) > 1) return(FALSE)
  if (any(is.na(x))) return(TRUE)
  return(FALSE)
}
