#' validate_parameter, vp
#'
#' A friendly function to simplify the validation of function or method input parameters.
#' \describe{
#'   \item{\code{validate_parameter}}{The original function name.}
#'   \item{\code{vp}}{A shortcut function alias.}
#' }
#'
#' @param parameter_value The original function or method parameter.
#'
#' @param default_value A default value to be substituted if \code{parameter_value} is NULL, NA, etc. (see \code{is_void()}).
#'
#' @param category One of the following strings: \code{"character"}, \code{"numeric"}, \code{"logical"}.
#'
#' @param parameter_length Often, parameters are received as vectors. Often, functions and methods only support vectors of size 1 or sometimes of a very specific length.
#'
#' @return The validated (sometimes coerced) parameter, possibly the default value. Sometimes, accompanied with warnings. If not coerction is possible, raises an error.
#'
#' @examples
#' validate_parameter(NULL, 2, "Numeric", 1)
#' vp(NA ,5, "Numeric", 1)
#' vp(c("a", "b"), c("c", "d"), "String", 2)
#'
#' @export
validate_parameter <- function(
  parameter_value,
  default_value = NULL,
  category = NULL,
  parameter_length = NULL,
  verbosity = NULL,
  ...) {

  # To validate parameters in this function,
  # we do not rely on vp() to avoid recursion.
  if (is_void(verbosity)) { verbosity <- 0 }

  # Default value.
  if (is_void(parameter_value)) { parameter_value <- default_value }

  # Length.
  if (!is_void(parameter_length)) {
    if (length(parameter_value) != parameter_length) {
      warning(paste0(
        "Parameter length: expected = ",
        parameter_length,
        ", received = ",
        length(parameter_value)))
      # TODO: Manage this in best effort mode.
    }
  }

  # Category.
  if (tolower(category) == "numeric") {
    if (!is.numeric(parameter_value)) {
      stop("Non-numeric parameter received")
      }
  } else if (tolower(category) == "character") {
    if (!is.character(parameter_value)) {
      stop("Non-character parameter received")
    }
  }

  return(parameter_value)
}
