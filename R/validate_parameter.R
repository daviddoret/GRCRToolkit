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
#' @param acceptable_values A list of of acceptable values. Any attempt to use a value not listed will raise an error. This is obvious, but note that \code{"default_value"} is applied before validation takes place against the list of acceptable values.
#'
#' @return The validated (sometimes coerced) parameter, possibly the default value. Sometimes, accompanied with warnings. If not coerction is possible, raises an error.
#'
#' @examples
#' validate_parameter(NULL, 2, "Numeric", 1)
#' vp(NA ,5, "Numeric", 1)
#' vp(c("a", "b"), c("c", "d"), "String", 2)
#' vp(c("a", "b"), NULL, "character", 2, c("a", "b", "c"))
#'
#' @export
validate_parameter <- function(
  parameter_value,
  default_value = NULL,
  category = NULL,
  parameter_length = NULL,
  acceptable_values = NULL,
  limit_min = NULL,
  limit_max = NULL,
  verbosity = NULL,
  ...) {

  # To validate parameters in this function,
  # we do not rely on vp() to avoid recursion.
  if (is_void(verbosity)) { verbosity <- 0 }

  # Default value.
  if (is_void(parameter_value)) { parameter_value <- default_value }

  error_level <- 0
  messages <- c()

  # Length.
  if (!is_void(parameter_length)) {
    if (length(parameter_value) != parameter_length) {
      error_level <- max(error_level, 1)
      messages <- c(messages, "Parameter length: expected = ",
        parameter_length,
        ", received = ",
        length(parameter_value))
      # TODO: Manage this in best effort mode.
    }
  }

  # Category ("loose" type)
  if (!is_void(category)) {
    if (tolower(category) == "numeric") {
      if (!is.numeric(parameter_value)) {
        error_level <- max(error_level, 2)
        messages <- c(messages, "Non-numeric parameter received")
        }
    } else if (tolower(category) == "character") {
      if (!is.character(parameter_value)) {
        error_level <- max(error_level, 2)
        messages <- c(messages, "Non-character parameter received")
      }
    }
  }

  # Acceptable Values.
  if (!is_void(acceptable_values)) {
    if (tolower(category) == "character")
    {
      # Case insensitive character comparison
      # TODO: Make it accent insensitive as well
      if (is.element(FALSE, is.element(tolower(parameter_value) ,tolower(acceptable_values)))) {
        error_level <- max(error_level, 2)
        messages <- c(messages, "Unacceptable parameter values received")
      }
    }
    else
    {
      if (is.element(FALSE, is.element(parameter_value ,acceptable_values))) {
        error_level <- max(error_level, 2)
        messages <- c(messages, "Unacceptable parameter values received")
      }
    }
  }

  # Min / Max Limits.
  if (!is_void(limit_min)) {
    if (min(parameter_value) < limit_min) {
      error_level <- max(error_level, 2)
      messages <- c(messages, "Parameter values below minimum limit received")
    }
  }
  if (!is_void(limit_max)) {
    if (max(parameter_value) > limit_max) {
      error_level <- max(error_level, 2)
      messages <- c(messages, "Parameter values above maximum limit received")
    }
  }

  if (verbosity > 0 | error_level > 0) {
    caller <- get_caller(level = 2)
    if (!is.null(caller)) {
      if (substr(caller,1,3) == "vp(") {
      # Bypass the vp(...) alias.
      caller <- get_caller(level = 3)
      }
    }
    if (error_level == 2) {
      stop(cat("Function call:", caller, "Validation report:", messages, sep = "\n"))
    } else if (error_level == 1) {
      warning(cat("Function call:", caller, "Validation report:", messages, sep = "\n"))
    } else {
      message(cat("Function call:", caller, "Validation report:", messages, sep = "\n"))
    }
  }

  return(parameter_value)
}
