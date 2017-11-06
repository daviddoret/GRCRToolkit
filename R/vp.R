#' @rdname validate_parameter
vp <- function(
  parameter_value,
  default_value = NULL,
  category = NULL,
  parameter_length = NULL,
  verbosity = NULL,
  ...) {
  return(
    validate_parameter(
      parameter_value = parameter_value,
      default_value = default_value,
      category = category,
      parameter_length = parameter_length,
      verbosity = verbosity,
      ...)
  )
}
