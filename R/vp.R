#' @rdname validate_parameter
vp <- function(
  parameter_value,
  default_value = NULL,
  category = NULL,
  parameter_length = NULL,
  acceptable_values = NULL,
  limit_min = NULL,
  limit_max = NULL,
  verbosity = NULL,
  ...) {
  return(
    validate_parameter(
      parameter_value = parameter_value,
      default_value = default_value,
      category = category,
      parameter_length = parameter_length,
      acceptable_values = acceptable_values,
      limit_min = limit_min,
      limit_max = limit_max,
      verbosity = verbosity, # No decrementing because this is an alias.
      ...)
  )
}
