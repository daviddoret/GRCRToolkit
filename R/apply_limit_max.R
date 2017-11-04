if (!require(pacman)) install.packages(pacman)

#' apply_limit_max
#'
#' A shortcut function to apply various types of maximum limits on a data set.
#'
#' @section Future enhancements:
#' \itemize{
#' \item{The replacement algorithm implemented is extremely naive. First, a protection mechanism should be implemented to prevent infinite loops. Second, the ratio of individuals within the limit should be computed from the original population and replacement samples, and should be used to retrieve more individuals than necessary when calling \code{replace_function} to reduce the number of loops required.}
#' \item{Provide support for matrices}
#' \item{Provide support for lists}
#' }
#'
#' @param x A vector of numeric values. \cr Or a dataframe but then parameter \code{target_column} must be provided or, by default, the limit will be applied to the first column.
#'
#' @field limit_value A strict (inclusive) lower bound applied to the original values. If NULL or NA, no limit will be applied.
#'
#' @field limit_behavior One of the following options determining how the limit will be applied to the original set of data:
#' \itemize{
#'  \item{\code{Limit}: }{(default) When a value is beyond limit, apply \code{min}/\code{max} functions to force it within bounds.}
#'  \item{\code{Replace}: }{When a value is beyond limit, get a new one from \code{replace_function} until its is within bound.}
#'  \item{\code{Discard}: }{When a value is beyond limit, remove it from the sample.}
#'  \item{\code{Info}: }{Return a vector of boolean values with \code{TRUE} matching the positions of values within limit and \code{FALSE} the positions of values beyond limit.}
#'}
#'
#' @field replace_function If \code{limit_behavior} equals \code{Replace}, the function to be called to draw new individuals from the population. It is expected that the function receives first an integer parameter corresponding to the number of individuals to be drawn, followed by ...
#' \cr WARNING #1: \code{replace_function} will be called until the population reaches its original size, which may end up in numerous calls or an infinite loop if \code{replace_function} does not return enough individuals within limit bounds.
#' \cr WARNING #2: if \code{x} is a dataframe, \code{replace_function} must return a dataframe with the same data structure for rbind to succeed.
#'
#' @field target_column If x is a dataframe, the name or index position of the column containing the numeric vector on which to apply the limit.
#'
#' @return A modified vector or dataframe whose values are within the limit
#'
#' @examples
#'
#' # Examples with vectors
#' apply_limit_max(x = 1:10)
#' # Output: [1]  1  2  3  4  5  6  7  8  9 10
#' apply_limit_max(x = 1:10, limit_value = 5)
#' # Output: [1]  5  5  5  5  5  6  7  8  9 10
#' apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Limit")
#' # Output: [1]  5  5  5  5  5  6  7  8  9 10
#' apply_limit_max(x = 1:10, limit_value = 5, limit_behavior = "Discard")
#' # Output: [1]  6  7  8  9 10
#' apply_limit_max(x = rnorm(n=20), limit_value = 0, limit_behavior = "Replace", replace_function = rnorm)
#' # Output: a default normal sample of size 20 whose values are all >= 0
#'
#' # Examples with dataframes
#' apply_limit_max(x = data.frame(x = 1:10, y = 2, z = rnorm(n=10)), limit_value = 5, limit_behavior = "limit", target_column = "x")
#' apply_limit_max(x = data.frame(x = 1:10, y = 2, z = rnorm(n=10)), limit_value = 5, limit_behavior = "discard", target_column = "x")
#'
#' # Example with dataframe and replacement
#' rf <- function(x,...){return(data.frame(x = rnorm(x,...), y = "new individuals"))}
#' df_original <- data.frame(x = rnorm(12), y = "old individuals")
#' apply_limit_max(x = df_original, limit_value = 0, limit_behavior = "replace", target_column = "x", replace_function = rf)
#'
#' @export
apply_limit_max <- function(
  x,
  limit_value = NULL,
  limit_behavior = NULL,
  replace_function = NULL,
  target_column = NULL,
  verbosity = NULL,
  ...) {

  # Default values
  if (is_void(limit_behavior)) { limit_behavior = "Limit" }
  if (!tolower(limit_behavior) == "replace") { replace_function <- NULL } # Conservatively cleaning useless parameters
  if (is_void(target_column)) { target_column = 1 }
  if (is_void(verbosity)) { verbosity = 0 }

  if (verbosity > 0) { message(limit_behavior) }

  if (!is_void(limit_value)) {

    if (is.vector(x)) {
      if (tolower(limit_behavior) == "limit") {
        # All values higher than the limit will be replaced by the limit
        x <- pmin(x, limit_value)
      } else if (tolower(limit_behavior) == "discard") {
        # All values strictly higher than the limit are discarded
        x <- x[x <= limit_value]
      } else if (tolower(limit_behavior) == "replace") {
        target_length <- length(x)
        # Discard the individuals that are outside bound
        x <- x[x <= limit_value]
        while (length(x) < target_length) {
          # How many individuals are we missing?
          missing <- target_length - length(x)
          # Replace the individuals we miss
          x <- c(x, replace_function(missing, ...))
          # Discard the individuals that are outside bound
          x <- x[x <= limit_value]
        }
      } else if (tolower(limit_behavior) == "info") {
        return(x <= limit_value)
      }
    }

    if (is.data.frame(x)) {
      target_vector <- x[[target_column]] # Use double square bracket notation to retrieve a vector.
      if (tolower(limit_behavior) == "limit") {
        # All values lower than the limit will be replaced by the limit
        limited_vector <- apply_limit_max(x = target_vector, limit_value = limit_value, limit_behavior = "limit", ...)
        x[target_column] <- limited_vector
        return(x)
      } else if (tolower(limit_behavior) == "discard") {
        # All values strictly lower than the limit are discarded
        within_limit <- apply_limit_max(x = target_vector, limit_value = limit_value, limit_behavior = "info", ...)
        return(x[within_limit,])
      } else if (tolower(limit_behavior) == "replace") {
        target_length <- length(target_vector)
        # Discard the individuals that are outside bound
        x <- apply_limit_max(x = x, limit_value = limit_value, limit_behavior = "Discard", target_column = target_column, ...)
        current_vector <- x[[target_column]]
        # How many individuals are we missing?
        missing <- target_length - length(current_vector)
        while (missing > 0) {
          # Replace and append the individuals we missed
          new_individuals <- replace_function(missing, ...)
          x <- rbind(x, new_individuals)
          # Re-discard the new individuals that are outside bound
          x <- apply_limit_max(x = x, limit_value = limit_value, limit_behavior = "Discard", target_column = target_column, ...)
          current_vector <- x[[target_column]]
          # How many individuals are we missing?
          missing <- target_length - length(current_vector)
          message(paste0("missing ",missing))
        }
        return(x)
      } else if (tolower(limit_behavior) == "info") {
        # Here we only return a vector, and not the dataframe.
        return(apply_limit_max(x[target_column], limit_value = limit_value, limit_behavior = limit_behavior, ...))
      }
    }
  }
  return(x)
}
