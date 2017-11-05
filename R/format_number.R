#' format_number, fn
#'
#' A friendly function to simplify the output of nicely formatted numbers.
#' \describe{
#'   \item{\code{format_number}}{The original function name.}
#'   \item{\code{fn}}{A shortcut function alias.}
#' }
#' \cr \strong{TODO}: This function is a bit procedural and is probably not optimal from a in terms of performances. We should test it with large volumes of numbers and optimize it as needed.
#'
#' @param x a vector of numbers
#'
#' @param digits the number of digits to be displayed
#'
#' @return nicely formatted numbers
#'
#' @examples
#' format_number(c(10.555,3.14),2)
#' fn(c(10.555,3.14),2)
#'
#' @export
format_number <- function(
  x,
  digits = NULL,
  ...) {
  # Reasonable default number of digits when not specified.
  if (is_void(digits)) { digits = 2 }

  x_is_na <- vapply(x,is.na,TRUE)

  x_rounded <- round(x, digits)
  x_lose_precision <- x != x_rounded
  x_tilde <- ifelse(x_lose_precision, "~", "")
  x_formatted_number <- format(x_rounded, digits = digits, big.mark = "'", scientific = FALSE, nsmall = digits)
  x_formatted_number_with_tilde <- paste0(
    x_tilde, x_formatted_number)

  x_final <- ifelse(x_is_na, "n/a", x_formatted_number_with_tilde)

  return(x_final)

}
