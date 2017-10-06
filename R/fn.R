if (!require(pacman)) install.packages(pacman)

#' fn = format number
#'
#' A shortcut function to make the string manipulation strings less verbose.
#' TODO: This function is very procedural and is probably inefficient performance wise. Rewrite it.
#'
#' @param x a vector of numbers
#'
#' @param digits the number of digits to be displayed
#'
#' @return nicely formatted numbers
#'
#' @examples
#' fn(c(10.555,3.14),2)
#'
#' @export
fn <- function(x, digits = NULL) {
  # Reasonable default number of digits when not specified.
  if(is.null(digits)) { digits = 2 }

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
