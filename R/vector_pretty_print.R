#' vector_pretty_print
#'
#' Converts a named vector to a single-line string pretty representation
#'
#' @param l the list
#'
#' @return a string that is a pretty representation of the vector
#'
#' @examples
#' l <- list(1, 2, 3, 4)
#' names(l) <- c("a", "b", "c", "d")
#' print(list_pretty_print(l))
#'
#' @export
vector_pretty_print <- function(v, ...) {
  output <- ""
  for(i in 1:length(v)){
    if(i > 1)
    {
      output <- cat(output, ",", sep="")
    }
    output <- cat(output,
                  str(names(v)[i]),
                  "=",
                  str(v[i]),
                  sep="")
  }
  return(output)
}
