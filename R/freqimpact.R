if (!require(pacman)) install.packages(pacman)

#' freqimpact
#'
#' Blablabla
#'
#' @param n The desired size of the frequency vector.
#'
#' @param frequency_function A frequency function supporting parameter \code{n} that returns a vector of integer frequencies of size \code{n}.
#'
#' @param impact_function An impact function supporting parameter \code{n} that returns a vector of impacts of size \code{n}.
#'
#' @param output_class Default: "vector". If \code{output_class} = "data.frame", returns a detailed data.frame with columns \code{frequency}, \code{impact_list} and \code{factor_value}.
#'
#' @return a vector or data.frame
#'
#' @examples
#' freqimpact(
#'   n = 8,
#'   frequency_function = function(n) {return(rbinom(n = n, size = 4, prob = .5))},
#'   impact_function = function(n) { return(rnorm(n = n, mean = 0, sd = 100))},
#'   output_class = "data.frame")
#'
#' @export
freqimpact <- function(n, frequency_function, impact_function, output_class = NULL, ...) {

  if (is_nanull(output_class)) { output_class <- "vector" }

  frequencies <- frequency_function(n = n, ...)

  df <- data.frame(frequency = frequencies)

  df$impact_list <- lapply(
    X = frequencies,
    FUN = function(frequency){
      return(
        ifelse(frequency == 0, NA, impact_function(n = frequency)))
    })

  df$factor_value <- unlist(lapply(
    X = df$impact_list,
    FUN = function(impact_list){
      return(
        ifelse(is.na(impact_list), 0, sum(unlist(impact_list)
            )))
    }))

  if (output_class == "vector") {
    return(df$factor_value)
  }

  if (output_class == "data.frame") {
    return(df)
  }

  }
