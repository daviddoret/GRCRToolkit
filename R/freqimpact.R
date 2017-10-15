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
#'   impact_function = function(n) { return(rnorm(n = n, mean = 100, sd = 5))},
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
      if(frequency == 0) {
        # When frequency is zero, no event took place.
        # For this reason, NA sounds more natural than 0
        # specifically in the impact_list column.
        return(NA)
      }
      else
      {
        return(impact_function(n = frequency))
      }
    })

  df$factor_value <- unlist(lapply(
    X = df$impact_list,
    FUN = function(impact_list){
      if (length(impact_list) == 1) # The test on length avoids a warning as is.na() does not support vectors.
        {
        if (is.na(impact_list)) {
          # But the factor_value column is the result of freq x impact,
          # so here if no event took place, NA would be a bad choice
          # because during that period of time, we really had an impact
          # of 0
          return(0)
          }
        }
      return(sum(unlist(impact_list)))
    }))

  if (output_class == "vector") {
    return(df$factor_value)
  }

  if (output_class == "data.frame") {
    return(df)
  }

  }
