require(gld)

#' get_gld_mode
#'
#' Returns the mode of the GLD (Generalized Lambda Distribution), aka Tukey distribution.
#' \cr When the shape parameters \code{lambda3 = -1, lambda4 = -1}, the mode is equivalent to the location parameter \code{lambda1}.
#' \cr But when the shape parameters are tweaked, we observe a shift of the distribution mode.
#' \cr This behavior is accentuated when \code{lambda2} approaches 0.
#'
#' @param lambda1
#'
#' @param lambda2
#'
#' @param lambda3
#'
#' @param lambda4
#'
#' @param search_range_start
#'
#' @param search_range_end
#'
#' @return The mode.
#'
#' @examples
#' get_gld_mode(lambda1 = 100, lambda2 = 100, lambda3 = -10, lambda4 = -1, search_range_start = -1000, search_range_end = 1000)
#'
#' @section Future improvements:
#' \itemize{
#'   \item{ Perform some research to check if there exists an analytical solution to this function. Then, implement it to avoid the usage of optimization. In its current version, the function relies on optimization to find a good estimate for the mode. Consequently, the precision of the mode or computational intensity to find it are not optimal. }
#'   \item{ If no analytical solution is found and if \code{search_range_start} and \code{search_range_end} are not provided, provide best effort default values. }
#' }
#'
#' @export
get_gld_mode <- function(
  lambda1 = NULL,
  lambda2 = NULL,
  lambda3 = NULL,
  lambda4 = NULL,
  search_range_start = NULL,
  search_range_end = NULL,
  ...) {

  if (lambda2 < 0) { stop("lambda2 < 0")  }
  if (lambda3 >= 0) { stop("lambda3 >= 0")  }
  if (lambda4 >= 0) { stop("lambda4 >= 0")  }

  return(

    get_dist_mode_from_pdf(
      pdf = function(x) {
        return(
          dgl(
            x = x,
            lambda1 = lambda1,
            lambda2 = lambda2,
            lambda3 = lambda3,
            lambda4 = lambda4
          )
        )
      },
      search_range_start = search_range_start,
      search_range_end = search_range_end)
      )
}
