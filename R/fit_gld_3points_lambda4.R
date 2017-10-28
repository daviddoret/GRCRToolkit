require(gld)

#' fit_gld_3points_lambda4
#'
#' This function optimizes the lambda4 (shape) parameter of a GLD distribution to further fit a 3 points estimate.
#' \cr This is an "internal" function called by fit_gld_3points.
#'
#' @inheritParams fit_gld_3points
#'
#' @return A numeric vector of size 1 representing an optimized lambda4 (shape) parameter that further fits a 3 points estimate.
#'
#' @export
fit_gld_3points_lambda4 = function(
  lambda1,
  lambda2,
  lambda3,
  lambda4,
  estimated_range_max_value,
  estimated_range_max_proba,
  verbosity = NULL,
  ...) {

  if (is_nanull(verbosity)) { verbosity <- 0 }

  # pgl does not support vectors in the lambda4 parameter,
  # (which I must say is perfectly reasonable).
  # So I declare a flat scalar version and take this opportunity
  # to throw away positive values (nlm optimizer doesn't support bounds either).
  f <- function(x, ...){
    if (x >= 0) {
      #if (verbosity > 0) { warning("x >= 0") }
      return(Inf)
    }
    return(pgl(
      q = estimated_range_max_value,
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = lambda3,
      lambda4 = x))
  }

  new_lambda4 <- find_function_1param_value_divideby2(
    f = f,
    y_target_value = estimated_range_max_proba,
    x_first_guess = -1,
    x_first_step = .1,
    x_search_limit_max = 0,
    y_precision = .00000001,
    verbosity = verbosity - 1
    )

  # TODO: Add a quality check on the resulting output

  return(new_lambda4)

}
