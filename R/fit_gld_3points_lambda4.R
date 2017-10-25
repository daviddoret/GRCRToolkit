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
  flat_function <- function(x, ...){
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

  # Then I declare a minimization function
  # that will vectorize the call to flat_function with vapply.
  minimization_function <- function(x, verbosity = NULL, ...){
    return(
      abs(
        vapply(x, flat_function, 0)
        -
          estimated_range_max_proba
      )
      # nlm prefers to reduce high numbers
      # so I artificially increase the output
      # of my minimization function by a factor
      # that guarantees precise enough results.
      * 1000000
      # * 1000
    )
  }

  # Then we run the optimization.
  # TODO: Study nlm options in greater details.
  optimization <- tryCatch({
    nlm(
      minimization_function,
      -1,
      ndigit = 16, # TODO: Move this to a central configuration mechanism
      iterlim = 128, # TODO: Move this to a central configuration mechanism
      print.level = 0) },
    warning = function(w) { if (verbosity > 0) { warning(w) }},
    error = function(e) { stop(e) },
    finally = {})

  # TODO: We should test the result against a tolerance threshold.
  #self$get_probability(self$estimated_range_max_value)
  #self$get_quantile(self$estimated_range_max_proba)

  new_lambda4 <- NA
  tryCatch({
      new_lambda4 <- optimization$estimate
      },
    warning = function(w){},
    error = function(e){},
    finally = {
      if (verbosity > 0) {
        message("Lambda4 optimization failed. Recycling previous value.")
      }
      new_lambda4 <- lambda4
    })

  if (verbosity > 0) { message(paste0("lambda4: ", new_lambda4)) }

  return(new_lambda4)

}
