require(gld)

#' fit_gld_3points_lambda3
#'
#' This function optimizes the lambda3 (shape) parameter of a GLD distribution to further fit a 3 points estimate.
#' \cr This is an "internal" function called by fit_gld_3points.
#'
#' @inheritParams fit_gld_3points
#'
#' @return A numeric vector of size 1 representing an optimized lambda3 (shape) parameter that further fits a 3 points estimate.
#'
#' @export
fit_gld_3points_lambda3 = function(
  lambda1,
  lambda2,
  lambda3,
  lambda4,
  estimated_range_min_value,
  estimated_range_min_proba,
  verbosity = NULL,
  ...) {

  if (is.null(verbosity)) { verbosity <- 0 }

  # pgl does not support vectors in the lambda3 parameter,
  # (which I must say is perfectly reasonable).
  # So I declare a flat scalar version and take this opportunity
  # to throw away positive values (nlm optimizer doesn't support bounds either).
  flat_function <- function(x){
    if (x >= 0)
    {
      #if (verbosity > 0) { warning("x >= 0") }
      return(Inf)
    }
    return(pgl(
      q = estimated_range_min_value,
      lambda1 = lambda1,
      lambda2 = lambda2,
      lambda3 = x,
      lambda4 = lambda4))
  }

  # Then I declare a minimization function
  # that will vectorize the call to flat_function with vapply.
  minimization_function <- function(x, ...){
    return(
      abs(
        vapply(x, flat_function, 0)
        -
          estimated_range_min_proba
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
      print.level = 0)},
  warning = function(w) { if (verbosity > 0) { warning(w) } },
  error = function(e) { stop(e) },
  finally = {})

  # TODO: We should test the result against a tolerance threshold.
  #self$get_probability(self$estimated_range_max_value)
  #self$get_quantile(self$estimated_range_max_proba)

  new_lambda3 <- NA
  tryCatch( {
    new_lambda3 <- optimization$estimate },
  warning = function(w){},
  error = function(e){},
  finally = {
    if (is_nanull(new_lambda3)) {
      if (verbosity > 0) {
        message("Lambda3 optimization failed. Recycling previous value.")
      }
      new_lambda3 <- lambda3
      }
  })

  if (verbosity > 0) { message(paste0("lambda3: ", new_lambda3)) }

  return(new_lambda3)

}
