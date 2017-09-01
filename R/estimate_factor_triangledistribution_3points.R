#' estimate_factor_triangle_3points
#'
#' Estimate a model factor,
#' using a triangle probability distribution,
#' and 3 quantiles input points.
#'
#' @param model_factor the target model factor being estimated
#'
#' @param min the lowest value in the estimated range
#'
#' @param mode the peak value in the estimate range, must be > min and < max
#'
#' @param max the highest value in the estimated range
#'
#' @return this function modifies and returns the model factor once estimated
#'
#' @examples
#'
estimate_factor_triangle_3points <- function(
  model_factor,
  min,
  mode,
  max,
  range = NULL, ...) {
  if(is.null(range)) {
    range <- model_config_get_option("triangle_3points", "range")
  }
  if(min >= mode) { stop("min must be < than mode") }
  if(mode >= max) { stop("mode must be < than max") }



}
