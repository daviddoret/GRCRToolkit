library(R6)
#' model_estimate_gldpert
#'
#' The GLD-PERT estimation method is based on the PERT estimation style,
#' well adapted for subject matter expert estimates.
#' It is primarily based on 3 estimates points (quantiles)
#' and a range size.
#' The range size is defaulted to .9, leaving .05 on each side
#' of the distribution.
#' These parameters are then fitted to a GLD (Generalized Lambda Distribution)
#' distribution. GLD was chosen for its great flexibility, fitting estimated
#' points in most circumstances. In addition to this, many usual distributions
#' may be represented by a GLD given the proper lambda parameters.
#'
#' @param range_min the estimate of the low point of the estimated range
#'
#' @param typical the estimate of the "most probable" value within the range, i.e. the mode
#'
#' @param range_max the estimate of the high point of the estimated range
#'
#' @param range_size the width of the estimated range, > 0, < 1
#'
#' @return an instance of a GLD-PERT estimation
#'
#' @examples
#' mde <- model_estimate_gldpert$new(range_min = 2, typical = 5, range_max = 10, range_size = .9)
#'
#' @export
model_estimate_gldpert <- R6Class(
  "model_estimate_gldpert",
  inherit = model_estimate,
  public = list(
    initialize = function(
      range_min,
      typical,
      range_max,
      range_size) {

      super$initialize()

      # Default values
      if(is.null(range_size)) { range_size = model_config_get_option("model_estimate_gldpert", "range_size") }

      # Validation rules
      if(!is.numeric(range_min)) { stop("range_min must be numeric") }
      if(!is.numeric(typical)) { stop("typical must be numeric") }
      if(!is.numeric(range_max)) { stop("range_max must be numeric") }
      if(!is.numeric(range_size)) { stop("range_size must be numeric") }
      if(!0 < range_size) { stop("range_size must be strictly higher than 0") }
      if(!range_size < 1) { stop("range_size must be strictly lower than 1") }
      if(!range_min < typical) { stop("range_min must be strictly lower than typical") }
      if(!typical < range_max) { stop("typical must be strictly lower than range_max") }
      # QUESTION: Should we allow negative quantiles? My intuition says no, but do I really have strong arguments?

      self$range_min <- range_min
      self$typical <- typical
      self$range_max <- range_max
      self$range_size <- range_size

      super$estimation_distribution_name <- "gldpert"

      super$estimation_probabilities <- c(
        (1 - range_size) / 2,
        .5,
        1 - (1 - range_size) / 2)
      super$estimation_quantiles <- c(range_min, typical, range_max)
      super$estimation_labels <- c("Min.", "Typical", "Max.")

    },
    fit_distribution(){

    }),

    active = list(
      range_min = function(value,...) {
        if(missing(value)) {
          return(super$estimation_parameters[["range_min"]]) }
        else {
          super$estimation_parameters[["range_min"]] <- value }},
      typical = function(value,...) {
        if(missing(value)) {
          return(super$estimation_parameters[["typical"]]) }
        else {
          super$estimation_parameters[["typical"]] <- value }},
      range_max = function(value,...) {
        if(missing(value)) {
          return(super$estimation_parameters[["range_max"]]) }
        else {
          super$estimation_parameters[["range_max"]] <- value }},
      range_size = function(value,...) {
        if(missing(value)) {
          return(super$estimation_parameters[["range_size"]]) }
        else {
          super$estimation_parameters[["range_size"]] <- value }}
  ),
  private = list())

