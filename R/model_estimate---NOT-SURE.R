library(R6)
#' model_estimate
#'
#' An abstract class that is used as a glueing interface between
#' the model_analysis + model_factor data model on one hand,
#' and an extensible set of estimation methods or approaches.
#'
#' @param name the unique name used to identify this factor within its model
#'
#' @param dist the theoretical probability distribution of this factor
#'
#' @return an instance of a mode factor
#'
#' @examples
#' f1 <- model_factor$new(name = "x", dist = "lnorm")
#'
#' @export
model_estimate <- R6Class(
  "model_estimate",
  public = list(
    initialize = function() {
      self$estimation_distribution_name <- NULL
      self$estimation_parameters <- list()
      self$estimation_probabilities <- c()
      self$estimation_quantiles <- c()
      self$estimation_labels <- c()
    }),
  active = list(
      estimation_distribution_name = function(value,...) {
        if(missing(value)) {
          return(private$private_estimation_distribution_name) }
        else {
          private$private_estimation_distribution_name <- value }},
      estimation_parameters = function(value,...) {
        if(missing(value)) {
          return(private$private_estimation_parameters) }
        else {
          private$private_estimation_parameters <- value }
      },
      estimation_probabilities = function(value,...) {
        if(missing(value)) {
          return(private$private_estimation_probabilities) }
        else {
          private$private_estimation_probabilities <- value }
      },
      estimation_quantiles = function(value,...) {
        if(missing(value)) {
          return(private$private_estimation_quantiles) }
        else {
          private$private_estimation_quantiles <- value }
      },
      estimation_labels = function(value,...) {
        if(missing(value)) {
          return(private$private_estimation_labels) }
        else {
          private$private_estimation_labels <- value }
      }
  ),
  private = list(
    private_estimation_distribution_name = NULL,
    private_estimation_parameters = NULL,
    private_estimation_probabilities = NULL,
    private_estimation_quantiles = NULL,
    private_estimation_labels = NULL
    ))
