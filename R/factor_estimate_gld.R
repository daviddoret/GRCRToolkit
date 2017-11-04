require(R6)
require(gld)

#' An abstract class for a GLD-based factor estimates.
#' Subclasses may inherit from it to implement various estimation techniques,
#' such as the 3 points estimate which is going to be the first
#' implementation.
#'
#' For the time being, I only support the FMKH parameterization. If
#' other parameterizations become necessary in the future, these will
#' require specifically dedicated R6 classes because the way we tweak
#' lambda parameters here is strongly linked to FMKH logic.
#'
#' TODO:
#' - data validation and general help to friendly manage lambda parameters
#'
#' @export
factor_estimate_gld <- R6Class(
  "factor_estimate_gld",
  inherit = factor_estimate,
  public = list(
    initialize = function(
      limit_min_value = NULL,
      limit_min_behavior = NULL,
      limit_max_value = NULL,
      limit_max_behavior = NULL,
      ...) {
      super$initialize(
        distribution_name = "Generalized Lambda (aka Tukey Lambda)",
        distribution_type = "Continuous",
        limit_min_value = limit_min_value,
        limit_min_behavior = limit_min_behavior,
        limit_max_value = limit_max_value,
        limit_max_behavior = limit_max_behavior,
        ...)
      #super$distribution_name <- "GLD"
      self$density_function <- function(
        x, lambda1 = NULL, lambda2 = NULL, lambda3 = NULL, lambda4 = NULL, ...){
        return(
          dgl(
            x = x,
            lambda1 = ifelse(is.null(lambda1),self$lambda1, lambda1),
            lambda2 = ifelse(is.null(lambda2),self$lambda2, lambda2),
            lambda3 = ifelse(is.null(lambda3),self$lambda3, lambda3),
            lambda4 = ifelse(is.null(lambda4),self$lambda4, lambda4)))}
      self$probability_function <- function(
        q, lambda1 = NULL, lambda2 = NULL, lambda3 = NULL, lambda4 = NULL, ...){
        return(
          pgl(
            q = q,
            lambda1 = ifelse(is.null(lambda1),self$lambda1, lambda1),
            lambda2 = ifelse(is.null(lambda2),self$lambda2, lambda2),
            lambda3 = ifelse(is.null(lambda3),self$lambda3, lambda3),
            lambda4 = ifelse(is.null(lambda4),self$lambda4, lambda4)))}
      self$quantile_function <- function(
        p, lambda1 = NULL, lambda2 = NULL, lambda3 = NULL, lambda4 = NULL, ...){
        return(
          qgl(
            p = p,
            lambda1 = ifelse(is.null(lambda1),self$lambda1, lambda1),
            lambda2 = ifelse(is.null(lambda2),self$lambda2, lambda2),
            lambda3 = ifelse(is.null(lambda3),self$lambda3, lambda3),
            lambda4 = ifelse(is.null(lambda4),self$lambda4, lambda4)))}
      self$random_function <- function(
        n, lambda1 = NULL, lambda2 = NULL, lambda3 = NULL, lambda4 = NULL, ...){
        return(
          rgl(
            n = n,
            lambda1 = ifelse(is.null(lambda1),self$lambda1, lambda1),
            lambda2 = ifelse(is.null(lambda2),self$lambda2, lambda2),
            lambda3 = ifelse(is.null(lambda3),self$lambda3, lambda3),
            lambda4 = ifelse(is.null(lambda4),self$lambda4, lambda4)))}
    },
    check_state_consistency = function(output_format = NULL,...) {
      # Informs us if the object state is consistent / logical.
      # This makes it possible to prevent useless calls to expensive functions
      # that may output multitude of warnings and errors when we know
      # from the beginning that this parameterization is doomed to failure.
      # Returns TRUE if parameters are consistent.
      # Returns a descriptive
      if(is.null(output_format)) { output_format = "boolean" }
      consistency_error_count <- super$check_state_consistency(output_format = "int")
      consistency_report <- super$check_state_consistency(output_format = "report")

      # Check if all mandatory parameters have been defined.
      if (is_void(self$lambda1)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb1 is missing."), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }
      if (is_void(self$lambda2)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb2 is missing."), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }
      if (is_void(self$lambda3)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb3 is missing."), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }
      if (is_void(self$lambda4)) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb4 is missing."), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }

      # Check consistency between parameters.
      if (self$lambda3 >= 0) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb3 >= 0"), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }
      if (self$lambda4 >= 0) {
        consistency_error_count <- consistency_error_count + 1
        consistency_report <- paste0(c(consistency_report, "\U3bb4 >= 0"), sep = "\n") # Unicode 3bb = 	greek small letter lamda
      }

      # And eventually output the conclusion in the desired format.
      if (output_format == "boolean")
      {
        return(consistency_error_count == 0)
      }
      else if (output_format == "int")
      {
        return(consistency_error_count)
      }
      else if (output_format == "report")
      {
        return(consistency_report)
      }
      else
      {
        stop("Sorry, this output format is not supported.")
      }
    },
    get_print_lines = function(...) {
      return(
          c(super$get_print_lines(),
          "Fitted distribution parameters:",
          paste0(
            " \U3bb1 = ", fn(self$lambda1,4), # Unicode 3bb = 	greek small letter lamda
            " ,\U3bb2 = ", fn(self$lambda2,4),
            " ,\U3bb3 = ", fn(self$lambda3,4),
            " ,\U3bb4 = ", fn(self$lambda4,4)
            )))
    }
  ),
  active = list(
    # lambda1: location parameter, or α for the gpd parameterisation
    lambda1 = function(value,...) {
      if (missing(value)) { return(private$private_lambda1) }
      else {private$private_lambda1 <- value }},
    lambda2 = function(value,...) {
      if (missing(value)) { return(private$private_lambda2) }
      else {private$private_lambda2 <- value }},
    lambda3 = function(value,...) {
      if (missing(value)) { return(private$private_lambda3) }
      else {private$private_lambda3 <- value }},
    lambda4 = function(value,...) {
      if (missing(value)) { return(private$private_lambda4) }
      else {private$private_lambda4 <- value }}
  ),
  private = list(
    private_lambda1 = NULL,
    private_lambda2 = NULL,
    private_lambda3 = NULL,
    private_lambda4 = NULL
  )
)
