if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6)

#' model_factor
#'
#' A risk model factor.
#'
#' @docType class
#' @export
#' @keywords data
#' @return An instance of the \code{model_factor} \code{\link{R6Class}}.
#' @examples
#' f1 <- model_factor$new()
#' @section Inherits:
#' \describe{
#'   \item{This is a root class.}{}
#' }
model_factor <- R6Class(
  "model_factor",
  public = list(
       initialize = function(
         name = NULL,
         factor_estimate = NULL,
         ...) {
         if (is.null(name)) { name <- "Anonymous factor"}
         self$name <- name
         self$factor_estimate <- factor_estimate
       },
       print = function(...) {
         cat("Factor:", self$name, sep = "")
         invisible(self)
       },
      get_random = function(...) {
        if (is_nanull(self$factor_estimate)) {
          warning("Factor estimate is not available") }
        else {
          return(self$factor_estimate$get_random(...))}
      },
      get_density = function(...) {
        if (is_nanull(self$factor_estimate)) {
          warning("Factor estimate is not available") }
        else {
          return(self$factor_estimate$get_density(...))}
      }
  ),
     active = list(
       factor_estimate = function(value, ...) {
         if (missing(value)) {
           return(private$private_factor_estimate)
         }
         else {
           private$private_factor_estimate <- value
         }
       },
       name = function(value, ...) {
         if (missing(value)) {
           return(private$private_name)
         }
         else {
           private$private_name <- value
         }
       }
     ),
     private = list(
       private_factor_estimate = NA,
       private_name = NA
     )
)
