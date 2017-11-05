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
#' @section Methods:
#' \describe{
#'   \item{get_random(n = 1, output_class = "vector")}{ Returns a random sample of size \code{n}. Returns a vector by default. If \code{output_class} = "data.frame", returns a data.frame with a column "factor_value". This second parameterization may be enriched by R6 subclasses to provide additional columns with complementary information. }
#' }
model_factor <- R6Class(
  "model_factor",
  public = list(
       initialize = function(
         name = NULL,
         factor_estimate = NULL,
         ...) {
         if (is_void(name)) { name <- "Anonymous factor"}
         self$name <- name
         self$factor_estimate <- factor_estimate
       },
       print = function(...) {
         cat("Factor:", self$name, sep = "")
         invisible(self)
       },
      get_random = function(n = NULL, output_class = NULL, ...) {
        if (is_void(n)) { n <- 1 }
        if (is_void(output_class)) { output_class <- "vector" }
        if (is_void(self$factor_estimate)) {
          warning("Factor estimate is not available") }
        else {
          return(self$factor_estimate$get_random(n = n, output_class = output_class, ...))
          }
      },
      get_density = function(...) {
        if (is_void(self$factor_estimate)) {
          warning("Factor estimate is not available") }
        else {
          return(self$factor_estimate$get_density(...))}
      },
      plot_simulation_sample = function(
        title = NULL,
        subtitle = NULL,
        caption = NULL,
        x_start = NULL,
        x_end = NULL,
        bins = NULL,
        n = NULL,
        x_scale_type = NULL,
        y_scale_type = NULL)
      {
        if (is_void(title)) { title <- self$name }
        if (is_void(title)) { subtitle <- "Simulation sample histogram" }

        self$factor_estimate$plot_simulation_sample(
          title = title,
          subtitle = subtitle,
          caption = caption,
          x_start = x_start,
          x_end = x_end,
          bins = bins,
          n = n,
          x_scale_type = x_scale_type,
          y_scale_type = y_scale_type)
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
