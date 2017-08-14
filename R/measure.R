library(R6)

#' measure
#'
#' Represent a real-value accompanied with a unit of measurement.
#'
#' @param value the real value of the measure
#'
#' @param unit the unit of measurement
#'
#' @examples
#' m1 <- measure$new(name = 100, unit = "year")
#'
#' @export
measure <- R6Class("measure",
                   public = list(
                     initialize = function(value = NULL, unit = NULL, ...) {
                       self$value <- value
                       self$unit <- unit },
                     convert_to = function(unit, ...) {

                     }),
                     active = list(
                       value = function(value,...) {
                         if(missing(value)) {
                           return(private$private_value) }
                         else {
                           private$private_value <- value }},
                       unit = function(unit,...) {
                         if(missing(unit)) {
                           return(private$private_unit) }
                         else {
                           private$private_unit <- unit }}),
                       private = list(
                         private_value = NULL,
                         private_unit = NULL
                       ))




