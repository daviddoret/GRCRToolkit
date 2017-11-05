require(R6)

#' measure
#'
#' Represent a real-value accompanied with a unit of measurement.
#' \cr In future versions, this class may be extended to provide support for unit conversions.
#'
#' @param value The real value of the measure (i.e. the number).
#'
#' @param unit The unit of measurement.
#'
#' @examples
#' m1 <- measure$new(value = 100, unit = "year")
#'
#' @export
measure <- R6Class(
  "measure",
  public = list(
   initialize = function(
     value = NULL,
     unit = NULL,
     ...) {
     if (is_void(value)) { value <- NA }
     if (is_void(unit)) { value <- NA }
     self$value <- value
     self$unit <- unit },
   convert_to = function(unit, ...) {
      stop("To be implemented")
   }),
   active = list(
     value = function(value,...) {
       if (missing(value)) {
         return(private$private_value) }
       else {
         if (is_void(value)) { value <- NA }
         private$private_value <- value }},
     unit = function(unit,...) {
       if (missing(unit)) {
         return(private$private_unit) }
       else {
         if (is_void(unit)) { unit <- NA }
         private$private_unit <- unit }}),
     private = list(
       private_value = NA,
       private_unit = NA
     ))




