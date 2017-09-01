if (!require(pacman)) install.packages(pacman)
pacman::p_load(R6,gld)

#' GLD 3 points estimate
#'
#' @export
factor_estimate_gld_3points <- R6Class(
  "factor_estimate_gld_3points",
  inherit = factor_estimate_gld,
  public = list(
    initialize = function(...) {
      super$initialize(
        estimation_method_name = "PERT-like 3 points estimate", ...)
    },
    fit_distribution = function(...) {

    },
    optimize_lambda1 = function(...) {
      # this parameter is obvious
      self$lambda1 <- mode
    },
    get_print = function(...) {
      return(paste0(super$get_print(),
                    "\nEstimation parameters:",
                    " min = ", self$min_value, " (", self$min_proba, ")",
                    " ,mode = ", self$mode_value, " (", self$mode_proba, ")",
                    " ,max = ", self$max_value, " (", self$max_proba, ")"))}
  ),
  active = list(
    min_value = function(value,...) {
      if(missing(value)) { return(private$private_min_value) }
      else { private$private_min_value <- value }},
    mode_value = function(value,...) {
      if(missing(value)) { return(private$private_mode_value) }
      else { private$private_mode_value <- value }},
    max_value = function(value,...) {
      if(missing(value)) { return(private$private_max_value) }
      else { private$private_max_value <- value }},
    min_proba = function(value,...) {
      if(missing(value)) { return(private$private_min_proba) }
      else { private$private_min_proba <- value }},
    mode_proba = function(value,...) {
      if(missing(value)) { return(private$private_mode_proba) }
      else { private$private_mode_proba <- value }},
    max_proba = function(value,...) {
      if(missing(value)) { return(private$private_max_proba) }
      else { private$private_max_proba <- value }},
    range_size_proba = function(value,...) {
      if(missing(value)) { return(self$max_proba - self$min_proba) }
      else {
        self$min_proba <- (1 - value) / 2
        self$max_proba <- 1 - (1 - value) / 2
        }}
  ),
  private = list(
    private_min_value = NULL,
    private_mode_value = NULL,
    private_max_value = NULL,
    private_min_proba = NULL,
    private_mode_proba = NULL,
    private_max_proba = NULL
  )
)
