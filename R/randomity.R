#if (!require(pacman)) install.packages(pacman)
#pacman::p_load(R6,random)
require(R6)
require(random)

#' randomity
#'
#' A helper class that generates pseudo random numbers with high entropy. Do not use this class directly: instead, use the global object \code{rand} that is already instanciated.
#'
#' @docType class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with properties and methods for pseudo random number generation.
#' @format \code{\link{R6Class}} object.
#' @examples
#' test <- randomity$new()
#' test$get(n = 32, min = 0, max = 10)
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Create a new object of this class.}
#'   \item{\code{get(n,min,max,verbosity)}}{Generate \code{n} pseudo random numbers with values between \code{min} and \code{max}.}
#' }
#' @section Future enhancements:
#' \describe{
#'   \item{- Add entropy from local system time}{}
#'   \item{- Add entropy from local system information}{}
#' }
randomity <- R6Class(
  "randomity",
  public = list(
    initialize = function() {
      #self$reseed()
    },
    get = function(n = NULL, min = NULL, max = NULL, verbosity = NULL) {
      if (is.null(n)) { n <- 1 }
      if (is.null(min)) { min <- 0 }
      if (is.null(max)) { max <- 1 }
      if (is.null(verbosity)) { verbosity <- 0 }

      if (runif(n = 1, min = 0, max = 1) > .9 | private$private_first_call) {
        # Because calls to external services such as randomNumbers() are slow,
        # every call to the function I roll a dice and reseed from time to time.
        self$reseed(verbosity = verbosity)
        private$private_first_call <- FALSE
      }

      return(
        runif(n = n, min = min, max = max)
      )

    },
    reseed = function(verbosity = NULL) {

      if (is.null(verbosity)) { verbosity <- 0 }

      new_seed <-
        c(randomNumbers(n = 1, min = 0, max = 100000000, col = 1, base = 10, check = FALSE)) +
        runif(n = 1, min = 0, max = 100000000)

      if (verbosity > 0) {
        print(message("New random seed: ", new_seed))
      }

      set.seed(new_seed)
    }
  ),
  private = list(
    private_first_call = TRUE
  )
)

#' rand
#'
#' A helper object to easily get pseudo random numbers with high entropy.
#' From time to time, you will observe that calls to this object's methods
#' take a bit more time than usual. This is due to the reseeding mechanism
#' that relies on external entropy.
#' This "global" object is an instance of the randomity class.
#' Type: help(randomity) for more information.
#'
#' @examples
#' print("Get a single pseudo random number between 0 and 1")
#' rand$get()
#' print("Get a dozen pseudo random numbers between -100 and 100")
#' rand$get(n = 12,min = -100,max = 100)
#' @export
rand <- randomity$new()
