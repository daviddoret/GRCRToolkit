if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

# get_dist_mode_from_pdf
#
# Find the mode of a distribution using optimization techniques.
# This is helpful when an analytic solution is not handily available.
get_dist_mode_from_pdf = function(pdf, precision = NULL, verbosity = NULL, ...) {

  if(is.null(precision)) { precision <- 1 }
  if(is.null(verbosity)) { verbosity <- 0 }

#  # Declare the minimization function
#  minimization_function <- function(x){
#    result <- pdf(x)
#    #if(result == 0) { return(Inf)  }
#    return(result)
#  }
#
#  # Run the optimization
#  optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128, print.level = verbosity)

  optimization <- optimize(pdf, c(-10000,10000), maximum = TRUE, tol = 1)

  if(is.null(optimization$maximum))
  {
    warning("optimize() failed miserably...")
  }
  #print(optimization)

  return(optimization$maximum)
}
