if (!require(pacman)) install.packages(pacman)
pacman::p_load(stats)

# get_mode_from_pdf
#
# Find the mode of a distribution using optimization techniques.
# This is helpful when an analytic solution is not handily available.
get_mode_from_pdf = function(pdf) {

  # Declare the minimization function
  minimization_function <- function(x){
    if(x == 0) { return(Inf)  }
    return(1 / pdf(x))
  }

  # Run the optimization
  optimization <- nlm(minimization_function, -1, ndigit = 22, iterlim = 128)

  #print(optimization)

  return(optimization$estimate)
}
