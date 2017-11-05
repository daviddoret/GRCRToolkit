#' get_dist_type
#'
#' Informs us if a distribution is discrete or continuous.
#'
#' Current development:
#' As of today, the list of distributions supported by the function
#' is far from satisfactory. I just implemented a few names that I
#' immediately needed + a few that I grabbed from wikipedia below.
#'
#' Future enhancements:
#' - Provide support for all well-known distributions.
#' - Find a more elegant way to manage distribution aliases, shortened versions, etc.
#'
#' References:
#' - List of probability distributions, Wikipedia
#'   https://en.wikipedia.org/wiki/List_of_probability_distributions
#'
#' @param dist_name the shortened or complete name of the distribution.
#'
#' @return "discrete", "continuous", or an error if the distribution is unknown.
#'
#' @examples
#' get_dist_type(dist_name="norm")
#' get_dist_type(dist_name="pois")
#'
#' @export
get_dist_type <- function(dist_name, ...) {
  output <- switch(dist_name,
         benford_law = "discrete",
         bernoulli = "discrete",
         beta = "continuous",
         beta_binomial = "discrete",
         betapert = "continuous",
         binomial = "discrete",
         degenerate = "discrete",
         discrete_uniform = "discrete",
         fisher_noncentral_hypergeometric = "discrete",
         wallenius_noncentral_hypergeometric = "discrete",
         hypergeometric = "discrete",
         norm = "continuous",
         normal = "continuous",
         lnorm = "continuous",
         pois = "discrete",
         poisson = "discrete",
         poisson_binomial = "discrete",
         poissonpert = "discrete",
         rademacher = "discrete"
         )
  if (is_void(output)) {
    stop("Unknown distribution", dist_name) }
  else {
    return(output)
  }
}
