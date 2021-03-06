% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_function_1param_value_divideby2.R
\name{find_function_1param_value_divideby2}
\alias{find_function_1param_value_divideby2}
\title{find_function_1param_value_divideby2}
\usage{
find_function_1param_value_divideby2(f, y_target_value, x_first_guess = NULL,
  x_first_step = NULL, x_search_limit_min = NULL,
  x_search_limit_max = NULL, y_precision = NULL, max_iteration = NULL,
  verbosity = NULL, ...)
}
\arguments{
\item{f}{A function receiving a numeric value as its first parameter followed by ...}

\item{y_target_value}{The output of \code{f(x)} for which we want to find \code{x}.}

\item{x_first_guess}{(Conditional) An initial best-effort first guess of what \code{x} could be.}

\item{x_first_step}{(Conditional) An initial step length to find better better values.}

\item{x_search_limit_min}{(Conditional) A strict limit below which \code{x} will not be searched.}

\item{x_search_limit_max}{(Conditional) A strict limit above which \code{x} will not be searched.}

\item{y_precision}{(Conditional) An acceptable degree of precision. Default: .01.}

\item{max_iteration}{(Conditional) The maximum number of search loops performed by the function. This is a safeguard to avoid infinite loops. Default: 2048.}

\item{verbosity}{(Conditional) When \code{verbosity > 0} detailed information will be output to the console.}
}
\value{
Numeric vector. \code{x} such that \code{f(x) = y_target_value}.
}
\description{
Given \code{f(x) = y} where \code{f} is a monotonic function, find \code{x} for a given \code{y}.
\cr Use a naive iterative approach dividing and mulitplying steps by 2 until a solution is found within desired precision.
\cr Non-monotic functions are supported when the search range is limited to a monotonic subset of the function's range.
\cr WARNING: This algorithm is naive, i.e. performance-wise suboptimal.
}
\examples{
precision <- .0001
y_target <- runif(n = 1, min = -1000000, max = +1000000)
c <- runif(n = 1, min = -1000000, max = +1000000)
m <- runif(n = 1, min = -1000000, max = +1000000)
f <- function(x) { return(c + m * x) }
x_found <- find_function_1param_value_divideby2(
  f = f,
  y_target_value = y_target,
  y_precision = precision,
  verbosity = verbosity)
y_found <- f(x_found)
delta <- abs(y_target - y_found)
label <- paste0(
  "f(x): c + m * x",
  "\\nc: ", fn(c,8),
  "\\nm: ", fn(m,8),
  "\\ny_target: ", fn(y_target,8),
  "\\nx_found: ", fn(x_found,8),
  "\\ny_found: ", fn(y_found,8),
  "\\ndelta: ", fn(delta,8))
message(label)

}

