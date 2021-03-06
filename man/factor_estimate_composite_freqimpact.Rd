% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_estimate_composite_freqimpact.R
\docType{class}
\name{factor_estimate_composite_freqimpact}
\alias{factor_estimate_composite_freqimpact}
\title{factor_estimate_composite_freqimpact}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
factor_estimate_composite_freqimpact
}
\value{
An instance of the factor_estimate_composite \code{\link{R6Class}}.
}
\description{
This class represents a risk model "composite factor estimate based on frequency x impact".\cr
This estimation method that is based on:\cr
 \itemize{
  \item a \code{frequency_factor} modeling the number of times the anticipated event will happen per period of time,
  \item an \code{impact_factor} modeling the impact (size, severity) of realized events.
  }
The impact per period of time is given by \deqn{\sum{i=1}^frequency impact}.\cr
}
\section{Inherits}{

\describe{
  \item{\code{\link{factor_estimate}}}{}
}
}
\examples{
fe1 <- factor_estimate_composite_custom$new()
}
\keyword{data}

