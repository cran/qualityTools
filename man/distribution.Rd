\name{distribution}
\alias{distribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distribution
}
\description{
calculates the most likely parameters for a given distribution
}
\usage{
distribution(x, distribution = "weibull", start, ...)
}

\arguments{
  \item{x}{
vector
}
  \item{distribution}{
one of "beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "logistic", "negative binomial", "normal", "poisson", "t","weibull"
}
  \item{start}{
start values if needed
}
  \item{\dots}{
...
}
}
\value{
object of class distrCollection
}
\references{
MASS Package
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\note{
This is just a wrapper for the fitdistr function of the MASS library.
}

