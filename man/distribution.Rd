\name{distribution}
\alias{distribution}
\keyword{Distribution Identification}
\title{
Distribution
}
\description{
calculates the most likely parameters for a given distribution.
}
\usage{
distribution(x, distribution = "weibull", start, ...)
}

\arguments{
  \item{x}{
vector of distributed values from which the parameter should be determined.
}
  \item{distribution}{
character string specifying the distribution of x. The function \code{distribution} will accept the following character strings for \code{distribution}:
  \itemize{
   \item \dQuote{normal}
   \item \dQuote{chi-squared}
   \item \dQuote{exponential}
   \item \dQuote{logistic}
   \item \dQuote{gamma}
   \item \dQuote{weibull}
   \item \dQuote{cauchy}
   \item \dQuote{beta}
   \item \dQuote{f}
   \item \dQuote{t}
   \item \dQuote{geometric}
   \item \dQuote{poisson}
   \item \dQuote{negative binomial}
   \item \dQuote{log-normal}
}
By default \code{distribution} is set to \dQuote{weibull}.
}
  \item{start}{
start values if needed.
}
  \item{\dots}{
...
}
}
\value{
\code{distribution()} returns an object of class \code{distrCollection}.
}
\references{
MASS Package.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\note{
This is just a wrapper for the fitdistr function of the MASS library!

}

