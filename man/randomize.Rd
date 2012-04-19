\name{randomize}
\Rdversion{1.1}
\alias{randomize}
\title{
Randomization
}
\description{
function to do randomize the run order of factorial designs.
}
\usage{
randomize(fdo, random.seed, so = FALSE)
}
\arguments{
  \item{fdo}{
needs to be an object of class \code{\link{facDesign}}.
}
  \item{random.seed}{
seed for randomness. See \code{\link{set.seed}}.
}
  \item{so}{
logical value specifying whether the standard order should be used or not.\cr
By default \code{so} is set to \sQuote{FALSE}.
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\seealso{
 \url{http://www.r-qualitytools.org}
}
