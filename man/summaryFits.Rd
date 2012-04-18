\name{summaryFits}
\alias{summaryFits}
\title{
Fit Summary 
}
\description{
Function to give an overview of fitted linear models for objects of class \code{\link{facDesign}}.
}
\usage{
summaryFits(fdo, lmFit = TRUE, curvTest = TRUE, origFit = TRUE)
}
\arguments{
  \item{fdo}{
needs to be an object of class \code{\link{facDesign}}.
}
  \item{lmFit}{
logical value deciding whether the fits from the object fdo should be taken or not.\cr
By default \code{lmFit} is set to \sQuote{TRUE}.
}
  \item{curvTest}{
logical value deciding wheter curvature tests will be performed or not.\cr
By default \code{curvTest} is set to \sQuote{TRUE}.
}
  \item{origFit}{
logical value. If \sQuote{TRUE} (default) the original values of the fits will be displayed.
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
This is BETA!
}
\seealso{
\url{http://www.r-qualitytools.org}
}


