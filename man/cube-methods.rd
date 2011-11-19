\name{cube-methods}
\docType{methods}
\alias{cube}
\alias{cube<-}
\alias{cube-methods}
\alias{cube,facDesign-method}
\alias{cube<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the \code{cube} portion for an object of class facDesign}

\usage{
\S4method{cube}{facDesign}(x)
\S4method{cube}{facDesign}(x) <- value
}
\arguments{
\item{x}{a \code{facDesign} object}
\item{value}{data.frame or vector}
}
\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{cube} for the factors in an object of class \code{\link{facDesign}}. So far used internally.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{facDesign}}\cr
\code{\link{fracDesign}}\cr
\code{\link{rsmDesign}}\cr
\code{\link{star}}\cr
\url{http://www.r-qualitytools.org}

}
\examples{
rsdo = rsmDesign(k = 3, alpha = 2)
cube(rsdo)
}