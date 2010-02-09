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
\item{x}{a `facDesign' object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{cube} for the factors in an object of class facDesign. So far used internally.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{facDesign}},
\code{\link{fracDesign}},
\code{\link{rsmDesign}},
\code{\link{star}}
}
\examples{
rsdo = rsmDesign(k = 3, alpha = 2)
cube(rsdo)
}