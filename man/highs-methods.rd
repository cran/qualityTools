\name{highs-methods}
\docType{methods}
\alias{highs}
\alias{highs<-}
\alias{highs-methods}
\alias{highs,facDesign-method}
\alias{highs<-,facDesign-method}
\alias{highs,mixDesign-method}
\alias{highs<-,mixDesign-method}
\title{Get and set methods}
\description{Get and set the \code{highs} for the factors in an object of class \code{\link{facDesign}} and \code{\link{mixDesign}}.} 

\usage{
\S4method{highs}{facDesign}(object)
\S4method{highs}{facDesign}(object) <- value
\S4method{highs}{mixDesign}(object)
\S4method{highs}{mixDesign}(object) <- value

}
\arguments{
\item{object}{a \code{facDesign} or \code{\link{mixDesign}} object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{highs} for the factors in an object of class \code{\link{facDesign}}.
}

\item{\code{signature(object = "mixDesign")}}{
Get and set the \code{highs} for the factors in an object of class \code{\link{mixDesign}}.
}

}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{factors}}\cr
\code{\link{lows}}\cr
\code{\link{highs}}\cr
\code{\link{types}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
fdo = facDesign(k=3)
lows(fdo) = c(10, 160, 1)
highs(fdo) = c(20, 200, 2)
summary(fdo)
}


