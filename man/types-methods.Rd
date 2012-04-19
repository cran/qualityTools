\name{types-methods}
\docType{methods}
\alias{types}
\alias{types<-}
\alias{types-methods}
\alias{types,facDesign-method}
\alias{types<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the \code{types} for the factors in an object of class \code{\link{facDesign}}.}

\usage{
\S4method{types}{facDesign}(x)
\S4method{types}{facDesign}(x) <- value
}
\arguments{
\item{x}{a \code{\link{facDesign}} object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the \code{types} for the factors in an object of class \code{\link{facDesign}}.
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
#NA in response column
fdo = fracDesign(k = 3)  
summary(fdo)
types(fdo) = c("numeric","numeric", "factor" )
names(fdo) = c("Time", "Temperature", "Catalyst")
summary(fdo)
}