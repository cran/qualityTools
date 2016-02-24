\name{lows-methods}
\docType{methods}
\alias{lows}
\alias{lows<-}
\alias{lows-methods}
\alias{lows,facDesign-method}
\alias{lows<-,facDesign-method}
\alias{lows,mixDesign-method}
\alias{lows<-,mixDesign-method}
\title{Get and set methods}
\description{Get and set the \code{lows} for the factors in an object of class \code{\link{facDesign}}.}

\usage{
\S4method{lows}{facDesign}(object)
\S4method{lows}{facDesign}(object) <- value
\S4method{lows}{mixDesign}(object)
\S4method{lows}{mixDesign}(object) <- value

}
\arguments{
\item{object}{a \code{\link{facDesign}}  or \code{\link{mixDesign}} object.}
\item{value}{data.frame or vector.}
}

\section{Methods}{
\describe{
\item{\code{signature(object = "facDesign")}}{
Get and set the \code{lows} for the factors in an object of class \code{\link{facDesign}}.
}
\item{\code{signature(object = "mixDesign")}}{
Get and set the \code{lows} for the factors in an object of class \code{\link{mixDesign}}.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}\cr
\code{\link{lows}}\cr
\code{\link{lows}}\cr
\code{\link{types}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
fdo = facDesign(k=3)
lows(fdo) = c(10, 160, 1)
lows(fdo) = c(20, 200, 2)
summary(fdo)
}