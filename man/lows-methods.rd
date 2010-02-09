\name{lows-methods}
\docType{methods}
\alias{lows}
\alias{lows<-}
\alias{lows-methods}
\alias{lows,facDesign-method}
\alias{lows<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the \code{lows} for the factors in an object of class facDesign}

\usage{
\S4method{lows}{facDesign}(object)
\S4method{lows}{facDesign}(object) <- value
}
\arguments{
\item{object}{a `facDesign' object}
\item{value}{data.frame or vector}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{lows} for the factors in an object of class facDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{lows}},\code{\link{lows}},\code{\link{types}}}
\examples{
fdo = facDesign(k=3)
lows(fdo) = c(10, 160, 1)
lows(fdo) = c(20, 200, 2)
summary(fdo)
}