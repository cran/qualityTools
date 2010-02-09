\name{highs-methods}
\docType{methods}
\alias{highs}
\alias{highs<-}
\alias{highs-methods}
\alias{highs,facDesign-method}
\alias{highs<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the \code{highs} for the factors in an object of class facDesign}

\usage{
\S4method{highs}{facDesign}(object)
\S4method{highs}{facDesign}(object) <- value
}
\arguments{
\item{object}{a `facDesign' object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{highs} for the factors in an object of class facDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{lows}},\code{\link{highs}},\code{\link{types}}}
\examples{
fdo = facDesign(k=3)
lows(fdo) = c(10, 160, 1)
highs(fdo) = c(20, 200, 2)
summary(fdo)
}