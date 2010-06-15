\name{types-methods}
\docType{methods}
\alias{types}
\alias{types<-}
\alias{types-methods}
\alias{types,facDesign-method}
\alias{types<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the \code{types} for the factors in an object of class facDesign}

\usage{
\S4method{types}{facDesign}(x)
\S4method{types}{facDesign}(x) <- value
}
\arguments{
\item{x}{a `facDesign' object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the \code{types} for the factors in an object of class facDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{lows}},\code{\link{highs}},\code{\link{types}}}
\examples{
fdo = fracDesign(k = 3)  #NA in response column
summary(fdo)
types(fdo) = c("numeric","numeric", "factor" )
names(fdo) = c("Time", "Temperature", "Catalyst")
summary(fdo)
}