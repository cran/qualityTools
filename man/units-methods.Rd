\name{units-methods}
\docType{methods}
\alias{units}
\alias{units<-}
\alias{units-methods}
\alias{units,facDesign-method}
\alias{units<-,facDesign-method}
\alias{units,mixDesign-method}
\alias{units<-,mixDesign-method}
\title{Get and set methods}
\description{Get and set the \code{units} for the factors in an object of class facDesign}

\usage{
\S4method{units}{facDesign}(x)
\S4method{units}{facDesign}(x) <- value
}
\arguments{
\item{x}{a `facDesign' or `mixDesign' object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the \code{units} for the factors in an object of class facDesign
}
\item{\code{signature(x = "mixDesign")}}{
Get and set the \code{units} for the factors in an object of class mixDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{lows}},\code{\link{highs}},\code{\link{types}}}
\examples{
fdo = fracDesign(k = 2)  #NA in response column
summary(fdo)
units(fdo) = c("min","C") 
names(fdo) = c("Time", "Temperature") 
summary(fdo)
}