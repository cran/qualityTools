\name{units-methods}
\docType{methods}
\alias{units}
\alias{units<-}
\alias{units-methods}
\alias{units,facDesign-method}
\alias{units<-,facDesign-method}
\alias{units,mixDesign-method}
\alias{units<-,mixDesign-method}
\alias{units,taguchiDesign-method}
\alias{units<-,taguchiDesign-method}
\alias{units,pbDesign-method}
\alias{units<-,pbDesign-method}
\title{Get and set methods}
\description{Get and set the \code{units} for the factors in an object of class \code{\link{facDesign}}, etc.}

\usage{
\S4method{units}{facDesign}(x)
\S4method{units}{facDesign}(x) <- value
}
\arguments{
\item{x}{a \code{\link{facDesign}} or \code{\link{mixDesign}} object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the \code{units} for the factors in an object of class \code{\link{facDesign}}.
}
\item{\code{signature(x = "mixDesign")}}{
Get and set the \code{units} for the factors in an object of class \code{\link{mixDesign}}.
}
\item{\code{signature(x = "taguchiDesign")}}{
Get and set the \code{units} for the factors in an object of class \code{\link{taguchiDesign}}.
}
\item{\code{signature(x = "pbDesign")}}{
Get and set the \code{units} for the factors in an object of class \code{\link{taguchiDesign}}.
}
}}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}
\seealso{
 \code{\link{factors}}\cr
 \code{\link{lows}}\cr
 \code{\link{highs}}\cr
 \code{\link{types}}\cr
 \url{http://www.r-qualitytools.org}
 }
\examples{
#NA in response column
fdo = fracDesign(k = 2)  
summary(fdo)
units(fdo) = c("min","C") 
names(fdo) = c("Time", "Temperature") 
summary(fdo)
}
