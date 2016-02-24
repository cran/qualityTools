\name{values-methods}
\docType{methods}
\alias{values}
\alias{values<-}
\alias{values-methods}
\alias{values,taguchiDesign-method}
\alias{values<-,taguchiDesign-method}
\alias{values,pbDesign-method}
\alias{values<-,pbDesign-method}
\title{Get and Set methods}
\description{
Methods for function \code{values}
}

\usage{
\S4method{values}{taguchiDesign}(object)
\S4method{values}{taguchiDesign}(object) <- value
\S4method{values}{pbDesign}(object)
\S4method{values}{pbDesign}(object) <- value
}
\arguments{
\item{object}{a \code{\link{taguchiDesign}} or a \code{\link{pbDesign}} object}
\item{value}{a list of values}
}


\section{Methods}{
\describe{

\item{\code{signature(object = "taguchiDesign")}}{
Get and set the \code{values} for the factors in an object of class \code{\link{taguchiDesign}}.
}
\item{\code{signature(object = "pbDesign")}}{
Get and set the \code{values} for the factors in an object of class \code{\link{pbDesign}}.
}
}}
\keyword{methods}
\seealso{
\url{http://www.r-qualitytools.org}
}
