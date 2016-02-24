\name{names-methods}
\docType{methods}
\alias{names-methods}
\alias{names,ANY-method}
\alias{names,doeFactor-method}
\alias{names<-,doeFactor-method}
\alias{names,facDesign-method}
\alias{names<-,facDesign-method}
\alias{names,taguchiFactor-method}
\alias{names<-,taguchiFactor-method}
\alias{names,taguchiDesign-method}
\alias{names<-,taguchiDesign-method}
\alias{names,pbFactor-method}
\alias{names<-,pbFactor-method}
\alias{names,pbDesign-method}
\alias{names<-,pbDesign-method}
\alias{names,gageRR-method}
\alias{names,mixDesign-method}
\alias{names<-,mixDesign-method}
\title{Methods for function names in Package \code{base}}
\description{
Methods for function \code{names} in Package \code{base}.}


\usage{
\S4method{names}{facDesign}(x)
\S4method{names}{facDesign}(x) <- value
\S4method{names}{mixDesign}(x)
\S4method{names}{mixDesign}(x) <- value
\S4method{names}{doeFactor}(x)
\S4method{names}{doeFactor}(x) <- value
\S4method{names}{taguchiDesign}(x)
\S4method{names}{taguchiDesign}(x) <- value
\S4method{names}{taguchiFactor}(x)
\S4method{names}{taguchiFactor}(x) <- value
\S4method{names}{pbDesign}(x)
\S4method{names}{pbDesign}(x) <- value
\S4method{names}{pbFactor}(x)
\S4method{names}{pbFactor}(x) <- value

\S4method{names}{gageRR}(x)
}

\arguments{
\item{x}{object of class \code{\link{facDesign}}, \code{\link{mixDesign}}, \code{\link{taguchiDesign}}, \code{taguchiFactor}, \code{\link{pbDesign}}, \code{pbFactor}, \code{doeFactor} or \code{\link{gageRR}}.}
\item{value}{character vector.}
}

\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
ANY object.
}

\item{\code{signature(x = "doeFactor")}}{
a \code{doeFactor} object.
}

\item{\code{signature(x = "facDesign")}}{
a \code{\link{facDesign}} object.
}

\item{\code{signature(x = "mixDesign")}}{
a \code{\link{mixDesign}} object.
}

\item{\code{signature(x = "taguchiDesign")}}{
a \code{\link{taguchiDesign}} object.
}

\item{\code{signature(x = "taguchiFactor")}}{
a \code{taguchiFactor} object.
}

\item{\code{signature(x = "pbDesign")}}{
a \code{\link{pbDesign}} object.
}

\item{\code{signature(x = "pbFactor")}}{
a \code{pbFactor} object.
}

\item{\code{signature(x = "gageRR")}}{
a \code{\link{gageRR}} object.
}
}}

\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}

\seealso{
\url{http://www.r-qualitytools.org}
}