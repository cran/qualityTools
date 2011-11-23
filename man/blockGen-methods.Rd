\name{blockGen-methods}
\docType{methods}
\alias{blockGen}
\alias{blockGen<-}
\alias{blockGen-methods}
\alias{blockGen,facDesign-method}
\alias{blockGen<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the generators for blocking}

\usage{
\S4method{blockGen}{facDesign}(object)
\S4method{blockGen}{facDesign}(object) <- value
}
\arguments{
\item{object}{a \code{\link{facDesign}} object}
\item{value}{data.frame or vector}
}

\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the generators for blocking.
}
}}

\note{\code{blockGen} is to be rewritten}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{response}} \cr
\code{\link{facDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
