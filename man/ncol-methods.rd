\name{ncol-methods}
\docType{methods}
\alias{ncol}
\alias{ncol-methods}
\alias{ncol,facDesign-method}
\title{Get method}
\description{Get the number of columns for a \code{\link{facDesign}} object}

\usage{
\S4method{ncol}{facDesign}(x)
}
\arguments{
\item{x}{a \code{\link{facDesign}} object}
}

\value{numeric - number of columns}

\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get the number of columns for a \code{\link{facDesign}} object.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{nrow}}\cr
\url{http://www.r-qualitytools.org}
}
