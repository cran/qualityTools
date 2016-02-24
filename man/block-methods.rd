\name{block-methods}
\docType{methods}
\alias{block}
\alias{block<-}
\alias{block-methods}
\alias{block,facDesign-method}
\alias{block<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the Blocking structure for a 'facDesign' object }

\usage{
\S4method{block}{facDesign}(object)
\S4method{block}{facDesign}(object) <- value
}
\arguments{
\item{object}{a \code{facDesign} object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the Blocking structure for a \code{facDesign} object. 
}
}}

\note{\code{block} is to be rewritten}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{response}} \cr
\code{\link{facDesign}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
#NA in response column
fdo = facDesign(k = 3)    
block(fdo)
}                                              
