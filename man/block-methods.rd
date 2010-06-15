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
\item{object}{a `facDesign' object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the Blocking structure for a 'facDesign' object 
}
}}

\note{\code{block} is to be rewritten}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{response}}, \code{\link{facDesign}}}
\examples{
fdo = facDesign(k = 3)  #NA in response column
block(fdo)
}                                              
