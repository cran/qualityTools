\name{star-methods}
\docType{methods}
\alias{star}
\alias{star<-}
\alias{star-methods}
\alias{star,facDesign-method}
\alias{star<-,facDesign-method}
\title{Get and set methods}
\description{function to set and get the star portion of a response surface design}

\usage{
\S4method{star}{facDesign}(x)
\S4method{star}{facDesign}(x) <- value
}
\arguments{
\item{x}{a `facDesign' object}
\item{value}{\code{\link{data.frame}} holding the star portion.}
}

\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Get and set the \code{star} for the factors in an object of class \code{\link{facDesign}}.\cr
Could be used to exchange a default star portion with a specific one. So far used internally.
}
}}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\seealso{
\code{\link{rsmDesign}}\cr
\code{\link{rsmChoose}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
#rotatable response surface design with 3 factors
design = rsmDesign(k = 3) 
#star portion of the design
star(design)  
}
