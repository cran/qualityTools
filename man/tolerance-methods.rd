\name{tolerance-methods}
\docType{methods}
\alias{tolerance}
\alias{tolerance<-}
\alias{tolerance-methods}
\alias{tolerance,gageRR-method}
\alias{tolerance<-,gageRR-method}
\title{Get and set methods}
\description{Get and set the \code{tolerance} for an object of class \code{\link{gageRR}}.}

\usage{
\S4method{tolerance}{gageRR}(x)
\S4method{tolerance}{gageRR}(x) <- value
}
\arguments{
\item{x}{a `gageRR' object}
\item{value}{data.frame or vector}
}



\section{Methods}{
\describe{
\item{\code{signature(objectc = "gageRR")}}{
Get and set the \code{tolerance} for an object of class \code{\link{gageRR}}.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\seealso{
\code{\link{gageRRDesign}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
x = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3)
#default 6tolerance
tolerance(x)   
#100 units                                               
tolerance(x) = 100                                            
tolerance(x)
}