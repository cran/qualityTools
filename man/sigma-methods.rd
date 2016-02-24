\name{sigma-methods}
\docType{methods}
\alias{sigma}
\alias{sigma<-}
\alias{sigma-methods}
\alias{sigma,gageRR-method}
\alias{sigma<-,gageRR-method}
\title{Get and set methods}
\description{Get and set the \code{sigma} for an object of class \code{\link{gageRR}}.}

\usage{
\S4method{sigma}{gageRR}(x)
\S4method{sigma}{gageRR}(x) <- value
}
\arguments{
  \item{x}{object of class \code{\link{gageRR}}.}
  \item{value}{number of sigmas for calculation the Number of Distinct Categories (signal-to-noise-ratio)in a Gage R&R analysis.}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "gageRR")}}{
Get and set the \code{sigma} for an object of class \code{\link{gageRR}}.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{gageRRDesign}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
x = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3)
#default 6sigma
sigma(x) 
sigma(x) = 5.15
sigma(x)
}