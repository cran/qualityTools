\name{factors-methods}
\docType{methods}
\alias{factors}
\alias{factors<-}
\alias{factors-methods}
\alias{factors,facDesign-method}
\alias{factors<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the \code{factors} for the factors in an object of class facDesign}

\usage{
\S4method{factors}{facDesign}(x)
\S4method{factors}{facDesign}(x) <- value
}
\arguments{
\item{x}{a `facDesign' object}
\item{value}{new factors (used internally)}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{factors} in an object of class facDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{factors}},\code{\link{factors}},\code{\link{types}}, \code{\link{response}}, \code{\link{facDesign}}}

\examples{
fdo = facDesign(k = 3)  #NA in response column
factors(fdo)
}