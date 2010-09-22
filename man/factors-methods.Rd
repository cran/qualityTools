\name{factors-methods}
\docType{methods}
\alias{factors}
\alias{factors<-}
\alias{factors-methods}
\alias{factors,facDesign-method}
\alias{factors<-,facDesign-method}
\alias{factors,mixDesign-method}
\alias{factors<-,mixDesign-method}
\title{Get and set methods}
\description{Get and set the \code{factors} for the factors in an object of class facDesign or mixDesign}

\usage{
\S4method{factors}{facDesign}(x)
\S4method{factors}{facDesign}(x) <- value
\S4method{factors}{mixDesign}(x)
\S4method{factors}{mixDesign}(x) <- value

}
\arguments{
\item{x}{a `facDesign' or `mixDesign' object}
\item{value}{new factors (used internally)}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{factors} in an object of class facDesign
}
\item{\code{signature(objectc = "mixDesign")}}{
Get and set the \code{factors} in an object of class mixDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{factors}},\code{\link{factors}},\code{\link{types}}, \code{\link{response}}, \code{\link{facDesign}}}

\examples{
fdo = facDesign(k = 3)  #NA in response column
factors(fdo)
}