\name{factors-methods}
\docType{methods}
\alias{factors}
\alias{factors<-}
\alias{factors-methods}
\alias{factors,facDesign-method}
\alias{factors<-,facDesign-method}
\alias{factors,taguchiDesign-method}
\alias{factors<-,taguchiDesign-method}
\alias{factors,mixDesign-method}
\alias{factors<-,mixDesign-method}
\title{Get and set methods}
\description{Get and set the \code{factors} for the factors in an object of class \code{facDesign}, \code{mixDesign} or \code{taguchiDesign}}

\usage{
\S4method{factors}{facDesign}(x)
\S4method{factors}{facDesign}(x) <- value
\S4method{factors}{mixDesign}(x)
\S4method{factors}{mixDesign}(x) <- value
\S4method{factors}{taguchiDesign}(x)
\S4method{factors}{taguchiDesign}(x) <- value

}
\arguments{
\item{x}{a `facDesign', 'taguchiDesign' or `mixDesign' object}
\item{value}{new factors (used internally)}
}

\section{Methods}{
\describe{
\item{\code{signature(object = "facDesign")}}{
Get and set the \code{factors} in an object of class facDesign
}
\item{\code{signature(object = "mixDesign")}}{
Get and set the \code{factors} in an object of class mixDesign
}
\item{\code{signature(object = "taguchiDesign")}}{
Get and set the \code{factors} in an object of class taguchiDesign
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{factors}},\code{\link{factors}},\code{\link{types}}, \code{\link{response}}, \code{\link{facDesign}}}

\examples{
fdo = facDesign(k = 3)  #NA in response column
factors(fdo)
}