\name{factors-methods}
\docType{methods}
\alias{factors}
\alias{factors<-}
\alias{factors-methods}
\alias{factors,facDesign-method}
\alias{factors<-,facDesign-method}
\alias{factors,taguchiDesign-method}
\alias{factors<-,taguchiDesign-method}
\alias{factors,pbDesign-method}
\alias{factors<-,pbDesign-method}
\alias{factors,mixDesign-method}
\alias{factors<-,mixDesign-method}
\title{Get and set methods}
\description{Get and set the \code{factors} for the factors in an object of class \code{facDesign}, \code{mixDesign}, \code{taguchiDesign} or \code{pbDesign}}

\usage{
\S4method{factors}{facDesign}(x)
\S4method{factors}{facDesign}(x) <- value
\S4method{factors}{mixDesign}(x)
\S4method{factors}{mixDesign}(x) <- value
\S4method{factors}{taguchiDesign}(x)
\S4method{factors}{taguchiDesign}(x) <- value
\S4method{factors}{pbDesign}(x)
\S4method{factors}{pbDesign}(x) <- value

}
\arguments{
\item{x}{a \code{\link{facDesign}}, \code{\link{taguchiDesign}}, \code{\link{pbDesign}} or \code{\link{mixDesign}} object.}
\item{value}{new factors (used internally).}
}

\section{Methods}{
\describe{
\item{\code{signature(object = "facDesign")}}{
Get and set the \code{factors} in an object of class \code{\link{facDesign}}.
}
\item{\code{signature(object = "mixDesign")}}{
Get and set the \code{factors} in an object of class \code{\link{mixDesign}}.
}
\item{\code{signature(object = "taguchiDesign")}}{
Get and set the \code{factors} in an object of class \code{\link{taguchiDesign}}.
}
\item{\code{signature(object = "pbDesign")}}{
Get and set the \code{factors} in an object of class \code{\link{taguchiDesign}}.
}
}}
\author{
 Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
 Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
 }
\seealso{\code{\link{factors}}\cr
\code{\link{factors}}\cr
\code{\link{factors}}\cr
\code{\link{types}}\cr
\code{\link{response}}\cr
\code{\link{facDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
#NA in response column
fdo = facDesign(k = 3)  
factors(fdo)
}