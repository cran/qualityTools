\name{response-methods}
\docType{methods}
\alias{response}
\alias{response<-}
\alias{response,facDesign-method}
\alias{response<-,facDesign-method}
\alias{response,mixDesign-method}
\alias{response<-,mixDesign-method}
\alias{response,taguchiDesign-method}
\alias{response<-,taguchiDesign-method}
\alias{response,steepAscent-method}
\alias{response<-,steepAscent-method}
\alias{response,gageRR-method}
\alias{response<-,gageRR-method}
\alias{response,MSALinearity-method}
\alias{response<-,MSALinearity-method}

\title{Get and set methods}
\description{Set or get the response for a \code{\link{facDesign}}, \code{\link{mixDesign}}, \code{\link{gageRRDesign}} or \code{\link{taguchiDesign}} object.}

\usage{
\S4method{response}{facDesign}(object)
\S4method{response}{facDesign}(object) <- value
\S4method{response}{mixDesign}(object)
\S4method{response}{mixDesign}(object) <- value
\S4method{response}{gageRR}(object)
\S4method{response}{gageRR}(object) <- value
\S4method{response}{taguchiDesign}(object)
\S4method{response}{taguchiDesign}(object) <- value
\S4method{response}{steepAscent}(object)
\S4method{response}{steepAscent}(object) <- value
\S4method{response}{MSALinearity}(object)
\S4method{response}{MSALinearity}(object) <- value
}

\arguments{
\item{object}{a \code{facDesign}, \code{mixDesign}, \code{gageRRDesign} or \code{taguchiDesign} object}
\item{value}{new response vector}
}

\section{Methods}{
\describe{

\item{\code{signature(object = "MSALinearity")}}{
Function to create the respone for an object of class \code{MSALinearity}.
}

\item{\code{signature(object = "facDesign")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the \code{\link{response}} attribute of a \code{\link{facDesign}} object.\cr
Value must be of same length as \code{nrow(object)}. If value is shorter/longer than \code{nrow(object)} the setting of the response will fail.
}

\item{\code{signature(object = "mixDesign")}}{
\code{\link{response}} is a generic accessor function, and \code{response<-} is a generic replacement function. The default methods get and set the \code{\link{response}} attribute of a \code{\link{mixDesign}} object.\cr
Value must be of same length as \code{nrow(object)}. If value is shorter/longer than \code{nrow(object)} the setting of the response will fail.
}

\item{\code{signature(object = "gageRR")}}{
\code{\link{response}} is a generic accessor function, and \code{response<-} is a generic replacement function. The default methods get and set the \code{\link{response}} attribute of \code{\link{gageRRDesign}} object.\cr
Value must be of same length as \code{nrow(object)}. If value is shorter/longer than \code{nrow(object)} the setting of the response will fail.
}

\item{\code{signature(object = "taguchiDesign")}}{
\code{\link{response}} is a generic accessor function, and \code{response<-} is a generic replacement function. The default methods get and set the \code{\link{response}} attribute of \code{\link{taguchiDesign}} object.
value must be of same length as \code{nrow(object)}. If value is shorter/longer than \code{nrow(object)} the setting of the response will fail.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\seealso{\code{\link{factors}}\cr
\code{\link{fracDesign}}\cr
\code{\link{taguchiDesign}}\cr
\code{\link{mixDesign}}\cr
\url{http://www.user.tu-berlin.de/kalicete/qualityTools}
}

\examples{
fdo = fracDesign(k = 3)   #NA in response column
fdo
y = rnorm(8)              #response
response(fdo) = y         #2^k numeric values in response column
fdo
}



