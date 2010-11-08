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

\title{Get and set methods}
\description{Set or get the response for a \code{facDesign}, \code{mixDesign}, \code{gageDesign} or \code{taguchiDesign} object}

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
}

\arguments{
\item{object}{a \code{facDesign}, \code{mixDesign}, \code{gageDesign} or \code{taguchiDesign} object}
\item{value}{new response vector}
}



\section{Methods}{
\describe{

\item{\code{signature(object = "facDesign")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the "response" attribute of a "facDesign" object.
value must be of same length as nrow(object). If value is shorter/longer than nrow(object) the setting of the response will fail.
}

\item{\code{signature(object = "mixDesign")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the "response" attribute of a "mixDesign" object.
value must be of same length as nrow(object). If value is shorter/longer than nrow(object) the setting of the response will fail.
}

\item{\code{signature(object = "gageRR")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the "response" attribute of "gageDesign" object.
value must be of same length as nrow(object). If value is shorter/longer than nrow(object) the setting of the response will fail.
}

\item{\code{signature(object = "taguchiDesign")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the "response" attribute of "taguchiDesign" object.
value must be of same length as nrow(object). If value is shorter/longer than nrow(object) the setting of the response will fail.
}

}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\seealso{\code{\link{factors}}, \code{\link{fracDesign}},\code{\link{taguchiDesign}},\code{\link{mixDesign}}}

\examples{
fdo = fracDesign(k = 3)  #NA in response column
fdo
y = rnorm(8) #response
response(fdo) = y#2^k numeric values in response column
fdo
}



