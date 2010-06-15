\name{response-methods}
\docType{methods}
\alias{response}
\alias{response<-}
\alias{response,facDesign-method}
\alias{response<-,facDesign-method}
\alias{response,gageRR-method}
\alias{response<-,gageRR-method}

\title{Get and set methods}
\description{Set or get the response for a 'facDesign' or 'gageRR' object}

\usage{
\S4method{response}{facDesign}(object)
\S4method{response}{facDesign}(object) <- value
\S4method{response}{gageRR}(object)
\S4method{response}{gageRR}(object) <- value
}

\arguments{
\item{object}{a `facDesign' object}
\item{value}{new response vector}
}



\section{Methods}{
\describe{

\item{\code{signature(object = "facDesign")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the "response" attribute of a "facDesign" object.
value must be of same length as nrow(object). If value is shorter/longer than nrow(object) the setting of the response will fail.
}

\item{\code{signature(object = "gageRR")}}{
response is a generic accessor function, and response<- is a generic replacement function. The default methods get and set the "response" attribute of "gageDesign" object.
value must be of same length as nrow(object). If value is shorter/longer than nrow(object) the setting of the response will fail.
}

}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\seealso{\code{\link{factors}}, \code{\link{fracDesign}}}

\examples{
fdo = fracDesign(k = 3)  #NA in response column
fdo
y = rnorm(8) #response
response(fdo) = y#2^k numeric values in response column
fdo
}



