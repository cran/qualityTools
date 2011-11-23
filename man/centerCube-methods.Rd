\name{centerCube-methods}
\docType{methods}
\alias{centerCube}
\alias{centerCube<-}
\alias{centerCube-methods}
\alias{centerCube,facDesign-method}
\alias{centerCube<-,facDesign-method}
\title{Get and set methods}
\description{Set or get the Center Points of the cube portion for an object of class facDesign}

\usage{
\S4method{centerCube}{facDesign}(x)
\S4method{centerCube}{facDesign}(x) <- value
}
\arguments{
\item{x}{a \code{\link{facDesign}} object}
\item{value}{data.frame or vector}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{\link{centerCube}} for the factors in an object of class \code{\link{facDesign}}. So far used internally.
}
}}
\seealso{
\code{\link{centerStar}} \cr
\code{\link{cube}} \cr
\code{\link{star}} \cr
\url{http://www.r-qualitytools.org}

}
\examples{
#create a response surface design for k = 3 factors
rsdo = rsmDesign(k = 3)

#split design into two blocks
rsdo = blocking(rsdo, 2)

#set two Center Points per block
centerCube(rsdo) = data.frame(A = c(0,0), B = c(0,0), C = c(0,0))

#get the centerPoints of the cube portion
centerCube(rsdo)
}