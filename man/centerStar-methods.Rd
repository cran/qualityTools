\name{centerStar-methods}
\docType{methods}
\alias{centerStar}
\alias{centerStar<-}
\alias{centerStar-methods}
\alias{centerStar,facDesign-method}
\alias{centerStar<-,facDesign-method}
\title{Get and set methods}
\description{Get and set the Center Points of the star portion for an object of class facDesign}

\usage{
\S4method{centerStar}{facDesign}(x)
\S4method{centerStar}{facDesign}(x) <- value
}
\arguments{
\item{x}{a `facDesign' object}
\item{value}{data.frame or vector}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the Center Points of the star portion for an object of class facDesign. So far used internally.
}
}}
\seealso{
\code{\link{centerCube}},
\code{\link{cube}},
\code{\link{star}},
}
\examples{
#create a response surface design for k = 3 factors
rsdo = rsmDesign(k = 3)
centerStar(rsdo)

}