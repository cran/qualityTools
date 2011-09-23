\name{runOrd-methods}
\docType{methods}
\alias{runOrd}
\alias{runOrd<-}
\alias{runOrd,facDesign-method}
\alias{runOrd<-,facDesign-method}
                        
\title{Get and set methods}
\description{Get and set the runOrd for a \code{\link{facDesign}} x.}
\usage{
\S4method{runOrd}{facDesign}(x)
\S4method{runOrd}{facDesign}(x) <- value
}

\arguments{
\item{x}{a \code{\link{facDesign}} object}
\item{value}{new runOrd vector}
}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\section{Methods}{
\describe{

\item{\code{signature(object = "facDesign")}}{
%%  ~~describe this method here~~
}
}}
\seealso{
\url{http://www.user.tu-berlin.de/kalicete/qualityTools}
}
\examples{
fdo = facDesign(k = 3)  #NA in response column
runOrd(fdo)
}

