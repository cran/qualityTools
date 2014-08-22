\name{fits-methods}
\docType{methods}
\alias{fits}
\alias{fits<-}
\alias{fits-methods}
\alias{fits,facDesign-method}
\alias{fits<-,facDesign-method}
\title{Get and set methods}
\description{Set or get the \code{fits} (i.e. linear model) for each response of a factorial design. The fit is stored in the facDesign object. Setting fits is required for optimization of multiple responses.}

\usage{
\S4method{fits}{facDesign}(x)
\S4method{fits}{facDesign}(x) <- value
}
\arguments{
  \item{x}{an object of class \code{\link{facDesign}}.}
  \item{value}{an object of class \code{\link{lm}}.}
}

\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the \code{fits} for an object of class \code{\link{facDesign}}.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{desires}}\cr
\code{\link{desirability}}\cr
\code{\link{rsmDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}

}
\examples{
#create response surface design
fdo = rsmDesign(k = 2, blocks = 2, alpha = "both")

#set two responses for the response surface designs
response(fdo) = data.frame(y= rnorm(14, 12, sd =  2), 
                           y2 =  -2*fdo[,4]^2 - fdo[,5]^2 + rnorm(14, 12))

#set a fit for each response
fits(fdo) = lm(y ~ A*B , data = fdo)
fits(fdo) = lm(y2 ~ A*B + I(A^2) + I(B^2), data = fdo)

#get the fitted response for y2
fits(fdo)[["y2"]]

}
