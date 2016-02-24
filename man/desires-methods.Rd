\name{desires-methods}
\docType{methods}
\alias{desires}
\alias{desires<-}
\alias{desires-methods}
\alias{desires,facDesign-method}
\alias{desires<-,facDesign-method}
\keyword{Multiple Response Optimization}
\title{Get and set methods}
\description{Set or get the desirability for each response of a factorial design. The desirability is stored in the facDesign object. Setting desirabilities is required for optimization of multiple responses.}

\usage{
\S4method{desires}{facDesign}(x)
\S4method{desires}{facDesign}(x) <- value
}
\arguments{
  \item{x}{an object of class \code{\link{facDesign}}}
  \item{value}{an object of class \code{\link{desirability}}; see example}
}
\section{Methods}{
\describe{
\item{\code{signature(objectc = "facDesign")}}{
Get and set the desires for an object of class \code{\link{facDesign}}.
}
}}

\references{
DERRINGER, G.; SUICH, R. 'Simulaneous Optimization of Several Response Variables', Journal of Quality Technology, vol. 12, no. 4. 214-219, 1980.
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{desirability}}\cr
\code{\link{fits}}\cr
\code{\link{optimum}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
#create a response surface design
fdo = rsmDesign(k = 2, blocks = 2, alpha = "both")

#set two responses for the response surface designs
response(fdo) = data.frame(y= rnorm(14, 12, sd =  2), 
                           y2 =  -2*fdo[,4]^2 - fdo[,5]^2 + rnorm(14, 12))

#set a fit for each response
fits(fdo) = lm(y ~ A*B , data = fdo)
fits(fdo) = lm(y2 ~ A*B + I(A^2) + I(B^2), data = fdo)

#define a desirability for response y
d = desirability(y, 6, 18, scale = c(0.5, 2), target = 12)

#plot the desirability function
plot(d)

#set the desirability for y and y2 in the factorial design fdo
desires(fdo) = d
desires(fdo) = desirability(y2, 6, 18, scale = c(1, 1), target = "min")
desires(fdo)
}
