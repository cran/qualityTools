\name{overall}
\alias{overall}
\title{
Overall Desirability
}
\description{
This is a function to calculate the desirability for each response as well as the overall desirability.\cr
The resulting \code{\link{data.frame}} can be used to plot the overall as well as the desirabilities for each response. \cr
This function serves for a visualization of the desirability approach for multiple response optimization.
}
\usage{
overall(fdo, steps = 20, constraints, ...)
}
\arguments{
  \item{fdo}{
needs to be an object of class \code{\link{facDesign}} containing \code{\link{fits}} and \code{\link{desires}}}
  \item{steps}{
numeric value - points per factor to be evaluated --> specifies also the grid size.
}
  \item{constraints}{
list - constraints for the factors in coded values such as list(A > 0.5,  B < 0.2). 
}
  \item{\dots}{
further arguments.
}
}
\value{
\code{overall} returns a \code{\link{data.frame}} with a column for each factor, desirability for each response and a column for the overall desirability.}
\references{
see \code{\link{desirability}}.
}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}


\seealso{
\code{\link{facDesign}}\cr
\code{\link{rsmDesign}}\cr
\code{\link{desirability}}\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
#arbitrary example with random data!!!
rsdo = rsmDesign(k = 2, blocks = 2, alpha = "both")
set.seed(123)
response(rsdo) = data.frame(y = rnorm(nrow(rsdo)), y2 = rnorm(nrow(rsdo)))
fits(rsdo) = lm(y ~ A*B + I(A^2) + I(B^2), data = rsdo)
fits(rsdo) = lm(y2 ~ A*B + I(A^2) + I(B^2), data = rsdo)
desires(rsdo) = desirability(y, -1, 2, scale = c(1, 1), target = "max")
desires(rsdo) = desirability(y2, -1, 0, scale = c(1, 1), target = "min")
dVals = overall(rsdo, steps = 10, constraints = list(A = c(-0.5,1), 
                B = c(0, 1)))

##Uncomment for visualization of desirabilities
#require(lattice)
#contourplot(y ~ A*B,  data = dVals) #desirability of y
#contourplot(y2 ~ A*B,  data = dVals) #desirability of y2
#wireframe(overall ~ A*B, shade = TRUE, data = dVals)
}