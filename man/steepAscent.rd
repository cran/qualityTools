\name{steepAscent}
\alias{steepAscent}
\title{
Steepest Ascent
}
\description{
\code{steepAscent} is a method to calculate the steepest ascent for a \code{\link{facDesign}} object.
}
\usage{
steepAscent(factors, response, size = 0.2, steps = 5, data)
}

\arguments{
  \item{factors}{
list containing vector of factor names (coded) to be included in calculation, first factor is the reference factor.
}
  \item{response}{
character - response given in data.
}
  \item{size}{
numeric integer value giving the step size in coded units for the first factor given in factors.\cr
By default \code{size} is set to \sQuote{0.2}.
}
  \item{steps}{
numeric integer value giving the number of steps.     \cr
By default \code{step} is set to \sQuote{5}.
}
  \item{data}{
needs to be an object of class \code{\link{facDesign}}.
}
}
\details{
A first order model is fitted for the factors given in factors. Based on the step size given the steepest ascent is calculated.
}
\value{
\code{steepAscent} returns an object of class \code{\link{steepAscent}}.
}
\references{
\url{http://www.itl.nist.gov/div898/handbook/pri/section5/pri5311.htm}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
This is the steepest ascent for a single response considering main effects only.\cr
For an example in context which shows the usage of the function \code{steepAscent()} to an object of class \code{\link{facDesign}}, 
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}
\seealso{
\code{\link{desires}} for multiple response optimization usign desirabilities\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
#Example from References
fdo = facDesign(k = 2, centerCube = 5)
lows(fdo) = c(170, 150)
highs(fdo) = c(230, 250)
names(fdo) = c("temperature", "time")
units(fdo) = c("C", "minutes")
yield = c(32.79, 24.07, 48.94, 52.49, 38.89, 48.29, 29.68, 46.5, 44.15)
response(fdo) = yield
summary(fdo)

sao = steepAscent(factors = c("B", "A"), response = "yield", size = 1,
                  data = fdo)
sao
obs.yield = c(NA, 56.2, 71.49, 75.63, 72.31, 72.10)
response(sao) = obs.yield
plot(sao, type = "b", col = 2, main = "Steepest Ascent")
}
\keyword{design}

