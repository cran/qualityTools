\name{steepAscent}
\alias{steepAscent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Steepest Ascent
}
\description{
\code{\link{steepAscent}} is a method to calculate the steepest ascent for a facDesign object
}
\usage{
steepAscent(factors, response, size = 0.2, steps = 5, data)
}

\arguments{
  \item{factors}{
list - vector of factor names (coded) to be included in calculation, first factor is reference factor
}
  \item{response}{
character - response given in data
}
  \item{size}{
numeric - step size in coded units for the first factor given in factors
}
  \item{steps}{
integer - number of steps
}
  \item{data}{
fdo - an object of class facDesign
}
}
\details{
A first order model is fitted for the factors given in factors. Based on the step size given the steepest ascent is calculated.
}
\value{
an object of class steepAscent
}
\references{
\url{http://www.itl.nist.gov/div898/handbook/pri/section5/pri5311.htm}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
This is the steepest ascent for a single response considering main effects only.
}

\seealso{
\code{\link{desires}} for multiple response optimization usign desirabilities.
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

sao = steepAscent(factors = c("B", "A"), response = "yield", size = 1, data = fdo)
sao
obs.yield = c(NA, 56.2, 71.49, 75.63, 72.31, 72.10)
response(sao) = obs.yield
plot(sao, type = "b", col = 2, main = "Steepest Ascent")
}
\keyword{design}

