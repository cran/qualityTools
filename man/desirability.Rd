\name{desirability}
\Rdversion{1.1}
\alias{desirability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Desirability Function
}
\description{
Creates desirability functions for use in the optimization of multiple responses
}
\usage{
desirability(response, low, high, target = "max", scale = c(1, 1), importance = 1, constraints)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{response}{
name of the response
}
  \item{low}{
lowest acceptable value for the response
}
  \item{high}{
highest acceptable value for the response
}
  \item{target}{
desired target value of the response
}
  \item{scale}{
scaling factors for one and two sided transformations
}
  \item{importance}{
not yet implemented 
}
  \item{constraints}{
not yet implemented
}
}
\details{
For a product to be developed different values of responses are desired leading to multiple response optimization. Minimization, Maximization as well as a specific target value are defined using desirability functions. A desirability function tranforms the values of a response into [0,1] where 0 stands for a non acceptable value of the response and 1 for values where higher/lower (depending on the direction of the optimization) values of the response have little merit. 
}
\value{
a desirability function
}
\references{
DERRINGER, George; SUICH, Ronald: Simultaneous Optimization of Several Response Variables. Journal of Quality Technology Vol. 12, No. 4, p. 214-219
}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{desires}},
\code{\link{optimum}},
}
\examples{
#Maximization of a response
#define a desirability for response y where higher values of y are better as long as the response is smaller than high
d = desirability(y, low = 6, high = 18, target = "max")

#show and plot the desirability function
d; plot(d)


#Minimization of a response including a scaling factor
#define a desirability for response y where lower values of y are better as long as the response is higher than low
d = desirability(y, low = 6, high = 18, scale = c(2),target = "min")

#show and plot the desirability function
d; plot(d)

#Specific target of a response is best including a scaling factor
#define a desirability for response y where desired value is at 8 and values lower than 6 as well as values higher than 18 are not acceptable
d = desirability(y, low = 6, high = 18, scale = c(0.5,2),target = 12)

#show and plot the desirability function
d; plot(d)
}
