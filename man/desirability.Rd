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
a value ranging from 0.1 to 10, used to calculate a weighted importance i.e. with importances 1,2 and 4 D=[(d1)^1,(d2)^2,(d3)^4]^(1/7)
}
  \item{constraints}{
not yet implemented
}
}
\details{
For a product to be developed different values of responses are desired leading to multiple response optimization. Minimization, Maximization as well as a specific target value are defined using desirability functions. A desirability function tranforms the values of a response into [0,1] where 0 stands for a non acceptable value of the response and 1 for values where higher/lower (depending on the direction of the optimization) values of the response have little merit.

A first desirability function was specified by Harrington (1965), Derringer and Suich (1980) came up with a modified appproach to transform several responses into a desirability function which was extended with the possibility of specifying weights Derringer (1994). Castillo, Montgomery and McCarville came up with a another modification. The first and the latter are not implemented

}
\value{
a desirability function
}
\references{
Harrington, EC 1965, 'The Desirability Function', Journal of the American Society for Quality Control, pp. 494-498.
Derringer, G and Suich, R 1980, 'Simulaneous Optimization of Several Response Variables', Journal of Quality Technology, vol. 12, no. 4. 214-219
Derringer, G 1994, 'A Balancing Act: Optimizing a Product's Properties', Quality Progress, vol. 27, no. 6, pp. 51-58.
Castillo, ED, Montgomery, DC & McCarville, DR 1996, 'Modified Desirability Functions for Multiple Response Optimization', Journal of Quality Technology, vol. 28, no. 3, pp. 337-345.
}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{desires}},
\code{\link{optimum}}
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
