\name{ppPlot}
\Rdversion{1.1}
\alias{ppPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability Plots for various distributions
}
\description{
ppPlot creates a Probability plot of the values in x including a line. Supported distributions are beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t" and "weibull".
}
\usage{
ppPlot(x, distribution, probs, main, xlab, ylab, xlim, ylim, grid = TRUE, box = TRUE, stats = TRUE, start, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the sample for ppPlot
}
  \item{distribution}{
a character specifying the distribution for the Probability plot
}
  \item{probs}{
percentages for the y axis
}
  \item{main}{
graphical parameter
}
  \item{xlab}{
graphical parameter
}
  \item{ylab}{
graphical parameter
}
  \item{xlim}{
graphical parameter
}
  \item{ylim}{
graphical parameter
}
  \item{grid}{
add a grid to the current plot
}
  \item{box}{
%%     ~~Describe \code{box} here~~
}
  \item{stats}{
%%     ~~Describe \code{stats} here~~
}
  \item{start}{
A named list giving the parameters to be fitted with initial values. Must be supplied for some distribution. (see Details)
}
  \item{\dots}{
further graphical parameters (see \code{\link{par}})
}
}
\details{
Distribution fitting is deligated to function fitdistr of the R-package MASS
}
\value{
a list 
\item{x}{coordinates}
\item{y}{coordinates}
\item{int}{Intercept}
\item{slope}{slope}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}


\seealso{
fitdistr in R-package MASS~
}
\examples{
#set up the plotting window for 6 plots
par(mfrow = c(3,2))

#generate random data from weibull distribution
x = rweibull(20, 8, 2)

#Probability Plot for different distributions
ppPlot(x, "log-normal")
ppPlot(x, "normal")
ppPlot(x, "exponential", DB = TRUE)
ppPlot(x, "cauchy")
ppPlot(x, "weibull")
ppPlot(x, "logistic")        

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
