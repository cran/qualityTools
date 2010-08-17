\name{qqPlot}
\Rdversion{1.1}
\alias{qqPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Quantile-Quantile Plots for various distributions
}
\description{
qqPlot creates a QQ plot of the values in x including a line which passes through the first and third quartiles. Supported distributions are beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "log-normal", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t" and "weibull"
}
\usage{
qqPlot(x, y, main, xlab, ylab, start, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the sample for qqPlot
}
  \item{y}{
a character specifying the distribution for the QQ plot or the second sample (if given --> defaulting to qqplot)
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
a list containing the x and y quantiles
\item{x}{sample quantiles}
\item{y}{theoretical quantiles}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}


\seealso{
fitdistr in R-package MASS
}
\examples{
#set up the plotting window for 6 plots
par(mfrow = c(3,2))

#generate random data from weibull distribution
x = rweibull(20, 8, 2)

#Quantile-Quantile Plot for different distributions
qqPlot(x, "log-normal")
qqPlot(x, "normal")
qqPlot(x, "exponential", DB = TRUE)
qqPlot(x, "cauchy")
qqPlot(x, "weibull")
qqPlot(x, "logistic")        
}
