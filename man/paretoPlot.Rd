\name{paretoPlot}
\alias{paretoPlot}
\title{paretoPlot}
\description{Display standardized effects and interactions of a 'facDesign' object in a pareto plot.}

\usage{
paretoPlot(fdo, threeWay = FALSE, abs = TRUE, decreasing = TRUE, na.last = NA, alpha = 0.05, xlim, ylim, xlab, ylab, main, ...)
}
\arguments{
\item{fdo}{an object of class facDesign}
\item{threeWay}{logical. If TRUE, threeway-interactions are displayed as well.}
\item{abs}{logical. If TRUE, absolute effects and interactions are displayed.}
\item{alpha}{the significance level used to calculate the critical value}
\item{decreasing}{logical. If TRUE, effects and interactions are sorted decreasing.}
\item{na.last}{na.last}
\item{xlab}{graphical parameter}
\item{ylab}{graphical parameter}
\item{xlim}{graphical parameter}
\item{ylim}{graphical parameter}
\item{main}{graphical parameter}
\item{...}{graphical parameters}
}
\details{
paretoPlot displays a pareto plot of effects and interactions for an object of class facDesign (i.e. 2^k full or 2^k-p fractional factorial design). For a given significance level alpha, a critical value is calculated and added to the plot. Standardization is achieved by dividing estimates with their standard error.
For unreplicated fractional factorial designs a Lenth Plot is generated.
}

\value{a list of effects for each response in the 'facDesign' object}

\references{
Design and Analysis of experiments - Volume2 - Advanced Experimental Design - Hinkelmann/Kempthorne
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{fracDesign}}, \code{\link{facDesign}}}
\examples{
#factorial design with replications
vp = fracDesign(k = 3, replicates = 2)  #NA in response column and 2 replicates per factor combination
y = 4*vp[,1] -7*vp[,2] + 2*vp[,2]*vp[,1] + 0.2*vp[,3] + rnorm(16)                #generate some data
response(vp) = y
paretoPlot(vp)                         #show effects and interactions (nothing significant expected)

#fractional factorial design --> Lenth Plot
vp = fracDesign(k = 4, gen = "D = ABC")
y = rnorm(8)                #generate some data
response(vp) = y
paretoPlot(vp)                         #show effects and interactions (nothing significant expected)


}