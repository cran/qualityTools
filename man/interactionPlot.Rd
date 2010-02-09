\name{interactionPlot}
\alias{interactionPlot}
\title{interactionPlot}
\description{Display effects of an object of class 'facDesign' in a line plot.}

\usage{
interactionPlot(fdo = NULL, y = NULL, response = NULL, fun = mean, main = paste("Interaction plot for ", deparse(substitute(fdo))), col = 1:2, ...)
}
\arguments{
\item{fdo}{an object of class facDesign or a vector x}
\item{fun}{function to use for the calculation of the interactions.}
\item{col}{vector of colors for the plot}
\item{y}{if fdo is a vector, interactionPlot defaults to interaction.plot if y is a vector too.}
\item{response}{response variable}
\item{main}{main}
\item{...}{further graphical parameters}
}
\details{
interactionPlot displays interactions for an object of class facDesign (i.e. 2^k full or 2^k-p fractional factorial design). Parts of the original interactionPlot were integrated.
}
\value{none}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{fracDesign}}}
\examples{
vp = fracDesign(k = 3, replicates = 2)  #NA in response column and 2 replicates per factor combination
y = 4*vp[,1] -7*vp[,2] + 2*vp[,2]*vp[,1] + 0.2*vp[,3] + rnorm(16)                #generate some data
response(vp) = y
interactionPlot(vp)                         #show effects and interactions (nothing significant expected)

}