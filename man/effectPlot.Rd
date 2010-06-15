\name{effectPlot}
\alias{effectPlot}
\title{effectPlot}
\description{Display effects of an object of class 'facDesign' in a line plot.}

\usage{
effectPlot(fdo, main = paste("Main Effect Plot for ", deparse(substitute(fdo))), showPoints = FALSE, ylab = NULL, pch = 15, ...)
}
\arguments{
\item{fdo}{an object of class facDesign}
\item{showPoints}{logical. If TRUE, single observations are displayed as well.}
\item{main}{main}
\item{ylab}{ylab}
\item{pch}{pch}
\item{...}{further graphical parameters}
}
\details{
effectPlot displays effects for an object of class facDesign (i.e. 2^k full or 2^k-p fractional factorial design).
}
\value{none}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{factors}}, \code{\link{fracDesign}}}
\examples{
vp = fracDesign(k = 3, replicates = 2)  #NA in response column and 2 replicates per factor combination
y = 4*vp[,1] -7*vp[,2] + 2*vp[,2]*vp[,1] + 0.2*vp[,3] + rnorm(16)                #generate some data
response(vp) = y
effectPlot(vp)                         #show effects and interactions (nothing significant expected)

}