\name{interactionPlot}
\alias{interactionPlot}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{interactionPlot}
\description{Display effects of an object of class \code{facDesign} in a line plot.}

\usage{
interactionPlot(fdo, y = NULL, response = NULL, fun = mean, main, col = 1:2, 
                ...)
}
\arguments{
\item{fdo}{needs to be an object of class \code{\link{facDesign}}.}
\item{fun}{function to use for the calculation of the interactions, as \code{\link{mean}},\code{\link{median}},etc.}
\item{col}{vector of colors for the plot. Single colors can be given as character strings or numeric values.}
\item{y}{if fdo is a vector, \code{interactionPlot()} defaults to \code{\link{interaction.plot}} from package \code{stats} if y is a vector too.}
\item{response}{response variable. If the response data frame of fdo consists of more then one responses,\cr
                this variable can be used to choose just one column of the response data frame. \code{response} needs to be an object of class 
                character with length of \sQuote{1}. It needs to be the same character as the name of the response in the response data frame that should be plotted.}
\item{main}{an overall title for the plot: see \code{\link{title}}.}
\item{...}{further graphical parameters: see \code{\link{par}}.}
}
\details{
\code{interactionPlot()} displays interactions for an object of class \code{\link{facDesign}} (i.e. 2^k full or 2^k-p fractional factorial design).\cr
Parts of the original interactionPlot were integrated.
}
\value{none}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
 For an example in context which shows the usage of the function \code{interactionPlot()} to an object of class \code{\link{facDesign}}, 
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}
\seealso{
\code{\link{factors}}\cr
\code{\link{fracDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}\cr
\url{http://webapps.r-qualitytools.org/brew/facDesign/fracDesign.html}
}
\examples{
#NA in response column and 2 replicates per factor combination
vp = fracDesign(k = 3, replicates = 2) 
#generate some data                            
y = 4*vp[,1] -7*vp[,2] + 2*vp[,2]*vp[,1] + 0.2*vp[,3] + rnorm(16)  
response(vp) = y
#show effects and interactions (nothing significant expected)
interactionPlot(vp)                                                

}