\name{paretoPlot}
\alias{paretoPlot}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{paretoPlot}
\description{Display standardized effects and interactions of a 'facDesign' object in a pareto plot.}

\usage{
paretoPlot(fdo, threeWay = FALSE, abs = TRUE, decreasing = TRUE, na.last = NA,
           alpha = 0.05, response = NULL, xlim, ylim, xlab, ylab, main,
           single = TRUE, ...)
}
\arguments{
\item{fdo}{an object of class facDesign}
\item{threeWay}{logical. If TRUE, threeway-interactions are displayed as well.}
\item{abs}{logical. If TRUE, absolute effects and interactions are displayed.}
\item{alpha}{the significance level used to calculate the critical value}
\item{response}{
response variable. If the response data frame of fdo consists of more then one responses,\cr
this variable can be used to choose just one column of the response data frame. \code{response} needs to be an object of class 
character with length of \sQuote{1}. It needs to be the same character as the name of the response in the response data frame that should be plotted.\cr
By default \code{response} is set to \sQuote{NULL}.}
\item{decreasing}{logical. If TRUE, effects and interactions are sorted decreasing.}
\item{na.last}{na.last}
\item{xlab}{graphical parameter}
\item{ylab}{graphical parameter}
\item{xlim}{graphical parameter}
\item{ylim}{graphical parameter}
\item{main}{graphical parameter}
\item{single}{
 a logical value.If \sQuote{TRUE} a new graphic device will be opened for each  \cr
 column of the respond dataframe of fdo (\code{response(fdo)}).\cr
 If set to \sQuote{FALSE} \code{par(mfrow)} will be set internally.\cr
 By default \code{single} is set to \sQuote{TRUE}.}
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
\seealso{\code{\link{factors}}, \code{\link{fracDesign}}, \code{\link{facDesign}}, \url{http://webapps.r-qualitytools.org/brew/facDesign/fracDesign.html}}
\examples{
#factorial design with replications
#NA in response column and 2 replicates per factor combination
vp = fracDesign(k = 3, replicates = 2)  
#generate some data
y1 = 4*vp[,1] -7*vp[,2] + 2*vp[,2]*vp[,1] + 0.2*vp[,3] + rnorm(16)
y2 = 9*vp[,1] -2*vp[,2] + 5*vp[,2]*vp[,1] + 0.5*vp[,3] + rnorm(16)               
response(vp) = data.frame(y1,y2)
#show effects and interactions (nothing significant expected)
paretoPlot(vp)                         

#fractional factorial design --> Lenth Plot
vp = fracDesign(k = 4, gen = "D = ABC")
#generate some data
y = rnorm(8)                
response(vp) = y
#show effects and interactions (nothing significant expected)
paretoPlot(vp)                        


}