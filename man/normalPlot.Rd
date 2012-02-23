\name{normalPlot}
\alias{normalPlot}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{
Normal plot
}
\description{
function to generate a normal plot of the factor effects for an object of class \code{\link{facDesign}}.
}
\usage{
normalPlot(fdo, threeWay = FALSE, na.last = NA, alpha = 0.05, response = NULL,
           sig.col = c("red1", "red2", "red3"), sig.pch = c(1, 2, 3), main,
           ylim, xlim, xlab, ylab, pch, col, border = "red", ...)
}
\arguments{
  \item{fdo}{
object of class \code{\link{facDesign}}.
}
  \item{threeWay}{
\sQuote{TRUE}/\sQuote{FALSE} plot three-way or higher interactions. By default \code{threeWay} is set to \sQuote{FALSE}. 
}
  \item{na.last}{
By default set to \sQuote{NA}.
}
  \item{alpha}{
alpha for marking interactions.
}
  \item{response}{
response variable. If the response data frame of fdo consists of more then one responses,\cr
this variable can be used to choose just one column of the response data frame. \code{response} needs to be an object of class 
character with length of \sQuote{1}. It needs to be the same character as the name of the response in the response data frame that should be plotted.\cr
By default \code{response} is set to \sQuote{NULL}.}
  \item{sig.col}{
vector - colors for marking significant interactions. By default \code{sig.col} is set to \sQuote{c("red1", "red2", "red3")}. 
}
\item{sig.pch}{
vector - point characters for marking significant interactions. By default \code{sig.pch} is set to \sQuote{c(1, 2, 3)}. 
}
  \item{main}{
graphical parameter. A main title for the plot, see also \code{\link{title}}.
}
  \item{ylim}{
graphical parameter. The y limits of the plot.
}
  \item{xlim}{
graphical parameter. The x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a \sQuote{reversed axis}.
}
  \item{xlab}{
graphical parameter. A label for the x axis, defaults to a description of x.
}
  \item{ylab}{
graphical parameter. A label for the y axis, defaults to a description of y.
}
  \item{pch}{
graphical parameter. Vector containing numerical values or single characters giving plotting points for the different factors.\cr
See \code{\link{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).\cr
}
\item{col}{
graphical parameter. Single numerical value or character string giving the color for the points.
}
\item{border}{
graphical parameter. Single numerical value or character string giving the color of the border line.  
}
  \item{\dots}{
further graphical parameters see \code{\link{par}}.
}
}
\details{
If the given facDesign object \code{fdo} contains replicates this function will deliver a normal plot\cr
i.e.: effects divided by the standard deviation (t-value) will be plotted against an appropriate probability\cr
scaling (see: \code{\link{ppoints}}).\cr
If the given facDesign object \code{fdo} contains no replications the standard error can not be calculated.\cr
In that case the function will deliver an effect plot.       \cr
i.e.: the effects will be plotted against an appropriate probability scaling. (see: \code{\link{ppoints}}).\cr
 
}
\value{
NULL}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\note{
 For a more detailed example which shows the usage of \code{normalPlot()} in context please read the vignette for the package 
 \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}
\seealso{
\code{\link{facDesign}}\cr
\code{\link{paretoPlot}}\cr
\url{http://www.r-qualitytools.org/Improve.html}  \cr
\url{http://webapps.r-qualitytools.org/brew/facDesign/fracDesign.html}
}
\examples{
#factorial design
fdo = facDesign(k=3, replicates = 2)  
#seed for random numbers
set.seed(123) 
#random numbers
y = rnorm(nrow(fdo)) 
#set the response 
response(fdo) = y 
#create a normal plot
normalPlot(fdo) 
}