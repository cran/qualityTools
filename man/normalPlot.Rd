\name{normalPlot}
\alias{normalPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Half-Normal plot
}
\description{
function to generate a half-normal plot of the factor effects for an object of class \code{\link{facDesign}}.
}
\usage{
normalPlot(fdo, threeWay = FALSE, na.last = NA, alpha = 0.05, sig.col = c("red1", "red2", "red3"), main, ylim, xlim, xlab, ylab, pch, col, border = "red", ...)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{sig.col}{
vector - colors for marking significant interactions. By default \code{sig.col} is set to \sQuote{c("red1", "red2", "red3")}. 
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
\value{
NULL}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\note{
 For a more detailed example which shows the usage of \code{normalPlot()} in context please read the vignette for the package 
 \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{facDesign}}\cr
\code{\link{paretoPlot}}\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
fdo = facDesign(k=3, replicates = 2)  #factorial design
set.seed(123) #seed for random numbers
y = rnorm(nrow(fdo))  #random numbers
response(fdo) = y #set the response
normalPlot(fdo) #create a normal plot
}