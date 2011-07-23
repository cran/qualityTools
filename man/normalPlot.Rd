\name{normalPlot}
\alias{normalPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Half-Normal plot
}
\description{
function to generate a half-normal plot of the factor effects for an object of class facDesign
}
\usage{
normalPlot(fdo, threeWay = FALSE, na.last = NA, alpha = 0.05, sig.col = c("red1", "red2", "red3"), main, ylim, xlim, xlab, ylab, pch, col, border = "red", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fdo}{
object of class facDesign
}
  \item{threeWay}{
TRUE/FALSE plot three-way or higher interactions 
}
  \item{na.last}{
%%     ~~Describe \code{na.last} here~~
}
  \item{alpha}{
alpha for marking interactions
}
  \item{sig.col}{
vector - colors for marking significant interactions
}
  \item{main}{
graphical parameter
}
  \item{ylim}{
graphical parameter
}
  \item{xlim}{
graphical parameter
}
  \item{xlab}{
graphical parameter
}
  \item{ylab}{
graphical parameter
}
  \item{pch}{
graphical parameter
}
\item{col}{
graphical parameter
}
\item{border}{
graphical parameter
}

  \item{\dots}{
further graphical parameters
}
}
\value{
NULL}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{facDesign}},
\code{\link{paretoPlot}},
}
\examples{
fdo = facDesign(k=3, replicates = 2)  #factorial design
set.seed(123) #seed for random numbers
y = rnorm(nrow(fdo))  #random numbers
response(fdo) = y #set the response
normalPlot(fdo) #create a normal plot
}