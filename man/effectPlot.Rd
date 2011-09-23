\name{effectPlot}
\alias{effectPlot}
\title{
Main Effect Plots
}
\description{
A main effect plot is created for designs of type taguchiDesign and facDesign.
}
\usage{
effectPlot(object, factors, fun = mean, single = FALSE, points = FALSE, classic = FALSE, axes = TRUE, lty, xlab, ylab, main, ylim, ...)
}

\arguments{
  \item{object}{
needs to be an object of class \code{\link{facDesign}} or \code{\link{taguchiDesign}}.
}
  \item{factors}{
for which factor is the effectPlot to be created.
}
  \item{fun}{
a function for the construction of the effectPlot such as \code{\link{mean}}, \code{link{median}},etc.\cr
By default \code{fun} is set to \code{\link{mean}}. 
}
  \item{single}{
logical value. If \sQuote{TRUE} device region can be set up using for instance \code{par(mfrow = c(2,2))}.\cr
By default \code{single} is set to \sQuote{FALSE}.
}
  \item{points}{
logical value. If \sQuote{TRUE} points are shown in addition to values out of \code{fun}.\cr
By default \code{points} is set to \sQuote{FALSE}.
}
  \item{axes}{
logical value indicating wheter the axes should be drawn or not.\cr
\sQuote{TRUE} by default.
}
  \item{classic}{
logical value. \sQuote{TRUE} creates an effectPlot as depicted in most textbooks.\cr
By default \code{classic} is set to \sQuote{FALSE}.
}
  \item{lty}{
numerical value which specifies the line type used.
}
  \item{xlab}{
a title for the x axis: \code{\link{title}}. 
}
  \item{ylab}{
a title for the y axis: \code{\link{title}}. 
}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{ylim}{
vector giving the range of the y-axis.
}
  \item{\dots}{
Arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}). 
}
}
 
\details{
effectPlot uses an altered version of the base function \code{\link{interaction.plot}} to draw each effectPlot. 
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\note{
 For an example in context which shows the usage of the function \code{effectPlot()} to an object of class \code{\link{facDesign}} or an object of class \code{\link{taguchiDesign}}, 
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.user.tu-berlin.de/kalicete/qualityTools/S5.html#SS1}.
}

\seealso{
\code{\link{interactionPlot}}\cr
\code{\link{paretoPlot}}\cr
\code{\link{facDesign}}\cr
\code{\link{response}}\cr
\code{\link{normalPlot}}\cr
\url{http://www.user.tu-berlin.de/kalicete/qualityTools/S5.html#SS1}
}
\examples{
#effectPlot for a 2^k factorial design
fdo = facDesign(k = 3)
response(fdo) = rnorm(8)  #set response with generic response function
effectPlot(fdo)

#effectPlot for a taguchiDesign
tdo = taguchiDesign("L9_3")
response(tdo) = rnorm(9)
effectPlot(tdo, points = TRUE, col = 2, pch = 16, lty = 3)

}
\keyword{design} %designed experiments
\keyword{methods}%Methods and Generic functions
