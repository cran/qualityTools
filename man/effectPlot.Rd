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
a facDesign or taguchiDesign object
}
  \item{factors}{
for which factor is the effectPlot to be created
}
  \item{fun}{
a function for the construction of the effectPlot such as \code{\link{mean}}, \code{link{median}}
}
  \item{single}{
if TRUE device region can be set up using for instance par(mfrow = c(2,2))
}
  \item{points}{
if TRUE points are shown in addition to fun values
}
  \item{axes}{
TRUE by default
}
  \item{classic}{
TRUE by default to create an effectPlot as depicted in most textbooks
}
  \item{lty}{
line type used
}
  \item{xlab}{
label for x-axis
}
  \item{ylab}{
label for y-axis
}
  \item{main}{
title
}
  \item{ylim}{
limits for y-axis
}
  \item{\dots}{
further graphical parameters
}
}


\details{
effectPlot uses an altered version of the base function interaction.plot to draw each effectPlot. 
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\seealso{
\code{\link{interactionPlot}}, \code{\link{paretoPlot}},\code{\link{facDesign}}, \code{\link{response}},\code{\link{normalPlot}}
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
