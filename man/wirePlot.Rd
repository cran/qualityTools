\name{wirePlot}
\Rdversion{1.1}
\alias{wirePlot}

\title{
3D Plot

}
\description{
Creates a wireframe diagramm for an object of class facDesign
}
\usage{
wirePlot(x, y, z, data = NULL, xlim, ylim, zlim, main, xlab, ylab, border, sub, zlab, form = "fit", phi, theta, ticktype, col = 1, steps, factors, fun, plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Factor A  
}
  \item{y}{
Factor B
}
  \item{z}{
Response variable
}
  \item{data}{
an object of class facDesign
}
  \item{xlim}{
graphical parameter
}
  \item{ylim}{
graphical parameter
}
  \item{zlim}{
graphical parameter
}
  \item{main}{
graphical parameter
}
  \item{xlab}{
graphical parameter
}
  \item{ylab}{
graphical parameter
}
  \item{border}{
  the color of the line drawn around the surface facets
}
  \item{sub}{
  the subtitle
}
  \item{zlab}{
graphical parameter
}
  \item{form}{
  \dQuote{quadratic}, \dQuote{full}, \dQuote{interaction}, \dQuote{linear}, \dQuote{fit}  or a formula y ~ x+y +x*y; \dQuote{fit} takes the formula from the fit in the facDesign object fdo. Quadratic or higher orders should be given as I(Variable^2).
}
  \item{phi}{
  angle defining the viewing direction. theta gives the azimuthal direction. phi gives the colatitude.
}
  \item{theta}{
  angle defining the viewing direction. theta gives the azimuthal direction.
}
  \item{ticktype}{
graphical parameter
}
  \item{col}{
a predefined (1, 2, 3 or 4) or self defined colorRampPalette or color to be used (i.e. \dQuote{red})
}
  \item{steps}{
  number of grid points per factor  
}
  \item{factors}{
  list of 4th 5th factor with value i.e. factors = list(D = 1.2, E = -1), if nothing is specified  values will be the mean of the low and the high value of the factors
}
  \item{fun}{
  function to be applied to z \dQuote{desirability}     
}
  \item{plot}{
  if TRUE a plot is created (most likely to disappear with time)
}
}
\details{
This function can be used to display the desirability of each response by specifying fun = \dQuote{desirability} or the fun = \dQuote{overall} (i.e. composed) desirability of all responses. The required desirabilities can be set using \code{\link{desires}}.
}
\value{
  \item{x}{locations of grid lines for x at which the values in z are measured}
  \item{y}{locations of grid lines for y at which the values in z are measured}
  \item{z}{a matrix containing the values of z to be plotted}
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}


\seealso{
\code{\link{contourPlot}},
\code{\link{filled.contour}},
\code{\link{persp}}
}
\examples{
#create a response surface design and assign random data to response y
fdo = rsmDesign(k = 3, blocks = 2)
response(fdo) = data.frame(y = rnorm(nrow(fdo)))

par(mfrow = c(3,2))

#I - display linear fit
wirePlot(A,B,y, data = fdo, form = "linear")

#II - display full fit (i.e. effect, interactions and quadratic effects
wirePlot(A,B,y, data = fdo, form = "full")

#III - display a fit specified before
fits(fdo) = lm(y ~ B + I(A^2) , data = fdo)
wirePlot(A,B,y, data = fdo, form = "fit")

#IV - display a fit given directly
wirePlot(A,B,y, data = fdo, form = "y ~ A*B + I(A^2)")

#V - display a fit using a different colorRamp
wirePlot(A,B,y, data = fdo, form = "full", col = 2)

#V - display a fit using a self defined colorRamp
myColour = colorRampPalette(c("green", "gray","blue"))
wirePlot(A,B,y, data = fdo, form = "full", col = myColour)


}
