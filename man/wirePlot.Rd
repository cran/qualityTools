\name{wirePlot}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\Rdversion{1.1}
\alias{wirePlot}

\title{
3D Plot
}
\description{
Creates a wireframe diagramm for an object of class \code{\link{facDesign}}.
}
\usage{
wirePlot(x, y, z, data = NULL, xlim, ylim, zlim, main, xlab, ylab, border, sub, 
         zlab, form = "fit", phi, theta, ticktype, col = 1, steps, factors, fun,
         plot)
}
\arguments{
  \item{x}{
name providing the Factor A for the plot.
}
  \item{y}{
name providing the Factor B for the plot.
}
  \item{z}{
name giving the Response variable.
}
  \item{data}{
needs to be an object of class \code{\link{facDesign}} and contains the names of x,y,z.
}
  \item{xlim}{
vector giving the range of the x-axis.
}
  \item{ylim}{
vector giving the range of the y-axis.
}
  \item{zlim}{
vector giving the range of the y-axis.
}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{xlab}{
a title for the x axis: \code{\link{title}}. 
}
  \item{ylab}{
a title for the y axis: \code{\link{title}}.
}
  \item{zlab}{
a title for the z axis: \code{\link{title}}.
}
  \item{border}{
numerical value or character string giving the color of the line drawn around the surface facets.
}
  \item{sub}{
a sub title for the plot: \code{\link{title}}. 
}
  \item{form}{
a character string or a formula with the syntax \dQuote{y~ x+y + x*y}.\cr
If \code{form} is a character it has to be one out of the following: 
\itemize{ 
  \item \dQuote{quadratic}
  \item \dQuote{full}
  \item \dQuote{interaction}
  \item \dQuote{linear}
  \item \dQuote{fit}
  }  
\dQuote{fit} takes the formula from the fit in the facDesign object fdo. Quadratic or higher orders should be given as I(Variable^2).\cr
By default \code{form} is set as \dQuote{fit}.  
}
  \item{phi}{
numerical value. Angle (in degree) defining the viewing direction. Phi gives the colatitude.\cr
By default \code{phi} is set to \sQuote{30}.
}
  \item{theta}{
numerical value. Angle (in degree) defining the viewing direction. Theta gives the azimuthal direction.\cr
By default \code{theta} is set to \sQuote{-30}.
}
  \item{ticktype}{
graphical parameter. Character string specifying wheter the ticks on the axes should be \dQuote{simple} or \dQuote{detailed} (default).
}
  \item{col}{
a predefined (1, 2, 3 or 4) or self defined \code{\link{colorRampPalette}} or color to be used (i.e. \dQuote{red}).
}
  \item{steps}{
number of grid points per factor. By default \code{steps} = 25.   
}
  \item{factors}{
  list of 4th 5th factor with value i.e. factors = list(D = 1.2, E = -1), if nothing is specified  values will be the mean of the low and the high value of the factors.
}
  \item{fun}{
  function to be applied to z \dQuote{desirability}.     
}
  \item{plot}{
  logical value. If \sQuote{TRUE} (default) a plot is created (most likely to disappear with time)
}
}
\details{
This function can be used to display the desirability of each response by specifying fun = \dQuote{desirability} or the fun = \dQuote{overall} (i.e. composed) desirability of all responses. The required desirabilities can be set using \code{\link{desires}}.
}
\value{
The function \code{wirePlot()} returns an invisible list with the following entries:
 \itemize{
  \item x - locations of grid lines for x at which the values in z are measured
  \item y - locations of grid lines for y at which the values in z are measured
  \item z - a matrix containing the values of z to be plotted}
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
 For an example in context which shows the usage of the function \code{wirePlot()} to an object of class \code{\link{facDesign}}, 
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}
\seealso{
\code{\link{contourPlot}}\cr
\code{\link{filled.contour}}\cr
\code{\link{persp}}\cr
\url{http://www.r-qualitytools.org/Improve.html}\cr
\url{http://webapps.r-qualitytools.org/brew/facDesign/fracDesign.html}
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
