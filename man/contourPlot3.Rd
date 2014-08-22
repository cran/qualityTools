\name{contourPlot3}
\alias{contourPlot3}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{
function to create a ternary plot (contour plot) for an object of class mixDesign
}
\description{
This function creates a ternary plot (contour plot) for mixture designs (i.e. object of class mixDesign)
}
\usage{
contourPlot3(x, y, z, response, data = NULL, main, xlab, ylab, zlab, border, 
             form = "linear", col = 1, col.text, cex.axis, axes = TRUE, 
             steps, factors)
}
\arguments{
  \item{x}{
factor 1 of the \code{\link{mixDesign}} object.
}
  \item{y}{
factor 2 of the \code{\link{mixDesign}} object.
}
  \item{z}{
factor 3 of the \code{\link{mixDesign}} object.
}
  \item{response}{
the response of the \code{\link{mixDesign}} object.
}
  \item{data}{
the \code{\link{mixDesign}} object from which x,y,z and the response are taken.
}
  \item{main}{
 giving an overall title for the plot: see \code{\link{title}}. 
}
  \item{xlab}{
 giving a title for the x axis : see \code{\link{title}}. 
}
  \item{ylab}{
 giving a title for the y axis : see \code{\link{title}}. 
}
  \item{zlab}{
 giving a title for the z axis : see \code{\link{title}}. 
}
  \item{border}{
 numeric or character (for example \dQuote{red}) value specifying the color of the surroundimg the ternary plot.\cr
 By default \code{border} is set to \dQuote{white}
}
  \item{form}{
a character string or a formula with the syntax \dQuote{y ~ A + B +C}.\cr
If \code{form} is a character string it has to be one out of the following: 
\itemize{ 
  \item \dQuote{linear}
  \item \dQuote{quadratic}
  \item \dQuote{fullCubic}
  \item \dQuote{specialCubic}
  }  
How the \code{form} influences the output is described in the reference listed below.\cr
By default \code{form} is set as \dQuote{linear}.
}
  \item{col}{
a predefined (1, 2, 3 or 4) or self defined \code{\link{colorRampPalette}}.
}
  \item{col.text}{
a numerical value or a character string (like \dQuote{red}) giving the color oft the inscription of the axes.\cr
The default value for \code{col.text} is \sQuote{1}.
}
  \item{cex.axis}{
numerical value giving the size of the inscription of the axes. \cr
The default value \code{cex.axis} is \sQuote{1}.
}
  \item{axes}{
logical value specifying whether the axes should be plotted or not. \cr
By default \code{axes} is set to \sQuote{TRUE}.
}
  \item{steps}{
resolution of the plot (number of rows for the square matrix), also number of grid points per factor.\cr
By default \code{steps} = 25.
}
  \item{factors}{
list of factors for categorizing with setting in case there are more than 3 factors (not yet implemented).
}
}
\value{
\code{contourPlot3} returns an invisible matrix containing the response values as NA's and numerics.
}
\references{
MYERS, Raymond H.; MONTGOMERY, Douglas C.; ANDERSON-COOK, Christine M.: Response Surface Methodology. New York: WILEY ,2009
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
This method is still under construction! \cr
For an example in context which shows the usage of the function \code{contourPlot3()} to an object of class \code{\link{mixDesign}}, 
please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}

\seealso{
\code{\link{wirePlot3}}  \cr
\code{\link{mixDesign}}  \cr
\code{\link{colorRampPalette}}  \cr
\url{http://www.r-qualitytools.org/Improve.html}\cr
\url{http://webapps.r-qualitytools.org/brew/facDesign/rsmDesign.html}
}
\examples{
#yarn example p.564 Response Surface Methodology
mdo = mixDesign(3,2, center = FALSE, axial = FALSE, randomize = FALSE, 
                replicates  = c(1,1,2,3))
names(mdo) = c("polyethylene", "polystyrene", "polypropylene")
units(mdo) = "percent"
elongation = c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 
               9.7, 11.8, 16.8, 16.0)
response(mdo) = elongation

dev.new(14,14); par(mfrow = c(2,2))
contourPlot3(A, B, C, elongation, data = mdo, form = "linear")
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = 2)
contourPlot3(A, B, C, elongation, data = mdo, 
             form = "elongation ~ I(A^2) - B:A + I(C^2)", col = 3)
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic", 
             col = colorRampPalette(c("yellow", "white", "red")))
}
