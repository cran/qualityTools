\name{contourPlot3}
\alias{contourPlot3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to create a ternary plot (contour plot) for an object of class mixDesign
}
\description{
This function creates a ternary plot (contour plot) for mixture designs (i.e. object of class mixDesign)
}
\usage{
contourPlot3(x, y, z, response, data = NULL, xlim, ylim, zlim, main, xlab, ylab, zlab, border, sub, form = "linear", phi, theta, ticktype, col = 1, col.text, cex.axis, axes = TRUE, steps, factors, fun, plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
factor 1 of the \code{\link{mixDesign}} object
}
  \item{y}{
factor 2 of the \code{\link{mixDesign}} object
}
  \item{z}{
factor 3 of the \code{\link{mixDesign}} object
}
  \item{response}{
the response of the \code{\link{mixDesign}} object
}
  \item{data}{
the \code{\link{mixDesign}} object
}
  \item{xlim}{

}
  \item{ylim}{

}
  \item{zlim}{

}
  \item{main}{

}
  \item{xlab}{

}
  \item{ylab}{

}
  \item{zlab}{

}
  \item{border}{

}
  \item{sub}{

}
  \item{form}{
"linear", "quadratic", "fullCubic", "specialCubic", or formula such as y ~ A + B + C
}
  \item{phi}{

}
  \item{theta}{

}
  \item{ticktype}{

}
  \item{col}{
a color or color.palette
}
  \item{col.text}{

}
  \item{cex.axis}{

}
  \item{axes}{

}
  \item{steps}{
resolution of the plot (number of rows for the square matrix)
}
  \item{factors}{
list of factors for categorizing with setting in case there are more than 3 factors (not yet implemented)
}
  \item{fun}{

}
  \item{plot}{
if FALSE no plot is being produced
}
}
\value{
a matrix containing NA's and numerics}
\references{
Myers; Montgomery; Anderson-Cook (2009) Response Surface Methodology - Wiley
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
this method is still under construction
}

\seealso{
\code{\link{wirePlot3}}
}
\examples{
#yarn example p.564 Response Surface Methodology
mdo = mixDesign(3,2, center = FALSE, axial = FALSE, randomize = FALSE, replicates  = c(1,1,2,3))
names(mdo) = c("polyethylene", "polystyrene", "polypropylene")
units(mdo) = "percent"
elongation = c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 9.7, 11.8, 16.8, 16.0)
response(mdo) = elongation

dev.new(14,14); par(mfrow = c(2,2))
contourPlot3(A, B, C, elongation, data = mdo, form = "linear")
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = 2)
contourPlot3(A, B, C, elongation, data = mdo, form = "elongation ~ I(A^2) - B:A + I(C^2)", col = 3)
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic", col = colorRampPalette(c("yellow", "white", "red")))

}
