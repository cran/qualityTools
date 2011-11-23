\name{mvPlot}
\Rdversion{1.1}
\alias{mvPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multi-Vari Charts
}
\description{
Draws a Multi-Vari Chart for 2, 3 or 4 factors.
}
\usage{
mvPlot(y, factor1, factor2, factor3, factor4, fun = mean, points = TRUE, connect = TRUE, col = c(1, 2, 3, 4), pch = c(1, 2, 3, 4), 
       xlim, ylim, main, main.sub, horiz = FALSE, lwd.b = 1, lwd.w = 1, pch.b = 15, pch.w = 17, col.w = 2, col.b = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
the values of the response in a vector.
}
  \item{factor1}{
vector providing factor 1 as shown in the example.
}
  \item{factor2}{
vector providing factor 2 as shown in the example.
}
  \item{factor3}{
vector providing factor 3 as shown in the example.
}
  \item{factor4}{
vector providing factor 4 as shown in the example.
}
  \item{fun}{
function to be used for calculation of y for unique settings of the factors e.g. the \code{\link{mean}} (default setting).
}
  \item{points}{
logical value deciding whether the points will be shown or not.\cr
By default \code{points} is set to \sQuote{TRUE}.
}
  \item{connect}{
logical value specifying whether the means of each factor will be connected or not.\cr
By default \code{connect} is set to \sQuote{TRUE}.
}
  \item{col}{
graphical parameter. Vector containing numerical values or character strings giving the colors for the different factors.\cr
By default \code{col} is set to \sQuote{c(1,2,3,4)}.
}
  \item{pch}{
graphical parameter. Vector containing numerical values or single characters giving plotting points for the different factors.\cr
See \code{\link{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).\cr
By default \code{pch} is set to \sQuote{c(1,2,3,4)}.
}
  \item{xlim}{
graphical parameter. The x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a \sQuote{reversed axis}.
}
  \item{ylim}{
graphical parameter. The y limits of the plot.
}
  \item{main}{
graphical parameter. A main title for the plot, see also \code{\link{title}}.
}
  \item{main.sub}{
graphical parameter. A sub title for the plot (character string).
}
  \item{horiz}{
logical value specifying where the legend will be plotted. If set to \sQuote{TRUE} the legend will appear above the main plot,\cr
if \sQuote{FALSE} (default) the legend will be placed next to the main plot at the right side.
}
  \item{lwd.b}{
numerical value giving the width of the line between the mean values of all groups.
}
  \item{lwd.w}{
numerical value giving the width of the line within the mean of the values of one group.
}
  \item{pch.b}{
numerical value or single character giving the type of point for the mean values of all groups.
}
  \item{pch.w}{
numerical value or single character giving the type of point for the mean values of the single groups.
}
  \item{col.w}{
numerical value or character string giving the color of points for the mean values of the single groups.
}
  \item{col.b}{
numerical value or character string giving the color of points for the mean values of all groups.
}
  \item{\dots}{
further graphical parameters see \code{\link{par}}.
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\url{http://www.r-qualitytools.org}
}

\examples{
#generate some data
factor1 = c(rep(1, 9), rep(2, 9), rep(3,9))
factor2 = c(rep(1,9), rep(2,9), rep(3,9))
temp = expand.grid(c(1,2,3), c(1,2,3))
temp = as.data.frame(rbind(temp, temp, temp))
temp = cbind(temp, rnorm(27, 20, 2))
names(temp) = c("factor1", "factor2", "response")

#have a look at the design matrix
temp

mvPlot(y = temp[,3], factor1 = temp[,1], factor2 = temp[,2], col.b = "red3", horiz = FALSE)

}
