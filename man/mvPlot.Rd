\name{mvPlot}
\Rdversion{1.1}
\alias{mvPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multi-Vari Charts
}
\description{
Draws a Multi-Vari Chart for 2, 3 or 4 factors
}
\usage{
mvPlot(y, factor1, factor2, factor3, factor4, fun = mean, points = TRUE, connect = TRUE, col = c(1, 2, 3, 4), pch = c(1, 2, 3, 4), xlim, ylim, main, main.sub, horiz = FALSE, lwd.b = 1, lwd.w = 1, pch.b = 15, pch.w = 17, col.w = 2, col.b = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
the values o the response
}
  \item{factor1}{
settings of factor 1
}
  \item{factor2}{
settings of factor 2
}
  \item{factor3}{
settings of factor 3
}
  \item{factor4}{
settings of factor 4
}
  \item{fun}{
function to be used for calculation of y for unique settings of the factors e.g. the mean
}
  \item{points}{
show points
}
  \item{connect}{
are the means of each factor to be connected
}
  \item{col}{
graphical parameter
}
  \item{pch}{
graphical parameter

}
  \item{xlim}{
graphical parameter
}
  \item{ylim}{
graphical parameter
}
  \item{main}{
graphical parameter
}
  \item{main.sub}{
sub title
}
  \item{horiz}{
position of the legend
}
  \item{lwd.b}{
lwd (between)
}
  \item{lwd.w}{
lwd (within)
}
  \item{pch.b}{
pch (between)
}
  \item{pch.w}{
pch (within)
}
  \item{col.w}{
col (within)
}
  \item{col.b}{
col (between)
}
  \item{\dots}{
further graphical parameters see \code{\link{par}}
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
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
