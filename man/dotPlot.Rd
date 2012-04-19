\name{dotPlot}
\Rdversion{1.1}
\alias{dotPlot}
\title{
Function to create a dot plot 
}
\description{
dotPlot creates a dot plot. For data in groups the dotPlot is displayed stacked in one or not-stacked in different plot regions.
}
\usage{
dotPlot(x, group, xlim, ylim, col, xlab, ylab, pch, cex, breaks, 
        stacked = TRUE, ...)
}

\arguments{
  \item{x}{
vector with numeric values which should be plotted.
}
  \item{group}{
(optional) vector for grouping the values (see examples).
}
  \item{xlim}{
vector giving the lower and upper limit of the x-axis.
}
  \item{ylim}{
vector giving the lower and upper limit of the y-axis.
}
  \item{col}{
vector containing numeric values or strings for different colors for the groups in the dot plot.
}
  \item{xlab}{
a title for the x axis: \code{\link{title}}.
}
  \item{ylab}{
a title for the y axis: \code{\link{title}}.
}
  \item{pch}{
a vector of integers specifying symbols or a single character to be used for plotting points for the different groups in the dot plot.
}
  \item{cex}{
the amount by which points and symbols should be magnified relative to the default.
}
  \item{breaks}{
a vector giving the breakpoints for the binning of the values in x.
}
  \item{stacked}{
a logical vector specifying whether the single groups should be plotted in a stacked dot plot or in a single one.
By default \code{stacked} is set to \sQuote{TRUE}.
}
  \item{\dots}{
further graphical parameters (see \code{\link{par}})}.
}
\details{
values in x are assigned to the bins defined by breaks. The actual binning is done using hist.
}
\value{
\code{dotPlot} a invisible matrix containing NA's and numeric values representing values in a bin. The number of bins is given by the number of columns of the matrix.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\code{\link{hist}}\cr
\code{\link{boxplot}}\cr
\url{http://www.r-qualitytools.org/Analyze.html}
}
\examples{
#create some data and grouping
x = rnorm(28)
g = rep(1:2, 14)

#dot plot with groups and no stacking
dotPlot(x, group = g, stacked = FALSE, pch = c(19, 20), 
        main = "Non stacked dot plot")

#dot plot with groups and stacking
x = rnorm(28)
dotPlot(x, group = g, stacked = TRUE, pch = c(19, 20), 
        main = "Stacked dot plot")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
