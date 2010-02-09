\name{dotPlot}
\Rdversion{1.1}
\alias{dotPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to create a dot plot 
}
\description{
dotPlot creates a dot plot. For data in groups the dotPlot is displayed stacked in one or not-stacked in different plot regions .
}
\usage{
dotPlot(x, group, xlim, ylim, col, xlab, ylab, pch, cex, breaks, stacked = TRUE, ...)
}

\arguments{
  \item{x}{
numeric values
}
  \item{group}{
grouping vector
}
  \item{xlim}{
limits of the x-axis
}
  \item{ylim}{
limits of the y-axis
}
  \item{col}{
colors for the different groups in the dot plot
}
  \item{xlab}{
a title for the x axis
}
  \item{ylab}{
a title for the y axis
}
  \item{pch}{
an integer specifying a symbol or a single character to be used for plotting points for the different groups in the dot plot
}
  \item{cex}{
the amount by which points and symbols should be magnified relative to the default
}
  \item{breaks}{
a vector giving the breakpoints for the binning of the values in x
}
  \item{stacked}{
stacked dot plot or not
}
  \item{\dots}{
further graphical parameters (see \code{\link{par}})}
}
\details{
values in x are assigned to the bins defined by breaks. The actual binning is done using hist.
}
\value{
a matrix containing NA's and numeric values representing values in a bin. The number of bins is given by the number of columns of the matrix
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hist}},
\code{\link{boxplot}}
}
\examples{
#create some data and grouping
x = rnorm(28)
g = rep(1:2, 14)

#dot plot with groups and no stacking
dotPlot(x, group = g, stacked = FALSE, pch = c(19, 20), main = "Non stacked dot plot")

#dot plot with groups and stacking
x = rnorm(28)
dotPlot(x, group = g, stacked = TRUE, pch = c(19, 20), main = "Stacked dot plot")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
