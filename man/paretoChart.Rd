\name{paretoChart}
\alias{paretoChart}
\title{
Pareto Chart
}
\description{
function to create a pareto chart.
}
\usage{
paretoChart(x, weight, showTable = TRUE, las = 0, main, col, border, xlab, 
            ylab = "Frequency", percentVec, ...)
}
\arguments{
  \item{x}{
vector of qualitative values.
}
  \item{weight}{
vector with weights for x (not yet implemented!)
}
  \item{showTable}{
logical value specifing whether a frequeny table will be added above the graph. \cr
By default \code{showTable} is set to \sQuote{TRUE}.
}
  \item{las}{
graphical parameter. Numeric in {0,1,2,3} giving the style of axis labels (see: \code{\link{par}}).
}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{col}{
numerical value or character string defining the fill-color of the bars.
}
  \item{border}{
numerical value or character string defining the border-color of the bars.
}
  \item{xlab}{
a title for the x axis: \code{\link{title}}. 
}
  \item{ylab}{
a title for the y axis: \code{\link{title}}. 
}
  \item{percentVec}{
numerical vector giving the position and values of tick marks for percentage axis.
}
  \item{\dots}{
further graphical parameters.
}
}
\value{
\code{paretoChart} returns a \code{\link{data.frame}} of percentages.}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
 For an example which shows the usage of the function \code{paretoChart()}
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Define.html}.
}
\seealso{\url{http://webapps.r-qualitytools.org/brew/paretoChart/paretoChart.html}}
\examples{

#artifical defects dataset
defects = LETTERS[runif(80, 1, 10)]
#paretoChart of the defects
paretoChart(defects) 
}

