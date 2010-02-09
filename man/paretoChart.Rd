\name{paretoChart}
\alias{paretoChart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pareto Chart
}
\description{
function to create a pareto chart
}
\usage{
paretoChart(x, weight, showTable = TRUE, las = 0, main, col, border, xlab, ylab = "Frequency", percentVec, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of qualitative values
}
  \item{weight}{
vector with weights for x (not yet implemented)
}
  \item{showTable}{
TRUE/FALSE  -  frequency table within window
}
  \item{las}{
graphical parameter
}
  \item{main}{
graphical parameter
}
  \item{col}{
graphical parameter
}
  \item{border}{
graphical parameter
}
  \item{xlab}{
graphical parameter
}
  \item{ylab}{
graphical parameter
}
  \item{percentVec}{
numeric - tick marks for percentage axis
}
  \item{\dots}{
further graphical parameters
}
}
\value{
data.frame of percentages}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}


\examples{
defects = LETTERS[runif(80, 1, 10)]#artifical defects dataset
paretoChart(defects) #paretoChart of the defects
}

