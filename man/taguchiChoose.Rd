\name{taguchiChoose}
\alias{taguchiChoose}
\title{
Taguchi Designs
}
\description{
Shows a matrix of possible taguchi designs
}
\usage{
taguchiChoose(factors1 = 0, factors2 = 0, level1 = 0, level2 = 0, ia = 0)
}
\arguments{
  \item{factors1}{
number of factors on level1
}
  \item{factors2}{
number of factors on level2
}
  \item{level1}{
number of levels for factors1
}
  \item{level2}{
number of levels for factors2
}
  \item{ia}{
number of interactions
}
}
\details{
\code{\link{taguchiChoose}} returns possible taguchi designs. Specifying the number of factor1 factors with level1 levels (factors1 = 2, level1 = 3 means 2 factors with 3 factor levels) and factor2 factors with level2 levels and desired interactions one or more taguchi designs are suggested. If all parameters are set to 0, a matrix of possible taguchi designs is shown.
}
\value{
an object of class \code{\link{taguchiDesign}}
}
\references{
\url{http://www.york.ac.uk/depts/maths/tables/taguchi_table.htm}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
It is recommended to consult a textbook on taguchi methods to be fully aware of the benefits and limitations that taguchi designs impose.
}

\seealso{
\code{\link{facDesign}} for 2^k factorial designs; \code{\link{rsmDesign}} for response surface designs; \code{\link{fracDesign}} for fractional factorial design; \code{\link{gageRRDesign}} for gage designs
}
\examples{
taguchiChoose()
}
\keyword{design}

