\name{oaChoose}
\alias{oaChoose}
\title{
Taguchi Designs
}
\description{
Shows a matrix of possible taguchi designs
}
\usage{
oaChoose(factors1, factors2, level1, level2, ia)
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
\code{\link{oaChoose}} returns possible taguchi designs. Specifying the number of factor1 factors with level1 levels (factors1 = 2, level1 = 3 means 2 factors with 3 factor levels) and factor2 factors with level2 levels and desired interactions one or more taguchi designs are suggested. If all parameters are set to 0, a matrix of possible taguchi designs is shown.
}
\value{
an object of class \code{\link{taguchiDesign}}
}
\references{
TODO:
}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
TODO:
}

\seealso{
\code{\link{facDesign}} for 2^k factorial designs; \code{\link{rsmDesign}} for response surface designs; \code{\link{fracDesign}} for fractional factorial design; \code{\link{gageRRDesign}} for gage designs
}
\examples{
oaChoose()
}
\keyword{design}

