\name{taguchiChoose}
\alias{taguchiChoose}
\keyword{Design of Experiments}
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
integer number of factors on level1. By default set to \sQuote{0}.
}
  \item{factors2}{
integer number of factors on level2. By default set to \sQuote{0}.
}
  \item{level1}{
integer number of levels for factors1.
}
  \item{level2}{
integer number of levels for factors2. By default set to \sQuote{0}
}
  \item{ia}{
integer number of interactions. . By default set to \sQuote{0}.
}
}
\details{
\code{\link{taguchiChoose}} returns possible taguchi designs.\cr
Specifying the number of factor1 factors with level1 levels (factors1 = 2, level1 = 3 means 2 factors with 3 factor levels) and factor2 factors with level2 levels and desired interactions one or more taguchi designs are suggested.\cr
If all parameters are set to 0, a matrix of possible taguchi designs is shown.
}
\value{
\code{taguchiChoose} returns an object of class \code{\link{taguchiDesign}}.
}
\references{
\url{http://www.york.ac.uk/depts/maths/tables/taguchi_table.htm}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
It is recommended to consult a textbook on taguchi methods to be fully aware of the benefits and limitations that taguchi designs impose.
}

\seealso{
\code{\link{facDesign}} for 2^k factorial designs\cr
\code{\link{rsmDesign}} for response surface designs\cr
\code{\link{fracDesign}} for fractional factorial design\cr
\code{\link{gageRRDesign}} for gage designs            \cr
\url{http://www.r-qualitytools.org}\cr
\url{http://webapps.r-qualitytools.org/brew/taguchi/taguchi.html}
}
\examples{
taguchiChoose()
}
\keyword{design}

