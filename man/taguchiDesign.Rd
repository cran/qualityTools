\name{taguchiDesign}
\alias{taguchiDesign}

\title{
Taguchi Designs
}
\description{
Creates a taguchi design 
}
\usage{
taguchiDesign(design, randomize = TRUE, replicates = 1)
}

\arguments{
  \item{design}{
character - L4_2 L8_2 L9_3 L12_2 L16_2 L16_4 L18_2_3 L25_5 L27_3 L32_2 L32_2_4 L36_2_3_a L36_2_3_b L50_2_5 L8_4_2 L16_4_2_a L16_4_2_b L16_4_2_c L16_4_2_d L18_6_3
}
  \item{randomize}{
TRUE/FALSE - randomizes the RunOrder of the design
}
  \item{replicates}{
Integer - number of replicates
}
}
\details{
an overview of possible taguchi designs is possible with \code{\link{taguchiChoose}}
}
\value{
an object of class \link{taguchiDesign}
}
\references{
TODO:

}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
TODO: Plackett Burmann, Fisher and Rao Note
}



\seealso{
\code{\link{facDesign}} for 2^k factorial designs; \code{\link{rsmDesign}} for response surface designs; \code{\link{fracDesign}} for fractional factorial design; \code{\link{gageRRDesign}} for gage designs
}
\examples{
tdo = taguchiDesign("L9_3")
values(tdo) = list(A = c("material 1","material 2","material 3"), B = c(29, 30, 35))
names(tdo) = c("Factors", "Are", "Documented", "In The Design")
response(tdo) = rnorm(9)
summary(tdo)
effectPlot(tdo)



}
\keyword{design}

