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
  \item{design}{design needs to be one of the following characters, which specifies the
                 orthogonal array of the taguchi design:
   \itemize{\item \dQuote{L4_2} for three two-level factors                  \cr
           \item \dQuote{L8_2} for seven two-level factors                   \cr
           \item \dQuote{L9_3} for four three-level factors                  \cr
           \item \dQuote{L12_2} for 11 two-level factors                     \cr
           \item \dQuote{L16_2} for 16 two-level factors                     \cr
           \item \dQuote{L16_4} for 16 four-level factors                    \cr
           \item \dQuote{L18_2_3} for one two-level and seven three-level factors \cr
           \item \dQuote{L25_5} for six five-level factors                   \cr
           \item \dQuote{L27_3} for 13 three-level factors                   \cr
           \item \dQuote{L32_2} for 32 two-level factors                     \cr    
           \item \dQuote{L32_2_4} for one two-level factor and nine four-level factors \cr
           \item \dQuote{L36_2_3_a} for 11 two-level factors and 12 three-level factors \cr
           \item \dQuote{L36_2_3_b} for three two-level factors and 13 three-level factors \cr
           \item \dQuote{L50_2_5} for one two-level factor and eleven five-level factors \cr
           \item \dQuote{L8_4_2} for one four-level factor and four two-level factors \cr
           \item \dQuote{L16_4_2_a} for one four-level factor and 12 two-level factors \cr
           \item \dQuote{L16_4_2_b} for two four-level factors and nine two-level factors \cr
           \item \dQuote{L16_4_2_c} for three four-level factors and six two-level factors \cr
           \item \dQuote{L16_4_2_d} for five four-level factors and two two-level factors \cr
           \item \dQuote{L18_6_3} for one six-level factors and six three-level factors \cr
}                   
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
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
Box, Bisgard S. G.E.P.; Fung, C. A.: An explanation and critique of taguchis
contributions to quality engineering. In: Quality and Reliability Engineering International (1988)
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

