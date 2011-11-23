\name{fracDesign}
\alias{fracDesign}
\title{fracDesign}
\description{Generates a 2^k full- or fractional factorial design.}

\usage{
fracDesign(k = 3, gen = NULL, replicates = 1, blocks = 1, centerCube = 0, random.seed = 1234)
}
\arguments{
\item{k}{numeric value giving the number of factors. By default \code{k} is set to \sQuote{3}.}
\item{gen}{one or more defining relations for a fractional factorial design. By default \code{gen} is set to \sQuote{NULL}.}
\item{replicates}{numeric value giving the number of replicates per factor combination. By default \code{replicates} is set to \sQuote{1}.}
\item{blocks}{numeric value giving the number of blocks. By default \code{blocks} is set to \sQuote{1}.}
\item{centerCube}{numeric value giving the number of centerpoints within the 2^k design. By default \code{centerCube} is set to \sQuote{0}.}
\item{random.seed}{seed for randomization of the design}
}
\details{
fracDesign generates 2^k full- or fractional factorial designs.
}
\value{\code{fracDesign} returns an object of class facDesign.}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
 For an example in context which shows the usage of the function \code{fracDesign()}  
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}
\seealso{
\code{\link{facDesign}}\cr 
\code{\link{rsmDesign}}\cr 
\code{\link{taguchiDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}}
\examples{
vp.full = facDesign(k = 3)                                  #returns a 2^3 full factorial design
vp.full = blocking(vp.full, 2)                              #design in 2 blocks
response(vp.full) = rnorm(2^3)                              #generate some random response
summary(vp.full)                                            #summary of the full factorial design (especially no defining relation)

vp.frac = fracDesign(k = 4, gen = "D=ABC")                  #returns a 2^4-1 fractional factorial design. Factor D will be aliased with
response(vp.frac) = rnorm(2^(4-1))                          #the three-way-interaction ABC (i.e. I = ABCD)
summary(vp.frac)                                            #summary of the fractional factorial design

vp.rep = fracDesign(k = 3, replicates = 3, centerCube = 4)  #returns a full factorial design with 3 replications per factor combination and 4 center points
summary(vp.rep)                                             #summary of the replicated fractional factorial Design
}