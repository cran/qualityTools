\name{facDesign}
\alias{facDesign}
\title{facDesign}
\description{Generates a 2^k full factorial design.}

\usage{
facDesign(k = 3, replicates = 1, blocks = 1, centerCube = 0)
}
\arguments{
\item{k}{numeric value giving the number of factors. By default \code{k} is set to \sQuote{3}.}
\item{replicates}{numeric value giving the number of replicates per factor combination. By default \code{replicates} is set to \sQuote{1}.}
\item{blocks}{numeric value giving the number of blocks. By default \code{blocks} is set to \sQuote{1}. Blocking is only performed for \code{k} greater 2.}
\item{centerCube}{numeric value giving the number of centerpoints within the 2^k design. By default \code{centerCube} is set to \sQuote{0}.}
}
\details{
\code{facDesign} generates 2^k full factorial designs.
}
\value{The function \code{\link{facDesign}} returns an object of class \code{\link{facDesign}}.}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{For an example in context which shows the usage of the function \code{\link{facDesign}} 
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.}
\seealso{
\code{\link{fracDesign}}\cr
\code{\link{rsmDesign}}\cr
\code{\link{taguchiDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}}
\examples{
vp.full = facDesign(k = 3)                                 #returns a 2^3 full factorial design
response(vp.full) = rnorm(2^3)                             #generate some random response
summary(vp.full)                                           #summary of the full factorial design (especially no defining relation)

#------------

vp.rep = facDesign(k = 2, replicates = 3, centerCube = 4)  #returns a full factorial design with 3 replications per factor combination and 4 center points
names(vp.rep) = c("Name 1", "Name 2")                      #set names
units(vp.rep) = c("min", "F")                              #set units
lows(vp.rep) = c(20, 40, 60)                               #set low and high factor values
highs(vp.rep) = c(40, 60, 80)         
summary(vp.rep)                                            #summary of the replicated full factorial Design     
}