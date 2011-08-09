\name{facDesign}
\alias{facDesign}
\title{facDesign}
\description{Generates a 2^k full factorial design.}

\usage{
facDesign(k = 3, replicates = 1, blocks = 1, centerCube = 0)
}
\arguments{
\item{k}{number of factors}
\item{replicates}{number of replicates per factor combination}
\item{blocks}{number of blocks}
\item{centerCube}{number of centerpoints within the 2^k design}
}
\details{
facDesign generates 2^k full factorial designs.
}
\value{an object of class facDesign}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{\code{\link{facDesign}}, \code{\link{rsmDesign}}, \code{\link{taguchiDesign}}}
\examples{
vp.full = facDesign(k = 3)                  #returns a 2^3 full factorial design
response(vp.full) = rnorm(2^3)              #generate some random response
summary(vp.full)                            #summary of the full factorial design (especially no defining relation)

#------------

vp.rep = facDesign(k = 2, replicates = 3, centerCube = 4)  #returns a full factorial design with 3 replications per factor combination and 4 center points
names(vp.rep) = c("Name 1", "Name 2") #set names
units(vp.rep) = c("min", "F")        #set units
lows(vp.rep) = c(20, 40, 60)          #set low and high factor values
highs(vp.rep) = c(40, 60, 80)         
summary(vp.rep)                       #summary of the replicated full factorial Design

}