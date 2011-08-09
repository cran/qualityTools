\name{rsmDesign}
\Rdversion{1.1}
\alias{rsmDesign}

\title{
Generate a response surface design (i.e. central composite design)
}
\description{
Generates a response surface design containing a cube, centerCube, star and centerStar portion.
}
\usage{
rsmDesign(k = 3, p = 0, alpha = "rotatable", blocks = 1, cc = 1, cs = 1,  fp = 1, sp = 1, faceCentered = FALSE)
}
\arguments{
  \item{k}{
number of factors
}
  \item{p}{
number of additional factors in the response surface design by aliasing effects
}
  \item{alpha}{
value of alpha "rotatable", "orthogonal" or "both". If "both" values for cc and cs will be DISCARDED
}
\item{blocks}{
number of blocks in the response surface design
}
\item{cc}{
number of centerpoints (per block) in the cube portion (i.e. the factorial 2^k design) of the response surface design. Replications of centerpoints in the cube portion can be set with this.
}
\item{cs}{
number of centerpoints in the star portion (alpha) of the response surface design.
Replications of centerpoints in the star portion can be set with this.
}
\item{fp}{
number of replications per factorial point (i.e. corner points e.g. (-1,1)) 
}

\item{sp}{
number of replications per star point (i.e. alpha) 
}

\item{faceCentered}{
faceCentered response surface design or not (i.e. alpha = 1)
}
}
\details{
Generated designs consist of a cube, centerCube, star and a centerStar portion. The replication structure can be set with the parameters cc (centerCube), cs (centerStar), fp (factorialPoints) and sp (starPoints). 
}
\value{
object of class facDesign
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}


\seealso{
\code{\link{facDesign}}
\code{\link{fracDesign}}
\code{\link{fracChoose}}
\code{\link{rsmChoose}}
}
\examples{
#central composite design for 2 factors with 2 blocks, alpha = 1.41, 5 centerpoints in the cube portion and 3 centerpoints in the star portion
rsmDesign(k = 2, blocks = 2, alpha = sqrt(2),cc = 5, cs = 3)

#central composite design with both, orthogonality and near rotatability 
rsmDesign(k = 2, blocks = 2, alpha = "both")

#central composite design with
#2 centerpoints in the factorial portion of the design i.e 2
#1 centerpoint int the star portion of the design i.e. 1
#2 replications per factorial point i.e. 2^3*2 = 16
#3 replications per star points 3*2*3 = 18
#makes a total of 37 factor combinations
rsdo = rsmDesign(k = 3, blocks = 1, alpha = 2, cc = 2, cs = 1, fp = 2, sp = 3)
nrow(rsdo) #37


}
