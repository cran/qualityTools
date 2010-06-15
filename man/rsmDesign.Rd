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
rsmDesign(k = 3, p = 0, alpha = "rotatable", blocks = 1, cc = 1, cs = 1, faceCentered = FALSE)
}
\arguments{
  \item{k}{
number of factors
}
  \item{p}{
number of additional factors in the response surface design by aliasing effects
}
  \item{alpha}{
value of alpha "rotatable", "orthogonal" or "both". If "both" values for cc and cs will be discarded
}
  \item{blocks}{
number of blocks in the response surface design
}
  \item{cc}{
number of centerpoints (per block) in the cube portion of the response surface design
}
  \item{cs}{
number of centerpoints in the star portion of the response surface design
}
  \item{faceCentered}{
faceCentered response surface design or not (i.e. alpha = 1)
}
}
\details{
Generated designs consist of a cube, centerCube, star and a centerStar portion.
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
}
