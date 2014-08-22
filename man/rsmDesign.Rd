\name{rsmDesign}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\Rdversion{1.1}
\alias{rsmDesign}

\title{
Generate a response surface design (i.e. central composite design)
}
\description{
Generates a response surface design containing a cube, centerCube, star and centerStar portion.
}
\usage{
rsmDesign(k = 3, p = 0, alpha = "rotatable", blocks = 1, cc = 1, cs = 1,  
          fp = 1, sp = 1, faceCentered = FALSE)
}
\arguments{
  \item{k}{
integer value giving the number of factors. By default \code{k} has the value \sQuote{3}.
}
  \item{p}{
integer value giving the number of additional factors in the response surface design by aliasing effects.\cr
By default \code{p} has the value \sQuote{0}.
}
  \item{alpha}{
character string - alpha shoud be \dQuote{rotatable} (default), \dQuote{orthogonal} or \dQuote{both}. If \dQuote{both} values for cc and cs will be DISCARDED.
}
\item{blocks}{
integer value giving the number of blocks in the response surface design.\cr
By default \code{blocks} has the value \sQuote{1}.
}
\item{cc}{
integer value giving the number of centerpoints (per block) in the cube portion (i.e. the factorial 2^k design) of the response surface design.\cr
Replications of centerpoints in the cube portion can be set with this.By default \code{cc} has the value \sQuote{1}.
}
\item{cs}{
integer value giving the number of centerpoints in the star portion (alpha) of the response surface design.\cr
Replications of centerpoints in the star portion can be set with this. By default \code{cs} has the value \sQuote{1}.
}
\item{fp}{
integer value giving the number of replications per factorial point (i.e. corner points e.g. (-1,1)).\cr
By default \code{fp} has the value \sQuote{1}. 
}
\item{sp}{
integer value giving the number of replications per star point (i.e. alpha).\cr
By default \code{sp} has the value \sQuote{1}. 
}
\item{faceCentered}{
logical value wheter to use a faceCentered response surface design or not (i.e. alpha = 1).\cr
By default \code{faceCentered} has the value \sQuote{FALSE}.
}
}
\details{
Generated designs consist of a cube, centerCube, star and a centerStar portion. The replication structure can be set with the parameters cc (centerCube), cs (centerStar), fp (factorialPoints) and sp (starPoints). 
}
\value{
\code{rsmDesign} returns an object of class \code{\link{facDesign}}.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
 For an example in context which shows the usage of the function \code{rsmDesign()}
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}

\seealso{
\code{\link{facDesign}}\cr
\code{\link{fracDesign}}\cr
\code{\link{fracChoose}}\cr 
\code{\link{pbDesign}}\cr
\code{\link{rsmChoose}}\cr
\url{http://www.r-qualitytools.org/Improve.html}\cr
\url{http://webapps.r-qualitytools.org/brew/facDesign/rsmDesign.html}
}
\examples{
#central composite design for 2 factors with 2 blocks, alpha = 1.41, 
#5 centerpoints in the cube portion and 3 centerpoints in the star portion:
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
