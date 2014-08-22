\name{fracDesign}
\alias{fracDesign}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{fracDesign}
\description{Generates a 2^k full- or fractional factorial design.}

\usage{
fracDesign(k = 3, p = 0, gen = NULL, replicates = 1, blocks = 1, centerCube = 0,
           random.seed = 1234)
}
\arguments{
\item{k}{numeric value giving the number of factors. By default \code{k} is set to \sQuote{3}.}
\item{p}{numeric integer between \sQuote{0} and \sQuote{7}. \code{p} is giving the number of additional factors in the response surface design by aliasing effects.\cr
         A 2^k-p factorial design will be generated and the generators of the standard designs available in  \code{fracChoose()} will be used. \cr
         By default \code{p} is set to \sQuote{0}. Any other value will cause the function to omit the argument \code{gen} given by the user \cr
         and replace it by the one out of the table of standard designs (see: \code{fracChoose()}). Replicates and blocks can be set anyway!}
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
\code{\link{fracChoose}}\cr 
\code{\link{pbDesign}}\cr
\code{\link{rsmDesign}}\cr 
\code{\link{taguchiDesign}}\cr
\url{http://www.r-qualitytools.org/Improve.html}\cr
\url{http://webapps.r-qualitytools.org/brew/facDesign/fracDesign.html}
}
\examples{
#returns a 2^3 full factorial design
vp.full = facDesign(k = 3)       
#design in 2 blocks                           
vp.full = blocking(vp.full, 2)   
#generate some random response                           
response(vp.full) = rnorm(2^3)   
#summary of the full factorial design (especially no defining relation)                           
summary(vp.full)                                           

#returns a 2^4-1 fractional factorial design. Factor D will be aliased with
vp.frac = fracDesign(k = 4, gen = "D=ABC") 
#the three-way-interaction ABC (i.e. I = ABCD)                 
response(vp.frac) = rnorm(2^(4-1))    
#summary of the fractional factorial design                      
summary(vp.frac)                                            

#returns a full factorial design with 3 replications per factor combination 
#and 4 center points
vp.rep = fracDesign(k = 3, replicates = 3, centerCube = 4)  
#summary of the replicated fractional factorial Design
summary(vp.rep)                                             
}