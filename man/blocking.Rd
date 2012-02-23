\name{blocking}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\Rdversion{1.1}
\alias{blocking}
\title{
Blocking
}
\description{
Blocks a given factorial or response surface design.
}
\usage{
blocking(fdo, blocks, BoR = FALSE, random.seed, useTable = "rsm", gen)
}
\arguments{
  \item{fdo}{
 needs to be an object of class \code{\link{facDesign}}.
}
  \item{blocks}{
 numerical value giving the number of blocks. 
}
  \item{BoR}{
 logical value indicating whether the replicates should be blocked or not. \cr
 By default \code{BoR} is set to \sQuote{FALSE}.
}
  \item{random.seed}{
 numerical variable for \code{\link{set.seed}} to generate repeatable results for randimization within blocks.
}
  \item{useTable}{
 character indicating which table to use. The follwing options will be accepted: \cr
 \itemize{ 
  \item \dQuote{rms} - table from reference
  \item \dQuote{calc} - table calculated by qualityTools
  }
}
  \item{gen}{
 giving the generator that will be used.
}
}
\value{
 The function \code{blocking()} returns a an object of class facDesign with blocking structure.
}
\references{
MYERS, Raymond H.; MONTGOMERY, Douglas C.; ANDERSON-COOK, Christine M.: Response Surface Methodology. New York: WILEY ,2009
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
\code{blocking} is still buggy at this time and Blocking on Replicates is not yet implemented and Blocking requires k >= 3 factors. This will change soon.\cr
 For an example in context which shows the effect of applying \code{blocking()} to 
 an object of class \code{\link{facDesign}}, please read the vignette for the package 
 \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}

\seealso{
\code{\link{facDesign}} \cr
\url{http://www.r-qualitytools.org/Improve.html}

}
\examples{
#create a 2^3 full factorial design
fdo = facDesign(k = 3)

#make it a design with 2 blocks
blocking(fdo, 2)

#create a response surface design for 3 factors
fdo = rsmDesign(k = 3)

#make it a design with 3 blocks (i.e. 1 block for star part and 2 blocks 
#for the cube part)
blocking(fdo, 3)
}
