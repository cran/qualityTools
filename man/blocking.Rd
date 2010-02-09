\name{blocking}
\Rdversion{1.1}
\alias{blocking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Blocking
}
\description{
Blocks a given factorial or response surface design.
}
\usage{
blocking(fdo, blocks, BoR = FALSE, random.seed, useTable = "rsm", gen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fdo}{
an object of class facDesign
}
  \item{blocks}{
number of blocks 
}
  \item{BoR}{
blocking on replicates (TRUE/FALSE)
}
  \item{random.seed}{
random.seed for randimization within blocks
}
  \item{useTable}{
which table to use \dQuote{rms} - table from first reference or \dQuote{calc} - table calculated by qualityTools 
}

  \item{gen}{
generator to use
}
}
\value{
an object of class facDesign with blocking structure
}
\references{
H. MYERS, Raymond; C. MONTGOMERY, Douglas; M.ANDERSON-COOK, Christine: Response Surface Methodology. WILEY 3rd ed. (2009)
}

\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
\code{blocking} is still buggy at this time and Blocking on Replicates is not yet implemented and Blocking requires k >= 3 factors. This will change soon.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{facDesign}}
}
\examples{

#create a 2^3 full factorial design
fdo = facDesign(k = 3)

#make it a design with 2 blocks
blocking(fdo, 2)

#create a response surface design for 3 factors
fdo = rsmDesign(k = 3)

#make it a design with 3 blocks (i.e. 1 block for star part and 2 blocks for the cube part)
blocking(fdo, 3)
}
