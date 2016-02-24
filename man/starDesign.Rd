\name{starDesign}
\alias{starDesign}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{
Axial Design
}
\description{
starDesign is a function to create the star portion of a response surface design. The \code{\link{starDesign}} function can be used to create a star portion of a response surface design for a sequential assembly strategy.\cr
One can either specify k and p and alpha and cs and cc OR simply simply pass an object of class \code{\link{facDesign}} to the data. In the latter an object of class \code{\link{facDesign}} otherwise a list containing the axial runs and centerpoints is returned.
}
\usage{
starDesign(k, p = 0, alpha = c("both", "rotatable", "orthogonal"), cs, cc, data)
}

\arguments{
  \item{k}{
integer value giving number of factors.
}
  \item{p}{
integer vaue giving the number of factors via aliasing.\cr
By default set to \sQuote{0}.
}
  \item{alpha}{
if no numeric value is given defaults to \dQuote{both} i.e. \dQuote{orthogonality} and \dQuote{rotatibility} which can be set as character strings too.
}
  \item{cs}{
integer value giving the number of centerpoints in the star portion of the design.
}
  \item{cc}{
integer value giving the number of centerpoints in the cube portion of the design.
}
  \item{data}{
optional - an object of class \code{\link{facDesign}}.
}
}
\value{
\code{starDesign} returns a \code{\link{facDesign}} object if an object of class \code{\link{facDesign}} is given
or a list containing entries for axial runs and center points in the cube and the star portion of a design.
}
\references{
MYERS, R.H.; MONTGOMERY, D.C.; ANDERSON-COOK, C.M.: Response Surface Methodology. New Jersey: Wiley,2009.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\code{\link{facDesign}} for 2^k factorial designs\cr
\code{\link{fracDesign}} for 2^k-p fractional factorial designs\cr
\code{\link{rsmDesign}} for response surface designs\cr
\code{\link{mixDesign}} for mixture designs\cr
\code{http://www.r-qualitytools.org}
}
\examples{
#Example 1 - sequential assembly
#factorial design with one center point in the cube portion
fdo = facDesign(k = 3, centerCube = 1)  
fdo

#set the response via generic response method
response(fdo) = 1:9 
 
#sequential assembly of a response surface design (rsd)
rsd = starDesign(data = fdo)  
rsd

#Example 2 - returning a list
starDesign(k = 3, cc = 2, cs = 2, alpha = "orthogonal")
starDesign(k = 3, cc = 2, cs = 2, alpha = "rotatable")
starDesign(k = 3, cc = 2, cs = 2, alpha = "both")
}
\keyword{design}

