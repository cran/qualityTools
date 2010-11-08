\name{starDesign}
\alias{starDesign}
\title{
Axial Design
}
\description{
starDesign is a function to create the star portion of a response surface design. The \code{\link{starDesign}} function can be used to create a star portion of a response surface design for a sequential assembly strategy. One can either specify k and p and alpha and cs and cc OR simply simply pass an object of class \code{\link{facDesign}} to the data. In the latter an object of class facDesign otherwise a list containing the axial runs and centerpoints is returned.
}
\usage{
starDesign(k, p = 0, alpha = c("both", "rotatable", "orthogonal"), cs, cc, data)
}

\arguments{
  \item{k}{
integer - number of factors
}
  \item{p}{
integer - number of factors via aliasing
}
  \item{alpha}{
if no numeric value is given defaults to both i.e. orthogonality and rotatibility
}
  \item{cs}{
integer - number of centerpoints in the star portion of the design
}
  \item{cc}{
integer - number of centerpoints in the cube portion of the design
}
  \item{data}{
optional - an object of class facDesign
}
}
\value{
a \code{\link{facDesign}} object if an object of class \code{\link{facDesign}} is given
or a list containing entries for axial runs and center points in the cube and the star portion of a design
}
\references{
Response Surface Methodology (2009) Myers, Montgomery, Anderson-Cook
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\code{\link{facDesign}} for 2^k factorial designs,\code{\link{fracDesign}} for 2^k-p fractional factorial designs,\code{\link{rsmDesign}} for response surface designs, \code{\link{mixDesign}} for mixture designs or \code{\link{taguchiDesign}} for taguchi designs
}
\examples{
#Example 1 - sequential assembly
fdo = facDesign(k = 3, centerCube = 1)  #factorial design with one center point in the cube portion
fdo
response(fdo) = 1:9  #set the response via generic response method
rsd = starDesign(data = fdo)  #sequential assembly of a response surface design (rsd)
rsd

#Example 2 - returning a list
starDesign(k = 3, cc = 2, cs = 2, alpha = "orthogonal")
starDesign(k = 3, cc = 2, cs = 2, alpha = "rotatable")
starDesign(k = 3, cc = 2, cs = 2, alpha = "both")
}
\keyword{design}

