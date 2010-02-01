\name{mixDesign}
\Rdversion{1.1}
\alias{mixDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mixture Designs
}
\description{
function to generate simplex lattice and simplex centroid mixture designs with optional center points and axial points
}
\usage{
mixDesign(p, n = 3, type = "lattice", center = TRUE, axial = FALSE, delta, replicates = 1, lower, total = 1, randomize, seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
number of factors
}
  \item{n}{
the degree (ignored if type = "centroid")
}
  \item{type}{
\dQuote{lattice} or \dQuote{centroid}
}
  \item{center}{
TRUE or FALSE for adding center points 
}
  \item{axial}{
TRUE or FALSE for adding axial points
}
  \item{delta}{
delta for axial runs 
}
  \item{replicates}{
vector with the number of replicates for the different design points i.e. c(center = 1, axial = 1, pureBlend = 1, BinaryBlend = 1, p-3 blend, p-2 blend, p-1 blend)
}
  \item{lower}{
vector of lower-Bound Constraints on the Component Proportions (i.e. must be given in percent )
}
  \item{total}{
vector with 
[1] the percentage of the mixture made up by the q - components (e.g. q = 3 and x1 + x2 + x3 = 0.8 --> total = 0.8 with 0.2 for the other factors being held constant)
[2] overall total in corresponding units (e.g. 200ml for the overall mixture)
}
  \item{randomize}{
TRUE or FALSE, if TRUE the RunOrder of the mixture design will be randomized
}
  \item{seed}{
seed for randomization
}
}

\value{
an object of class mixDesign
}
\references{
Cornell (2002). Experiments with Mixtures. Third Edition. Wiley.
Chasalow, S; Brand, R (1995) Generation of Simplex Lattice Points. Journal of the Royal Statistical Society. Series C (Applied Statistics), Vol. 44, No. 4 (1995), pp. 534-545
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\note{
In this version the creation of (augmented) lattice, centroid mixture designs is fully supported. Getters and Setter methods  for the mixDesign object exist just as for objects of class facDesign (i.e. factorial designs). The creation of constrained component proportions is partially supported but don't rely on it. Visualization (i.e. ternary plots) for some of these designs can be done with the help of the wirePlot3 and contourPlot3 function. Visualization is however BETA and the analysis of mixture designs is not as convenvient as for objects of class facDesign (i.e. needs to be done manually, at this point). Feedback is welcome...
}

\seealso{
\code{\link{facDesign}},
\code{\link{fracDesign}},
\code{\link{rsmDesign}},
\code{\link{wirePlot3}},
\code{\link{contourPlot3}}
}
\examples{

#simplex lattice design with center (default response added with NA's)
mixDesign(p = 3, n = 2, center = FALSE)

#simplex lattice design with a center (default response added with NA's)
mixDesign(p = 3, n = 2, center = TRUE)

#simplex lattice design with augmented points (default response added with NA's)
mixDesign(p = 3, n = 2, center = FALSE, axial = TRUE)

#simplex lattice design with augmented points, center and 2 replications (default response added with NA's)
mixDesign(p = 3, n = 2, center = TRUE, axial = TRUE, replicates = 2)

#simplex centroid design giving 2^(p-1) distinct design points (default response added with NA's)
mixDesign(p = 3, n = 2, type = "centroid")

#simplex centroid design with augmented points (default response added with NA's)
mixDesign(p = 3, n = 2, type = "centroid", axial = TRUE)



#yarn elongation example from Cornell (2002)
mdo = mixDesign(3,2, center = FALSE, axial = FALSE, randomize = FALSE, replicates  = c(1,1,2,3))
names(mdo) = c("polyethylene", "polystyrene", "polypropylene")#set names (optional)
#units(mdo) = "\%" #set units (optional)
#set response (i.e. yarn elongation)
elongation = c(11.0, 12.4, 15.0, 14.8, 16.1, 17.7, 16.4, 16.6, 8.8, 10.0, 10.0, 9.7, 11.8, 16.8, 16.0)  
response(mdo) = elongation

#print a summary of the design
summary(mdo)

#show contourPlot and wirePlot
dev.new(14, 14);par(mfrow = c(2,2))
contourPlot3(A, B, C, elongation, data = mdo, form = "quadratic")
wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", theta = 190)
wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", phi = 390, theta = 0)
wirePlot3(A, B, C, elongation, data = mdo, form = "quadratic", phi = 90)
}