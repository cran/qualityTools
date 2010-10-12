\name{gageRR}
\Rdversion{1.1}
\alias{gageRR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gage R&R - Gage Repeatability and Reproducibility
}
\description{
Performs a Gage R&R analysis for an object of class gageRR
}
\usage{                             
gageRR(gdo, method = "crossed", sigma = 5.15, alpha = 0.25, DM = NULL, HM = NULL,
 tolerance = NULL, dig = 3, ...)
}

\arguments{
  \item{gdo}{
object of class gageRR
}
  \item{method}{
"crossed" which is the typical design for performing a Measurement Systems
Analysis using Gage Repeatability and Reproducibility or "nested" which is used
 for destructive testing (i.e. the same part cannot be measured twice). Operators  
 measure each a different sample of parts under the premise that the parts of each
 batch are alike.
}
\item{sigma}{ number of sigmas; for sigma=6 this relates to 99.73 percent representing the full spread of a normal distribution function (i.e. pnorm(3) - pnorm(-3)). Another popular setting sigma=5.15 relates to 99 percent (i.e. pnorm(2.575) - pnorm(-2.575)).
}
  \item{alpha}{
alpha value for discarding the interaction Operator:Part and fitting a non-interaction model. 
}
  \item{DM}{
%%     ~~Describe \code{DM} here~~
}
  \item{HM}{
%%     ~~Describe \code{HM} here~~
}
  \item{tolerance}{
Tolerance for the measured parts. This is required to calculate the Process to Tolerance Ratio.
}
\item{dig}{
number of significant digits for \code{\link{format}}.
}
  \item{\dots}{
further graphical parameters
}
}
\value{
returns an object of class gageRR and shows typical Gage Repeatability and Reproducibility Output including Process to Tolerance Ratios and the number of distinctive categories (i.e. ndc) the measurement system is able to discriminate with the tested setting.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de
}
}

\seealso{
\code{\link{gageRRDesign}} 
\code{\link{response}} 
\code{\link{cg}} 
}
\examples{
#create a crossed Gage R&R Design
gdo = gageRRDesign(3,10, 2, randomize = FALSE)

#set the response i.e. Measurements
y = c(23,22,22,22,22,25,23,22,23,22,20,22,22,22,24,25,27,28,23,24,23,24,24,22,22,22,24,23,22,24,20,20,25,24,22,24,21,20,21,22,21,22,21,21,24,27,25,27,23,22,25,23,23,22,22,23,25,21,24,23)
response(gdo) = y


#perform a Gage R&R
gdo = gageRR(gdo, tolerance = 5)

#summary
summary(gdo)

#standard graphics for Gage R&R
plot(gdo)


##create a crossed Gage R&R Design - Vardeman, VanValkenburg 1999 - Two-Way Random-Effects Analyses and Gauge
#gdo = gageRRDesign(Operators = 5, Parts = 2, Measurements = 3, randomize = FALSE)
#
##Measurements
#weight = c(3.481, 3.448, 3.485, 3.475, 3.472,
#           3.258, 3.254, 3.256, 3.249, 3.241,
#           3.477, 3.472, 3.464, 3.472, 3.470,
#           3.254, 3.247, 3.257, 3.238, 3.250,
#           3.470, 3.470, 3.477, 3.473, 3.474,
#           3.258, 3.239, 3.245, 3.240, 3.254)
#
##set the response i.e. Measurements
#response(gdo) = weight
#
##perform a Gage R&R
#gdo = gageRR(gdo)
#
##summary
#summary(gdo)
#
##standard graphics for Gage R&R
#plot(gdo)
#

}

