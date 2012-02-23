\name{pbDesign}
\alias{pbDesign}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{
Plackett-Burman Designs
}
\description{
Creates a Plackett-Burman design.
}
\usage{
pbDesign(n, k, randomize = TRUE, replicates = 1)
}
\arguments{
  \item{n}{
integer value giving the number of trials.
}
  \item{k}{
integer value giving the number of factors.
}
  \item{randomize}{
logical value (\sQuote{TRUE}/\sQuote{FALSE}) - randomizes the RunOrder of the design.\cr
By default \code{randomize} is set to \sQuote{TRUE}.
}
  \item{replicates}{
Integer giving the number of replicates.
}
}
\value{
\code{pbDesign} returns an object of class \code{\link{pbDesign}}.
}
\references{
\itemize{
 \item Plackett, R.L. and Burman J.P.: The design of optimum multifactorial \cr
       experiments. Biometrika 33 (1946), 305-332               \cr
 \item Myers, R.H.; Montgomery, D.C. and Anderson-Cook, C.M.: Response Surface \cr
       Methodology. p. 165. New Jersey: Wiley & Sons, 2009.
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}
\note{
 This function creates Placket-Burman Designs up to n=48. Bigger Designs are not implemented 
 because of lack in practicability. For the creation either the number of factors or the number of trials can be denoted. Wrong combinations will lead to
 an error message. Originally Placket-Burman-Design are  applicable for number of trials divisible by 4. If n is not divisble by 4 this function will take
 the next larger Placket-Burman Design and truncate the last rows and columns. 
}
\seealso{
\code{\link{facDesign}} for 2^k factorial designs\cr
\code{\link{rsmDesign}} for response surface designs\cr
\code{\link{fracDesign}} for fractional factorial design\cr
\code{\link{gageRRDesign}} for gage designs           \cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
 pbdo=pbDesign(5)
}