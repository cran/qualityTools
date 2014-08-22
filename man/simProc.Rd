\name{simProc}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\alias{simProc}

\title{
Simulated Process
}
\description{
This is a function to simulate a black box process for teaching the use of designed experiments. The optimal factor settings can be found using a sequential assembly strategy i.e. apply a 2^k factorial design first, calculate the path of the steepest ascent, again apply a 2^k factorial design and augment a star portion to find the optimal factor settings. Of course other strategies are possible.
}
\usage{
simProc(x1, x2, x3, noise = TRUE)
}

\arguments{
  \item{x1}{
numeric vector containing the values for factor 1.
}
  \item{x2}{
numeric vector containing the values for factor 2.
}
  \item{x3}{
numeric vector containing the values for factor 3.
}
  \item{noise}{
logical value deciding whether noise should be added or not. Default setting is \sQuote{FALSE}.
}
}

\details{
factor 1 is best within [40, 250]; factor 2 within [90, 240]
}

\value{
\code{simProc} returns a numeric value within the range [0,1].
}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\note{
 For an example in context which shows the usage of the function \code{simProc()}
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}
}

\seealso{
\code{\link{facDesign}} for 2^k factorial designs\cr
\code{\link{rsmDesign}} for response surface designs\cr
\code{\link{fracDesign}} for fractional factorial design\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
simProc(120, 140, 1)
simProc(120, 220, 1)
simProc(160, 140, 1)
}
\keyword{design}

