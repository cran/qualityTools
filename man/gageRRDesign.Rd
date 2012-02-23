\name{gageRRDesign}
\Rdversion{1.1}
\alias{gageRRDesign}
\keyword{Measurement Systems Analysis}
\title{
Gage R&R - Gage Repeatability and Reproducibility
}
\description{
Creates a Gage R&R design.
}
\usage{
gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, method = "crossed",
             sigma = 6, randomize = TRUE)
}

\arguments{
  \item{Operators}{
numeric value giving a number or a character vector defining the Operators.\cr
By default \code{Operators} is set to \sQuote{3}.
}
  \item{Parts}{
a number or character vector defining the Parts.\cr
By default \code{parts} is set to \sQuote{10}.
}
  \item{Measurements}{
a number defining the measurements per part.
By default \code{Measurments} is set to \sQuote{3}.
}
  \item{method}{
\dQuote{crossed} which is the typical design for performing a Measurement Systems
 Analysis using Gage Repeatability and Reproducibility or \dQuote{nested} which is used
 for destructive testing (i.e. the same part cannot be measured twice). Operators  
 measure each a different sample of parts under the premise that the parts of each
 batch are alike. \cr
 By default \code{method} is set to \dQuote{crossed}.
}
  \item{sigma}{
 For sigma=6 this relates to 99.73 percent representing the full spread of a normal distribution function (i.e. pnorm(3) - pnorm(-3)).\cr
 Another popular setting sigma=5.15 relates to 99 percent (i.e. \code{pnorm(2.575) - pnorm(-2.575)}).
 By default \code{sigma} is set to \sQuote{6}.
}
  \item{randomize}{
logicalvalue. \sQuote{TRUE} (default) randomizes the gageRR design. 
}
}
\value{
\code{gagRRDeign} returns an object of class \code{\link{gageRR}}.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
 For an example in context which shows the usage of the function \code{gageRRDesign()} 
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Measure.html}.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{gageRR}}\cr
 \url{http://www.r-qualitytools.org/Measure.html}\cr
 \url{http://webapps.r-qualitytools.org/brew/Gage/gageRR.html}
}
\examples{
#create a Gage R&R Design
temp = gageRRDesign(3,10, 2, randomize = FALSE)

#set the response i.e. Measurements
y = c(23,22,22,22,22,25,23,22,23,22,20,22,22,22,24,25,27,28,23,24,23,24,24,22,
      22,22,24,23,22,24,20,20,25,24,22,24,21,20,21,22,21,22,21,21,24,27,25,27,
      23,22,25,23,23,22,22,23,25,21,24,23)
response(temp) = y

#perform a Gage R&R
gdo = gageRR(temp)

#summary
summary(gdo)

#standard graphics for Gage R&R
plot(gdo)
}
