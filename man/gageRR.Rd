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
gageRR(gdo, method = "crossed", sigma = 5.15, alpha = 0.25, DM = NULL, HM = NULL, toler = NULL, plotit = F, dig = 4, ...)
}

\arguments{
  \item{gdo}{
object of class gageRR
}
  \item{method}{
"crossed" or "nested"
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{alpha}{
alpha value for fitting a non interaction model
}
  \item{DM}{
%%     ~~Describe \code{DM} here~~
}
  \item{HM}{
%%     ~~Describe \code{HM} here~~
}
  \item{toler}{
%%     ~~Describe \code{toler} here~~
}
  \item{plotit}{
%%     ~~Describe \code{plotit} here~~
}
  \item{dig}{
%%     ~~Describe \code{dig} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\value{
object of class gageRR
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de
}
}

\seealso{
\code{\link{gageRRDesign}} 
}
\examples{
#create a Gage R&R Design
temp = gageRRDesign(Operators = 3,3,3)

#set the response i.e. Measurements
response(temp)  = rnorm(27)

#perform a Gage R&R
gdo = gageRR(temp)

#summary
summary(gdo)

#standard graphics for Gage R&R
plot(gdo)

}

