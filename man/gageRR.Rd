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
temp = gageRRDesign(3,10, 2, randomize = FALSE)

#set the response i.e. Measurements
y = c(23,22,22,22,22,25,23,22,23,22,20,22,22,22,24,25,27,28,23,24,23,24,24,22,22,22,24,23,22,24,20,20,25,24,22,24,21,20,21,22,21,22,21,21,24,27,25,27,23,22,25,23,23,22,22,23,25,21,24,23)
response(temp) = y

#perform a Gage R&R
gdo = gageRR(temp)

#summary
summary(gdo)

#standard graphics for Gage R&R
plot(gdo)

}

