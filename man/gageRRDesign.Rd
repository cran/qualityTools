\name{gageRRDesign}
\Rdversion{1.1}
\alias{gageRRDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Gage R&R - Gage Repeatability and Reproducibility
}
\description{
Creates a Gage R&R design
}
\usage{
gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, method = "crossed", sigma = 6, randomize = TRUE)
}

\arguments{
  \item{Operators}{
a number or a character vector defining the Operators
}
  \item{Parts}{
a number or character vector defining the Parts
}
  \item{Measurements}{
a number defining the measurements per part
}
  \item{method}{
"crossed" or "nested"
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{randomize}{
randomize the gageRR design
}
}
\value{
object of class gageRR
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{gageRR}},
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
