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
