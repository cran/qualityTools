\name{identity-methods}
\docType{methods}
\alias{identity}
\alias{identity-methods}
\alias{identity,facDesign-method}
\alias{identity,taguchiDesign-method}
\title{Get method}
\description{Calculates the alias table for a fractional factorial design and prints an easy to read summary of the defining relations such as 'I = ABCD' for a standard 2^(4-1) factorial design.}

\usage{
\S4method{identity}{facDesign}(x)
}
\arguments{
\item{x}{a \code{\link{facDesign}} or \code{\link{taguchiDesign}} object}
}

\value{
\item{identity in character representation}{columns}
}

\section{Methods}{
\describe{
\item{\code{signature(x = "facDesign")}}{
Calculates the alias table for a fractional factorial design and prints an easy to read summary of the defining relations such as \sQuote{I = ABCD} for a standard 2^(4-1) factorial design.
}
\item{\code{signature(x = "taguchiDesign")}}{
Calculates the alias table for a fractional taguchi design and prints an easy to read summary of the defining relations such as \sQuote{I = ABC} for a standard \dQuote{L4_2} taguchi design.
}
}}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\code{\link{aliasTable}}\cr
\code{\link{fracDesign}}\cr
\code{\link{fracChoose}}\cr
\url{http://www.r-qualitytools.org}
}
\examples{
#generate a 2^(4-1) factorial design by assigning the interaction ABC 
#to the factor D
vp = fracDesign(k = 4, gen = "D = ABC")

#the defining relation is (D = ABC)*D = I = ABCD. I is the identity.
identity(vp)

#the identity can be seen in the according alias table
aliasTable(vp)
}
