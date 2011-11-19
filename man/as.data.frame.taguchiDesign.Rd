\name{as.data.frame.taguchiDesign}
\alias{as.data.frame.taguchiDesign}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coerce to a data.frame
}
\description{
S3 generic for taguchiDesign class.
}

\usage{
\method{as.data.frame}{taguchiDesign}(x, row.names = NULL, optional = FALSE, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 needs to be an object of class \code{\link{taguchiDesign}}.
}
  \item{row.names}{
 vector containing the row names. 
}
  \item{optional}{
 logical value. If \sQuote{TRUE}, setting row names and converting column names (to syntactic names: see \code{\link{make.names}}) is optional.\cr
 By default \code{optional} is set to \sQuote{TRUE}.
}
  \item{\dots}{
 additional arguments to be passed to or from methods.
}
}
\value{
The function \code{as.data.frame.taguchiDesign} returns a data frame.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\seealso{
\url{http://www.r-qualitytools.org}
}


