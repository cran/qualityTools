\name{as.data.frame.steepAscent}
\alias{as.data.frame.steepAscent}
\title{
Coerce to a data.frame
}
\description{
S3 generic for steepAscent class.
}

\usage{
\method{as.data.frame}{steepAscent}(x, row.names = NULL, optional = FALSE, ...)
}

\arguments{
  \item{x}{
 needs to be an object of class \code{\link{steepAscent}}.
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
The function \code{as.data.frame.steepAscentDesign} returns a data frame.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\seealso{
\url{http://www.r-qualitytools.org}
}


