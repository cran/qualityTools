\name{as.data.frame.pbDesign}
\alias{as.data.frame.pbDesign}
\keyword{Design of Experiments}
\title{
Coerce to a data.frame
}
\description{
S3 generic for pbDesign class.
}

\usage{
\method{as.data.frame}{pbDesign}(x, row.names = NULL, optional = FALSE, ...)
}

\arguments{
  \item{x}{
 needs to be an object of class \code{\link{pbDesign}}.
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
The function \code{as.data.frame.pbDesign} returns a data frame.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
Etienne Stockhausen \email{stocdar@mailbox.tu-berlin.de}
}
\seealso{
\url{http://www.r-qualitytools.org}
}


