\name{print.adtest}
\alias{print.adtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test Statistics
}
\description{
Generic S3 function for objects of class \code{adtest}.
}
\usage{
print.adtest(x, digits = 4, quote = TRUE, prefix = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
needs to be an object of class \code{adtest}.
}
  \item{digits}{
minimal number of significant digits, see \code{\link{print.default}}.
}
  \item{quote}{
logical, indicating whether or not strings should be printed with surrounding quotes.\cr
By default \code{quote} is set to \sQuote{TRUE}.
}
  \item{prefix}{
single character or character string that will be printet in front of x.  
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\seealso{
\code{\link{print.default}}\cr
\url{http://www.user.tu-berlin.de/kalicete/qualityTools/}
}

