\name{print.invisible}
\alias{print.invisible}
\title{
Print Function
}
\description{
Generic S3 function for objects of class \code{\link{invisible}}.
}

\usage{
\method{print}{invisible}(x, \dots)
}
\arguments{
  \item{x}{
needs to be an object of class \code{\link{invisible}}.}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}

\note{
\code{print.invisible} is used to hide the internals of the method \code{\link{simProc}}.
}
\seealso{
\code{\link{simProc}}\cr
\code{\link{print.default}}\cr
\url{http://www.r-qualitytools.org}
}
