\name{fracChoose}
\keyword{Design of Experiments}
\Rdversion{1.1}
\alias{fracChoose}
\title{
Choosing a fractional or full factorial design from a table.
}
\description{
Designs displayed are the classic minimum abberation designs. Choosing a design is done by clicking with the mouse into the appropriate field.
}
\usage{
fracChoose()
}

\value{
\code{fracChoose()} returns an object of class \code{\link{facDesign}}.
}
\references{
\itemize{
 \item BOX, George. E. P. and HUNTER, J.S.: "Multifactor Experimental Designs for Exploring Response Surfaces",\cr
       The Annals of Mathematical Statistics 28, p.195- 241, 1957 \cr
  \item BOX, George E. P. and HUNTER, J. Stuart and HUNTER, William G.\cr
       Statistics for Experimenters (2005) 28, p.272, table 6.22 \cr
}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
 For an example in context which shows the usage of the function \code{fracChoose()}  
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}

\seealso{
\code{\link{facDesign}} \cr
\code{\link{fracDesign}}\cr
\code{\link{rsmChoose}} \cr
\code{\link{rsmDesign}} \cr
\url{http://www.r-qualitytools.org/Improve.html} \cr 
\url{http://webapps.r-qualitytools.org/brew/facDesign/fracDesign.html} \cr

}
