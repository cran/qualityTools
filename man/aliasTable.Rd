\name{aliasTable}
\alias{aliasTable}

\title{
Display an alias table
}
\description{
Function to do display an alias table for the aliased effects of a fractional factorial design.
}
\usage{
aliasTable(fdo, degree, show = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fdo}{
 needs to be an object of class \code{\link{facDesign}}.
}

  \item{degree}{
 numeric giving the degree of interaction i.e. degree=3 means up to threeway interactions.
}

  \item{show}{
 a logigal value specifying whether the alias table should be shown or not.   \cr
 By default \code{show} is set to \sQuote{TRUE}.
}
}
\value{
 The function \code{aliasTable()} returns a matrix indicating the aliased effects. 
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\note{
 For a more detailed example which shows the effect of applying \code{aliasTable()} to 
 an object of class \code{\link{facDesign}}, please read the vignette for the package 
 \code{\link{qualityTools}} at \url{http://www.user.tu-berlin.de/kalicete/qualityTools/S5.html#SS2}.
}

\seealso{
%\code{\link{confounds}},
\code{\link{fracDesign}},       \cr
\code{\link{fracChoose}},       \cr
\url{http://www.user.tu-berlin.de/kalicete/qualityTools/S5.html#SS2}
}
\examples{
fracFac = fracDesign(k = 3, gen = "C = AB")                                 #create a fractional factorial design
aliasTable(fracFac)                                                         #display the alias table
}
