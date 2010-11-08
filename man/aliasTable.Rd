\name{aliasTable}
\alias{aliasTable}

\title{
display an alias table
}
\description{
function to do display an alias table for the aliased effects of a fractional factorial design
}
\usage{
aliasTable(fdo, degree, show = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fdo}{
object of class facDesign
}

  \item{degree}{
degree of interaction i.e. degree=3 means up to threeway interactions
}

  \item{show}{
TRUE/FALSE
}


}
\value{
matrix 
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}



\seealso{
%\code{\link{confounds}},
\code{\link{fracDesign}},
\code{\link{fracChoose}},
}
\examples{
fracFac = fracDesign(k = 3, gen = "C = AB") #create a fractional factorial design
aliasTable(fracFac)  #display the alias table
}
