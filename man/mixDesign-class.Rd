\name{mixDesign-class}
\keyword{Design of Experiments}
\Rdversion{1.1}
\docType{class}
\alias{mixDesign-class}
\title{Class "mixDesign"}
\description{
mixDesign class for simplex lattice and simplex centroid mixture designs with optional center points and augmented points.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mixDesign", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{factors}:}{Object of class \code{"list"} ~~ }
    \item{\code{total}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{lower}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{design}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{designType}:}{Object of class \code{"character"} ~~ }
    \item{\code{pseudo}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{response}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{Type}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{block}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{runOrder}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{standardOrder}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{desireVal}:}{Object of class \code{"list"} ~~ }
    \item{\code{desirability}:}{Object of class \code{"list"} ~~ }
    \item{\code{fits}:}{Object of class \code{"data.frame"} ~~ }
  }
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\seealso{
\code{\linkS4class{facDesign}}
}
\examples{
showClass("mixDesign")
}
\keyword{classes}
