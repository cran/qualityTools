\name{taguchiDesign-class}
\keyword{Design of Experiments}
\Rdversion{1.1}
\docType{class}
\alias{taguchiDesign-class}

\title{Class "taguchiDesign"}
\description{
taguchiDesign Class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("taguchiDesign", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{factors}:}{Object of class \code{"list"} ~~ }
    \item{\code{design}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{designType}:}{Object of class \code{"character"} ~~ }
    \item{\code{replic}:}{Object of class \code{"data.frame"} ~~ }
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
\examples{
showClass("taguchiDesign")
}
\keyword{classes}
