\name{facDesign-class}
\keyword{Design of Experiments}
\Rdversion{1.1}
\docType{class}
\alias{facDesign-class}

\title{Class "facDesign"}
\description{
facDesign Class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("facDesign", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{factors}:}{Object of class \code{"list"} ~~ }
    \item{\code{cube}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{star}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{centerCube}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{centerStar}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{generator}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{response}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{block}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{blockGen}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{runOrder}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{standardOrder}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{desireVal}:}{Object of class \code{"list"} ~~ }
    \item{\code{desirability}:}{Object of class \code{"list"} ~~ }
    \item{\code{fits}:}{Object of class \code{"list"} ~~ }
  }
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\examples{
showClass("facDesign")
}
\keyword{classes}
