\name{distr-class}
\Rdversion{1.1}
\docType{class}
\alias{distr-class}

\title{Class "distr"}
\description{
distr Class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("distr", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"vector"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{parameters}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sd}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{n}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{loglik}:}{Object of class \code{"numeric"} ~~ }
  }
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\examples{
showClass("distr")
}
\keyword{classes}
