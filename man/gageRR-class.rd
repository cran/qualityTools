\name{gageRR-class}
\Rdversion{1.1}
\keyword{Measurement Systems Analysis}
\docType{class}
\alias{gageRR-class}

\title{Class "gageRR"}
\description{
gageRR Class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gageRR", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{X}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{ANOVA}:}{Object of class \code{"aov"} ~~ }
    \item{\code{RedANOVA}:}{Object of class \code{"aov"} ~~ }
    \item{\code{method}:}{Object of class \code{"character"} ~~ }
    \item{\code{Estimates}:}{Object of class \code{"list"} ~~ }
    \item{\code{Varcomp}:}{Object of class \code{"list"} ~~ }
    \item{\code{Sigma}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{GageName}:}{Object of class \code{"character"} ~~ }
    \item{\code{GageTolerance}:}{Object of class \code{"character"} ~~ }
    \item{\code{DateOfStudy}:}{Object of class \code{"character"} ~~ }
    \item{\code{PersonResponsible}:}{Object of class \code{"character"} ~~ }
    \item{\code{Comments}:}{Object of class \code{"character"} ~~ }
    \item{\code{b}:}{Object of class \code{"factor"} ~~ }
    \item{\code{a}:}{Object of class \code{"factor"} ~~ }
    \item{\code{numM}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{numO}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{numP}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{y}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{facNames}:}{Object of class \code{"character"} ~~ }

  }
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\examples{
showClass("gageRR")
}
\keyword{classes}
