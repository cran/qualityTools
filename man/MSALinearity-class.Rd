\name{MSALinearity-class}
\keyword{Measurement Systems Analysis}
\Rdversion{1.1}
\docType{class}
\alias{MSALinearity-class}

\title{Class \code{"MSALinearity"}}
\description{MSALinearity Class
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MSALinearity", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{X}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{Y}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{model}:}{Object of class \code{"lm"} ~~ }
    \item{\code{conf.level}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{Linearity}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{GageName}:}{Object of class \code{"character"} ~~ }
    \item{\code{GageTolerance}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{DateOfStudy}:}{Object of class \code{"character"} ~~ }
    \item{\code{PersonResponsible}:}{Object of class \code{"character"} ~~ }
    \item{\code{Comments}:}{Object of class \code{"character"} ~~ }
    \item{\code{facNames}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{as.data.frame}{\code{signature(x = "MSALinearity")}: ... }
    \item{plot}{\code{signature(x = "MSALinearity")}: ... }
    \item{response}{\code{signature(object = "MSALinearity")}: ... }
    \item{response<-}{\code{signature(object = "MSALinearity")}: ... }
    \item{show}{\code{signature(object = "MSALinearity")}: ... }
    \item{summary}{\code{signature(object = "MSALinearity")}: ... }
	 }
}

\author{
     Thomas Roth: thomas.roth@tu-berlin.de \cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}
\examples{
showClass("MSALinearity")
}
\keyword{classes}
