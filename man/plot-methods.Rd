\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY-method}
\alias{plot,desirability-method}
\alias{plot,distr-method}
\alias{plot,distrCollection-method}
\alias{plot,gageRR-method}
\alias{plot,steepAscent-method}
\alias{plot,MSALinearity-method}
\title{Methods for Function plot in Package \code{graphics}}
\description{Methods for function \code{\link{plot}} in Package \code{graphics}}

%\usage{
%\S4method{plot}{desirability}(x, y = NULL, scale, main, xlab, ylab, type, col,
%\                             numPoints = 500, ...)
%\S4method{plot}{distr}(x,y, main, xlab, xlim, ylim, ylab, line.col, line.width,
%\                      box, ...)
%\S4method{plot}{distrCollection}(x, y, xlab, ylab, xlim, ylim, line.col, 
%\                                line.width, ...)
%\S4method{plot}{gageRR}(x,y, main, xlab, ylab, col, lwd, fun, ...)
%}


\section{Methods}{
\describe{

\item{\code{signature(x = "MSALinearity")}}{
object of class \code{MSALinearity}.
}

\item{\code{signature(x = "ANY")}}{
ANY 
}

\item{\code{signature(x = "desirability")}}{
object of class \code{\link{desirability}}.
}

\item{\code{signature(x = "distr")}}{
object of class \code{distr}.
}

\item{\code{signature(x = "distrCollection")}}{
object of class \code{distrCollection}.
}

\item{\code{signature(x = "steepAscent")}}{
object of class \code{distrCollection}.
}

\item{\code{signature(x = "gageRR")}}{
object of class \code{\link{gageRR}}.
}
}}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}
\seealso{
\url{http://www.r-qualitytools.org}
}
\keyword{methods}


