\name{names-methods}
\docType{methods}
\alias{names-methods}
\alias{names,ANY-method}
\alias{names,doeFactor-method}
\alias{names<-,doeFactor-method}
\alias{names,facDesign-method}
\alias{names<-,facDesign-method}
\alias{names,taguchiFactor-method}
\alias{names<-,taguchiFactor-method}
\alias{names,taguchiDesign-method}
\alias{names<-,taguchiDesign-method}
\alias{names,gageRR-method}
\alias{names,mixDesign-method}
\alias{names<-,mixDesign-method}
\title{Methods for Function names in Package `base'}
\description{
Methods for function \code{names} in Package `base'}


\usage{
\S4method{names}{facDesign}(x)
\S4method{names}{facDesign}(x) <- value
\S4method{names}{mixDesign}(x)
\S4method{names}{mixDesign}(x) <- value
\S4method{names}{doeFactor}(x)
\S4method{names}{doeFactor}(x) <- value
\S4method{names}{taguchiDesign}(x)
\S4method{names}{taguchiDesign}(x) <- value
\S4method{names}{taguchiFactor}(x)
\S4method{names}{taguchiFactor}(x) <- value

\S4method{names}{gageRR}(x)
}

\arguments{
\item{x}{object of class facDesign, mixDesign, taguchiDesign, taguchiFactor, doeFactor or gageRR}
\item{value}{character vector}
}



\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
ANY object
}

\item{\code{signature(x = "doeFactor")}}{
a doeFactor object
}

\item{\code{signature(x = "facDesign")}}{
a facDesign object
}

\item{\code{signature(x = "mixDesign")}}{
a mixDesign object
}

\item{\code{signature(x = "taguchiDesign")}}{
a taguchiDesign object
}

\item{\code{signature(x = "taguchiFactor")}}{
a taguchiFactor object
}


\item{\code{signature(x = "gageRR")}}{
a gageRR object
}
}}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}