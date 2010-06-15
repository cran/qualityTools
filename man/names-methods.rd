\name{names-methods}
\docType{methods}
\alias{names-methods}
\alias{names,ANY-method}
\alias{names,doeFactor-method}
\alias{names<-,doeFactor-method}
\alias{names,facDesign-method}
\alias{names<-,facDesign-method}
\alias{names,gageRR-method}
\title{Methods for Function names in Package `base'}
\description{
Methods for function \code{names} in Package `base'}


\usage{
\S4method{names}{facDesign}(x)
\S4method{names}{facDesign}(x) <- value
\S4method{names}{doeFactor}(x)
\S4method{names}{doeFactor}(x) <- value
\S4method{names}{gageRR}(x)
}

\arguments{
\item{x}{object of class facDesign, doeFactor or gageRR}
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

\item{\code{signature(x = "gageRR")}}{
a gageRR object
}
}}

\author{Thomas Roth \email{thomas.roth@tu-berlin.de}}