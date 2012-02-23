\name{qualityTools-package}
\Rdversion{1.2}
\alias{qualityTools-package}
\alias{qualityTools}
\docType{package}
\title{
Statistical Methods for Quality Sciences
}
\description{
This Package contains methods associated with the (\strong{D})\bold{efine} (\strong{M})\bold{easure} (\strong{A})\bold{nalyze} (\strong{I})\bold{mprove} and (\strong{C})\bold{
ontrol} (i.e. \bold{DMAIC}) cycle of the Six Sigma Quality Management methodology.

\enumerate{
\item \bold{Define:} Pareto Chart

\item \bold{Measure:} Probability and Quantile-Quantile Plots, Process Capability Ratios for various distributions and Gage R&R

\item \bold{Analyze:} Pareto Chart, Multi-Vari Chart, Dot Plot

\item \bold{Improve:} Full and fractional factorial, response surface and mixture designs as well as the desirability approach for simultaneous optimization of more than one response variable. Normal, Pareto and Lenth Plot of effects as well as Interaction Plots etc.

\item \bold{Control:} Quality Control Charts can be found in the qcc package
}
}

\details{
\tabular{ll}{
Package: \tab qualityTools\cr
Type: \tab Package\cr
Version: \tab 0.96.2\cr
Date: \tab 2012-07-01\cr
URL: \tab \url{http://r-qualitytools.org}\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}}

\note{This package is primarily used for teaching! The package vignette is available under \url{http://www.r-qualitytools.org}.}

\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
Maintainer: Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\keyword{package}
\examples{
example(paretoChart)
example(mvPlot)
example(dotPlot)
example(qqPlot)
example(ppPlot)
example(pcr)
example(gageRR)
example(facDesign)
%%example(normalPlot)
example(fracDesign)
example(effectPlot)
example(taguchiDesign)
example(rsmDesign)
example(paretoPlot)
example(wirePlot)
example(contourPlot)
example(mixDesign)
example(desirability)
example(optimum)
}
