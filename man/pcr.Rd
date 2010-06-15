\name{pcr}
\Rdversion{1.1}
\alias{pcr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the process capability ratios for a given dataset and distribution
}
\description{
Calculates the process capability ratios cp, cpk, cpkL and cpkU for a given dataset and distribution. A histogramm with a density curve is displayed along with the specification limits and a Quantile-Quantile Plot for the specified distribution
}
\usage{
pcr(x, distribution = "normal", lsl, usl, target, boxcox = FALSE, lambda, main, xlim, ylim, grouping = NULL, std.dev = NULL, conf.level = 0.9973002, start, lineWidth = 2, lineCol = "red", lineType = "solid", specCol = "black", specWidth = 2, cex.text = 2, cex.val = 1.5, cex.col = "darkgray", ...)
}
\arguments{
  \item{x}{
numeric vector
}
  \item{distribution}{
character string such as "weibull"
}
  \item{lsl}{
lower specification limit
}
  \item{usl}{
upper specification limit
}
  \item{target}{
target value 
}
  \item{boxcox}{
perform a Box-Cox transformation (default is FALSE)
}
  \item{lambda}{
lambda for the transformation 
}
  \item{main}{
main
}
  \item{xlim}{
xlim
}
  \item{ylim}{
ylim
}
  \item{grouping}{
grouping vector
}
  \item{std.dev}{
historical standard devation (only for normal distribution)
}
  \item{conf.level}{

}
  \item{start}{
A named list giving the parameters to be fitted with initial values. Must be supplied for some distribution. (see Details)
}
  \item{lineWidth}{
the width of the line for the density curve
}
  \item{lineCol}{
the color of the line for the density curve
}
  \item{lineType}{
the line Type e.g. "dashed", "solid", etc.
}
  \item{specCol}{
col for the specification limits
}
  \item{specWidth}{
lwd for the specification limits
}
  \item{cex.text}{
cex for lsl, usl and target
}
  \item{cex.val}{
cex for the process capability ratios
}
  \item{cex.col}{
col for lsl, usl and target 
}
  \item{\dots}{
other graphical parameters (par)
}
}
\details{
Distribution fitting is deligated to function fitdistr of the R-package MASS.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\code{\link{qqPlot}},
\code{\link{ppPlot}}
}
\examples{
#process capability for a weibull distribution
x = rweibull(30, 2, 8)
pcr(x, "weibull", lsl = 0, usl = 17)

#process capability assuming a normal distribution
pcr(x, "normal", lsl = 0, usl = 17)
}
