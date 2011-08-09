\name{cp}
\Rdversion{1.1}
\alias{cp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process Capability Indices
}
\description{
Calculates the process capability cp, cpk, cpkL (onesided) and cpkU (onesided) for a given dataset and distribution. A histogramm with a density curve is displayed along with the specification limits and a Quantile-Quantile Plot for the specified distribution. Lower-, upper and total fraction of nonconforming entities are calculated. Box Cox Transformations are supported as well as the calculation of Anderson Darling Test Statistics. 
}
\usage{
cp(x, distribution = "normal", lsl, usl, target, boxcox = FALSE, lambda, main, xlim, ylim, grouping = NULL, std.dev = NULL,  conf.level = 0.9973002, start, lineWidth = 1, lineCol = "red", lineType = "solid", specCol = "red3", specWidth = 1, cex.text = 2, cex.val = 1.5, cex.col = "darkgray", ...)
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
(optional) target value 
}
  \item{boxcox}{
perform a Box-Cox transformation (default is FALSE)
}
  \item{lambda}{
(optional) lambda for the transformation, default is to have the function estimate lambda 
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
(optional) If grouping is given the standard deviation is calculated as mean standard deviation of the specified subgroups corrected by the factor c4 and expected fraction of nonconforming is calculated using this standard deviation.
}
  \item{std.dev}{
(optional) historical standard devation (only for normal distribution)
}
  \item{conf.level}{ defaults to 99.73\% which is the reference interval bounded by the 99.865\% and 0.135\% quantile. 
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
other graphical parameters
}
}
\details{
Distribution fitting is deligated to function fitdistr of the R-package MASS as well as the calculation of lambda for the Box Cox Transformation. cpk is always min(cpK, cpL).
\itemize{
\item pt stands for total fraction nonconforming\cr
\item pu stands for upper fraction nonconforming\cr
\item pl stands for lower fraction nonconforming\cr
\item cp stands for process capability index\cr
\item cpkL stands for lower process capability index\cr
\item cpkU stands for upper process capability index\cr
\item cpk stands for minimum process capability index\cr
} 
} 

\note{
At this point there's no distinction made between process performance P_pk and process capability. The latter implies a process that is in statistical contro whereas process performance is estimated for a process that might not have been demonstrated to be in a state of statistical control.
}

\references{
ISO (2007). Statistical methods - Process performance and capability statistics for measured quality characteristics (ISO 21747:2006).

Mittag, H.-J. and H. Rinne (1999). Prozessfaehigkeitsmessung fuer die industrielle Praxis. Muenchen: Hanser.

Kotz, Samuel; Lovelace, Cynthia R.: Process capability indices in theory and practice.
London;, New York : Arnold, 1998
}

\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\code{\link{qqPlot}},
\code{\link{ppPlot}}
}
\examples{
x = rweibull(30, 2, 8) +100
#process capability for a weibull distribution
cp(x, "weibull", lsl = 100, usl = 117)

#box cox transformation and one sided
cp(x, boxcox = TRUE, lsl = 1)

#process capability assuming a normal distribution
cp(x, "normal", lsl = 0, usl = 17)

#process capability for a normal distribution and data in subgroups
#some artificial data with shifted means in subgroups
x = c(rnorm(5, mean = 1), rnorm(5, mean = 2), rnorm(5, mean = 0))

#grouping vector
group = c(rep(1,5), rep(2,5), rep(3,5))
                                                                    
#calculate process capability
cp(x, grouping = group) #compare to sd(x)

}
