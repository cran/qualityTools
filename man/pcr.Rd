\name{pcr}
\Rdversion{1.1}
\alias{pcr}
\keyword{Process Capability Analysis}
\keyword{Distribution Identification}
\keyword{Six Sigma}
\title{
Process Capability Indices
}
\description{
Calculates the process capability cp, cpk, cpkL (onesided) and cpkU (onesided) for a given dataset and distribution. \cr
A histogramm with a density curve is displayed along with the specification limits and a Quantile-Quantile Plot for the specified distribution.\cr
Lower-, upper and total fraction of nonconforming entities are calculated. Box-Cox Transformations are supported as well as the calculation of Anderson Darling Test Statistics. 
}
\usage{
pcr(x, distribution = "normal", lsl, usl, target, boxcox = FALSE, 
    lambda = c(-5,5), main, xlim, ylim, grouping = NULL, std.dev = NULL,  
    conf.level = 0.9973002, start, lineWidth = 1, lineCol = "red", 
    lineType = "solid", specCol = "red3", specWidth = 1, cex.text = 2, 
    cex.val = 1.5, cex.col = "darkgray", plot = TRUE, bounds.lty = 3, 
    bounds.col = "red", ...)
}
\arguments{
  \item{x}{
numeric vector containing the values for which the process capability should be calculated.
}
  \item{distribution}{
character string specifying the distribution of x. The function \code{cp} will accept the following caracter strings for \code{distribution}:
\itemize{
  \item \dQuote{normal}
  \item \dQuote{log-normal}
  \item \dQuote{exponential}
  \item \dQuote{logistic}
  \item \dQuote{gamma}
  \item \dQuote{weibull}
  \item \dQuote{cauchy}
  \item \dQuote{gamma3}
  \item \dQuote{weibull3}
  \item \dQuote{lognormal3}
  \item \dQuote{beta}
  \item \dQuote{f}
  \item \dQuote{t}
  \item \dQuote{geometric}
  \item \dQuote{poisson}
  \item \dQuote{negative-binomial}
}
By default \code{distribution} is set to \dQuote{normal}.
}
  \item{lsl}{
numeric value for the lower specification limit.
}
  \item{usl}{
numeric value for the upper specification limit.
}
  \item{target}{
(optional) numeric value giving the target value. 
}
  \item{boxcox}{
logical value specifying whether a Box-Cox transformation should be performed or not. \cr
By default \code{boxcox} is set to \sQuote{FALSE}.
}
  \item{lambda}{
(optional) lambda for the transformation, default is to have the function estimate lambda. 
}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{xlim}{
vector giving the range of the x-axis.
}
  \item{ylim}{
vector giving the range of the y-axis.
}
  \item{grouping}{
(optional) If grouping is given the standard deviation is calculated as mean standard deviation of the specified subgroups corrected by the factor c4 and expected fraction of nonconforming is calculated using this standard deviation.
}
  \item{std.dev}{
(optional) historical standard devation (only provided for normal distribution!).
}
  \item{conf.level}{
numeric value between \sQuote{0} and \sQuote{1} giving the confidence interval.\cr 
By default \code{conf.level} is 0.9973 (99.73\%) which is the reference interval bounded by the 99.865\% and 0.135\% quantile. 
}
  \item{start}{
a named list giving the parameters to be fitted with initial values. Must be supplied for some distribution (see \code{\link{fitdistr}} of the R-package \code{MASS}).
}
  \item{lineWidth}{
a numeric value specifying the width of the line for the density curve.
}
  \item{lineCol}{
numerical value or character string (like \dQuote{red}) specifying the color of the line for the density curve.
}
  \item{lineType}{
character string specifying the line type e.g. \dQuote{dashed}, \dQuote{solid}, etc.
}
  \item{specCol}{
numerical value or character string specifying the color for the specification limits.
}
  \item{specWidth}{
numerical value specifying the line width for the specification limits.
}
  \item{cex.text}{
numerical value specifying the cex for lsl, usl and target.
}
  \item{cex.val}{
numerical value specifying the cex for the process capability ratios.
}
  \item{cex.col}{
numerical value or character string specifying the color for lsl, usl and target. 
}
  \item{plot}{
logical value. If set to \sQuote{FALSE} the graphical output will be omitted.\
By default \code{plot} is set to \sQuote{TRUE}.
}
  \item{bounds.col}{
 graphical parameter. For further details see \code{\link{ppPlot}} or \code{\link{qqPlot}}.
}
  \item{bounds.lty}{
 graphical parameter. For further details see \code{\link{ppPlot}} or \code{\link{qqPlot}}.
}
  \item{\dots}{
some other graphical parameters.
}
}
\value{
The function \code{pcr} returns a list with \code{lambda}, \code{cp}, \code{cpl}, \code{cpu}, \code{ppt}, \code{ppl}, \code{ppu}, \code{A}, \code{usl}, \code{lsl}, \code{target}.
}
\details{
Distribution fitting is deligated to function \code{\link{fitdistr}} of the R-package \code{MASS} as well as the calculation of lambda for the Box Cox Transformation. p-values for Anderson Darling Test are reported for the most important distributions.\cr
cpk is always min(cpK, cpL).
\itemize{
\item pt stands for total fraction nonconforming\cr
\item pu stands for upper fraction nonconforming\cr
\item pl stands for lower fraction nonconforming\cr
\item cp stands for process capability index\cr
\item cpkL stands for lower process capability index\cr
\item cpkU stands for upper process capability index\cr
\item cpk stands for minimum process capability index\cr
}
For a Box-Cox tranformation a data vector with positive values is needed to estimate an optimal value of lambda for the Box-Cox power transformation of teh values. The Box-Cox power transformation is used to bring the distribution of the data vector to a form close to normal. Estimation of the optimal lambda is deligated to the function \code{\link[MASS]{boxcox}} of the MASS package. 
The Box-Cox transformation has the form y(lambda)=(y^lambda-1)/lambda for lambda not equal to zero, and y(lambda)=log(y) for lambda equal to zero. The function boxcox computes the profile log-likelihoods for a range of values of parameter lambda. Function boxcox.lamba returns the value of lambda with the maximum profile log-likelihood.

In case no specification limits are given, \sQuote{lsl} and \sQuote{usl} are calculated to support a process capability index of 1.
} 

\note{
At this point there's no distinction made between process performance P_pk and process capability.\cr
The latter implies a process that is in statistical contro whereas process performance is estimated for a process that might not have been demonstrated to be in a state of statistical control.\cr
Currently work is being done to support the usage of three parameter weibull, loglogistic and gamma distribution.\cr
For a detailed example which shows the usage of the function \code{cp()} please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Analyze.html}.
}

\references{
\itemize{
 \item ISO(2006). Statistical methods - Process performance and capability statistics for measured quality characteristics (ISO 21747).
 \item ISO/TR(2007).Statistical methods in process management - capability and performance - part 4: process capability estimates and performance (ISO/TR 22514-4). 
 \item MITTAG, H.-J.; RINNE, H.: Prozessfaehigkeitsmessung fuer die industrielle Praxis. Muinch: Hanser, 1999.
 \item KOTZ, Samuel; LOVELACE, Cynthia R.: Process capability indices in theory and practice. London,New York: Arnold, 1998.
 \item Process Capability Statistics for Non-Normal Distributions in R \url{http://www.r-qualitytools.org/useR2011/ProcessCapabilityInR.pdf}
}
}

\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\code{\link{qqPlot}}\cr
\code{\link{ppPlot}}\cr
\url{http://www.r-qualitytools.org/Analyze.html}\cr
\url{http://webapps.r-qualitytools.org/brew/PCR/pcr.html}
}
\examples{
x = rweibull(30, 2, 8) +100
#process capability for a weibull distribution
pcr(x, "weibull", lsl = 100, usl = 117)

#box cox transformation and one sided
pcr(x, boxcox = TRUE, lsl = 1)

#boxcox with lambda=2
pcr(x, boxcox = 2, lsl = 1)

#process capability assuming a normal distribution
pcr(x, "normal", lsl = 0, usl = 17)

#process capability for a normal distribution and data in subgroups
#some artificial data with shifted means in subgroups
x = c(rnorm(5, mean = 1), rnorm(5, mean = 2), rnorm(5, mean = 0))

#grouping vector
group = c(rep(1,5), rep(2,5), rep(3,5))
                                                                    
#calculate process capability
pcr(x, grouping = group) #compare to sd(x)

}
