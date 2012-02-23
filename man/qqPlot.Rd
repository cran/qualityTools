\name{qqPlot}
\Rdversion{1.1}
\alias{qqPlot}
\keyword{Distribution Identification}
\keyword{Six Sigma}
\title{
Quantile-Quantile Plots for various distributions
}
\description{
\code{qqPlot} creates a QQ plot of the values in x including a line which passes through the first and third quartiles.
}      
\usage{
qqPlot(x, y, confbounds = TRUE, alpha, main, xlab, ylab, xlim, ylim, 
       border = "red", bounds.col = "black", bounds.lty = 1, start, ...)
}
\arguments{
  \item{x}{
the sample for qqPlot
}
  \item{y}{
character string specifying the distribution of x. The function \code{qqPlot} will support the following character strings for \code{y}:
  \itemize{
   \item \dQuote{beta}
   \item \dQuote{cauchy}
   \item \dQuote{chi-squared}
   \item \dQuote{exponential}
   \item \dQuote{f}
   \item \dQuote{gamma}
   \item \dQuote{geometric}
   \item \dQuote{log-normal}
   \item \dQuote{lognormal}
   \item \dQuote{logistic}
   \item \dQuote{negative binomial}
   \item \dQuote{normal}
   \item \dQuote{Poisson}
   \item \dQuote{t}
   \item \dQuote{weibull}
}
By default \code{distribution} is set to \dQuote{normal}.
}
  \item{confbounds}{
boolean value: \sQuote{TRUE} if confidence bounds should be drawn (default value).
}
  \item{alpha}{
significance level for the confidence bounds, set on \sQuote{0.05} by default.
}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{xlab}{
a title for the x axis: \code{\link{title}}.
}
  \item{ylab}{
a title for the y axis: \code{\link{title}}.
}
  \item{xlim}{
vector giving the range of the x-axis.
}
  \item{ylim}{
vector giving the range of the y-axis.
}
\item{border}{
numerical value or single character string giving the color of interpolation line.\cr
By default \code{border} is set to \dQuote{red}.
}
\item{bounds.col}{
numerical value or single character string giving the color of confidence bounds lines.\cr
By default bounds is set to \dQuote{black}.
}
\item{bounds.lty}{
numerical value  giving the color of confidence bounds lines.\cr
By default bounds is set to \sQuote{1}.
}
\item{start}{
A named list giving the parameters to be fitted with initial values. Must be supplied for some distribution: (see Details).
}
  \item{\dots}{
further graphical parameters: (see \code{\link{par}}).
}
}
\details{
Distribution fitting is deligated to function \code{\link{fitdistr}} of the R-package MASS.\cr
For computation of the confidence bounds the variance of the quantiles is estimated using the delta method,\cr
which implies estimation of observed Fisher Information matrix as well as the gradient of the CDF of the fitted distribution.\cr
Where possible, those values are replaced by their normal approximation. 
}
\value{
a list containing the x and y quantiles
\item{x}{sample quantiles}
\item{y}{theoretical quantiles}
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}
\note{
 For an example in context which shows the usage of the function \code{qqPlot()}  
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Analyze.html}.
}
\seealso{
\code{\link{ppPlot}}
\code{\link{fitdistr}} in R-package MASS\cr
\url{http://www.r-qualitytools.org/Analyze.html}
}
\examples{
#set up the plotting window for 6 plots
par(mfrow = c(3,2))

#generate random data from weibull distribution
x = rweibull(20, 8, 2)

#Quantile-Quantile Plot for different distributions
qqPlot(x, "log-normal")
qqPlot(x, "normal")
qqPlot(x, "exponential", DB = TRUE)
qqPlot(x, "cauchy")
qqPlot(x, "weibull")
qqPlot(x, "logistic")        
}
