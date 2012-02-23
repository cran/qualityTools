\name{ppPlot}
\Rdversion{1.1}
\alias{ppPlot}
\keyword{Distribution Identification}
\keyword{Six Sigma}
\title{
Probability Plots for various distributions
}
\description{
\code{ppPlot} creates a Probability plot of the values in x including a line. 
}
\usage{
ppPlot(x, distribution,  confbounds = TRUE, alpha, probs, main, xlab, ylab, 
       xlim, ylim, border = "red", bounds.col = "black", bounds.lty = 1, 
       grid = TRUE, box = TRUE, stats = TRUE, start, ...)
}
\arguments{
  \item{x}{
vector containing the sample for \code{ppPlot}.
}
  \item{distribution}{
character string specifying the distribution of x. The function \code{ppPlot} will support the following character strings for \code{distribution}:
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
  \item{probs}{
vector containing the percentages for the y axis. All the values need to be between \sQuote{0} and \sQuote{1}\cr
If \code{probs} is missing it will be calculated internally.
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
  \item{grid}{
logical value, deciding whether a grid will be added to the current plot.\cr
By default \code{grid} is set to \sQuote{TRUE}.
}
  \item{box}{
logical value, deciding whether a box is drawn to around the current plot.\cr
By default \code{box} is set to \sQuote{TRUE}.
}
  \item{stats}{
logical value deciding whether \code{ppPlot} returns the values listed under value.\cr
By default \code{stats} is set to \sQuote{TRUE}.
}
  \item{start}{
A named list giving the parameters to be fitted with initial values. Must be supplied for some distribution. (see Details)
}
  \item{\dots}{
further graphical parameters (see \code{\link{par}}).
}
}
\details{
Distribution fitting is deligated to function \code{\link{fitdistr}} of the R-package MASS.\cr
For computation of the confidence bounds the variance of the quantiles is estimated using the delta method,\cr
which implies estimation of observed Fisher Information matrix as well as the gradient of the CDF of the fitted distribution.\cr
Where possible, those values are replaced by their normal approximation. 
}
\value{
\code{ppPlot} returns a list containing the following: \cr
 x - x coordinates\cr
 y - y coordinates   \cr
 int - Intercept        \cr
 slope - slope            
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\note{
 For an example in context which shows the usage of the function \code{ppPlot()}  
 please read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Analyze.html}.
}

\seealso{
\code{\link{qqPlot}}
\code{\link{fitdistr}} in R-package MASS\cr
\url{http://www.r-qualitytools.org/Analyze.html}
}
\examples{
#set up the plotting window for 6 plots
par(mfrow = c(3,2))

#generate random data from weibull distribution
x = rweibull(20, 8, 2)

#Probability Plot for different distributions
ppPlot(x, "log-normal")
ppPlot(x, "normal")
ppPlot(x, "exponential", DB = TRUE)
ppPlot(x, "cauchy")
ppPlot(x, "weibull")
ppPlot(x, "logistic")        

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
