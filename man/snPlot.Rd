\name{snPlot}   
\alias{snPlot}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{Signal-to-Noise-Ratio Plots
}
\description{
A Signal-to-Noise-Ratio plot is created for designs of type taguchiDesign with at least two replicates.
}
\usage{
snPlot(object, type="nominal", factors, fun = mean, response = NULL,
       single = FALSE, points = FALSE, classic = FALSE, axes = TRUE, 
       lty, xlab, ylab, main, ylim, ...)
}
\arguments{
  \item{object}{
needs to be an object of class \code{\link{taguchiDesign}}.
}
  \item{type}{
character variable setting the type of the signal-to-noise-ratio plot. The following three can be chosen:
\itemize{
\item \dQuote{nominal}: Creates a nominal-the-best signal-to-noise-ratio plot. 
      This type of signal-to-noise-ratio plot is used if the goal is to equalize the observed values to a nominal value. For example, the
      output impedance of an electric device should be as close to specification as possible to garantee a high level of quality. \cr
\item \dQuote{smaller}: Creates a smaller-the-better signal-to-noise-ratio plot.
      This type of signal-to-noise-ratio plot is used if the goal is to minimize the observed values. For example, the usage of an of an 
      automatic peeling device: the remains of skin on potatoes should be as small as possible to ensure a high level of quality.\cr
\item \dQuote{larger}: Creates a larger-the-better signal-to-noise-ratio plot.
      This type of signal-to-noise-ratio plot is used if the goal is to maximize the observed values. For example, the
      amount of fruits on trees in a cultivation should be maximised to maximise the profit.\cr
}
By default \code{fun} is set to \dQuote{nominal}
}
  \item{factors}{
for which factor is the effectPlot to be created.
}
  \item{fun}{
a function for the construction of the effectPlot such as \code{\link{mean}}, \code{\link{median}},etc.\cr
By default \code{fun} is set to \code{\link{mean}}. 
}
  \item{response}{
response variable. If the response data frame of fdo consists of more then one responses,\cr
this variable can be used to choose just one column of the response data frame. \code{response} needs to be an object of class 
character with length of \sQuote{1}. It needs to be the same character as the name of the response in the response data frame that should be plotted.\cr
By default \code{response} is set to \sQuote{NULL}.}
  \item{single}{
logical value. If \sQuote{TRUE} device region can be set up using for instance \code{par(mfrow = c(2,2))}.\cr
By default \code{single} is set to \sQuote{FALSE}.
}
  \item{points}{
logical value. If \sQuote{TRUE} points are shown in addition to values out of \code{fun}.\cr
By default \code{points} is set to \sQuote{FALSE}.
}
  \item{classic}{
logical value. \sQuote{TRUE} creates an effectPlot as depicted in most textbooks.\cr
By default \code{classic} is set to \sQuote{FALSE}.
}
  \item{axes}{
logical value indicating wheter the axes should be drawn or not.\cr
\sQuote{TRUE} by default.
}
  \item{lty}{
numerical value which specifies the line type used.
}
  \item{xlab}{
a title for the x axis: \code{\link{title}}. 
}
  \item{ylab}{
a title for the y axis: \code{\link{title}}. 
}
  \item{main}{
an overall title for the plot: see \code{\link{title}}.
}
  \item{ylim}{
vector giving the range of the y-axis.
}
  \item{\dots}{
Arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}). 
}
}
\details{
\code{snPlot} uses \code{\link{effectPlot}} and creates an effect plot for the signal-to-noise ratios as target values.\cr
Depending on the used \code{type} the target values for the single replications of the taguchi design will be calculated in the following way:
\itemize{
 \item \dQuote{nominal}: SN = 10*log(mean(y)/var(y)) \cr
 \item \dQuote{smaller}: SN = -10*log((1/n)*sum(y^2)) \cr
 \item \dQuote{larger}: SN = -10*log((1/n)*sum(1/y^2)) \cr
}
Signal-to-Noise ratio plots are an additional tool to estimate the effects of the single factors. Beside the effect plot, which is used to 
identify the factor with the most effect to a process or something like that, the signal-to-noise ratio plot can be used to judge the variance 
an d therefore the validity of the results of an effect plot.
}
\value{
snPlot invisibly returns a data.frame containing all the single Signal-to-Noise ratios.
}
\references{
\itemize{
 \item \url{http://www.statsoft.com/textbook/experimental-design}  \cr
 \item  APTE, P.R.: Taguchi Method for Dynamic Problems - 3 Day Taguchi Method Workshop at Unimap. Bombay, 2012.
}
}
\author{
 Thomas Roth \email{thomas.roth@tu-berlin.de} \cr 
 Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}
\seealso{
\code{\link{interactionPlot}}\cr
\code{\link{paretoPlot}}\cr
\code{\link{facDesign}}\cr
\code{\link{response}}\cr
\code{\link{normalPlot}}\cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
tdo = taguchiDesign("L9_3",replicates=3)
response(tdo) = rnorm(27)
snPlot(tdo, points = TRUE, col = 2, pch = 16, lty = 3)
}