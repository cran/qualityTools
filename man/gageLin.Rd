\name{gageLin}
\alias{gageLin}
\keyword{Measurement Systems Analysis}
\keyword{Six Sigma}
\title{
Function to visualize and calucalte the linearity of a gage.
}
\description{
Function visualize the linearity of a gage by plotting the single and mean bias
in one plot and intercalate them with a straight line. \cr
Furthermore the function deliver some characteristic values of linearity studies
according to MSA (Measurement System Analysis).
}
\usage{
gageLin(object, conf.level = 0.95, ylim, col, pch, lty = c(1, 2), 
        stats = TRUE, plot = TRUE)
}
\arguments{
  \item{object}{
   an object of class \code{MSALinearity}. To create such an object see \code{\link{gageLinDesign}}.
}
  \item{conf.level}{
   an numeric value between \sQuote{0} and \sQuote{1}, giving the confidence intervall
   for the analysis. \cr
   Default value: \sQuote{0.95}
}
  \item{ylim}{
   a vector with two entries, giving the minimum and the maximum of the y-axis.
}
  \item{col}{
   a vector with four numeric entries. The first gives the color of the single points,
   the second gives the color of the points for the mean bias, the third gives the color
   fo the straight interpolation line and the fourth gives the color for the lines 
   representing the confidence interval. If one of the values is missing or negative 
   the points or lines are not plotted. \code{col} is by default \sQuote{c(1,2,1,4)} 
}
  \item{pch}{
   a vector with two numeric or single character entries giving the symbols for the
   single points (1st entry) and  the mean bias (2nd entry). The default vector is 
   \dQuote{c(20,18)}
}
  \item{lty}{
   a vector with two entries giving the line-style for the interpolating line and the 
   confidence interval lines. For detailed information to the entries please see
   \code{\link{par}}. The default value for \code{lty} is \sQuote{c(1,2)}.
}
  \item{stats}{
   a logigal value. If \sQuote{TRUE} (default) the function returns all calculated 
   information.
}
  \item{plot}{
   a logigal value. If \sQuote{TRUE} (default) the function deliver a plot.
}
}
\value{
The function returns an object of class \code{MSALinearity} which can be used
with e.g. \code{\link{plot}} or \code{\link{summary}}.
}
\references{
\itemize{ \item Cgrysler Group LCC & Ford Motor Company & General Motors Corporation, 
                Measurement System Analysis - MSA (2010), 4th ed. 
                Southfield,Michigan: Automotive Industry Action Group. \cr
 }
}
\author{
     Thomas Roth: thomas.roth@tu-berlin.de \cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}

\seealso{
  \code{\link{cg}} \cr
  \code{\link{gageRR}} \cr
  \code{\link{gageLinDesign}}  \cr
  \code{\link{response}} \cr
  \code{\link{edit}}\cr
  \url{http://www.r-qualitytools.org} \cr
  \url{http://webapps.r-qualitytools.org/brew/gageLin/gageLin.html}
}
\examples{
# Results of single runs
A=c(2.7,2.5,2.4,2.5,2.7,2.3,2.5,2.5,2.4,2.4,2.6,2.4)  
B=c(5.1,3.9,4.2,5,3.8,3.9,3.9,3.9,3.9,4,4.1,3.8)
C=c(5.8,5.7,5.9,5.9,6,6.1,6,6.1,6.4,6.3,6,6.1)
D=c(7.6,7.7,7.8,7.7,7.8,7.8,7.8,7.7,7.8,7.5,7.6,7.7)
E=c(9.1,9.3,9.5,9.3,9.4,9.5,9.5,9.5,9.6,9.2,9.3,9.4)

# creates Desing
test=gageLinDesign(ref=c(2,4,6,8,10),n=12)
 # creates data.frame with results             
Messungen=data.frame(rbind(A,B,C,D,E))
# enter results in Desing               
response(test)=Messungen                              

# no plot and no return
MSALin=gageLin(test,stats=FALSE,plot=FALSE)
# plot only           
plot(MSALin)
# summary                                          
summary(MSALin)                                       
}
