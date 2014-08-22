\name{cg}
\alias{cg}
\alias{cgToleranceView}
\alias{cgHist}
\alias{cgRunChart}
\keyword{Gage Capability}
\keyword{Six Sigma}
\title{
 Function to calculate and visualize the gage capability.
}
\description{
 Function visualize the given values of measurement in a run chart and in 
 a histogram. Furthermore the \dQuote{centralized Gage potential index} Cg 
 and the \dQuote{non-centralized Gage Capability index} Cgk are calculated 
 and displayed.
}
\usage{
cg             (x, target, tolerance, ref.interval, facCg, facCgk, n = 0.2, 
                type, col, pch, xlim, ylim, conf.level = 0.95, cex.val = 1.5)
                
cgToleranceView(x, target, tolerance, ref.interval, facCg, facCgk, n = 0.2, 
                type, col, pch, xlim, ylim, main, conf.level = 0.95, cex.val = 1,
                cgOut = TRUE)
                
cgHist         (x, target, tolerance, ref.interval, facCg, facCgk, n = 0.2, col,
                xlim, ylim, main, conf.level = 0.95, cex.val = 1, cgOut = TRUE)
                
cgRunChart     (x, target, tolerance, ref.interval, facCg, facCgk, n = 0.2, 
                type, col, pch, xlim, ylim,main, conf.level = 0.95, cex.val = 1,
                cgOut = TRUE)

}
\arguments{
  \item{x}{
 a vector containing the measured values.
}
  \item{target}{
 a numeric value giving the expected target value for the x-values.
}
  \item{tolerance}{
 vector of length 2 giving the lower and upper specification limits. 
}
  \item{ref.interval}{
 numeric value giving the confidence intervall on which the calculation is based.
 By default it is based on 6 sigma methodology. Regarding the normal distribution 
 this relates to \code{pnorm(3) - pnorm(-3}) which is exactly 99.73002 percent
 If the calculation is based on an other sigma value \code{ref.interval} needs 
 to be adjusted. To give an example: If the sigma-level is given by 5.15 the 
 \code{ref.interval} relates to \code{pnorm(5.15/2)-pnorm(-5.15/2)} which is exactly
 0.989976 percent.
 }
  \item{facCg}{
 numeric value as a factor for the calculation of the gage potential index.
 The default Value for facCg is \sQuote{0.2}.
}
  \item{facCgk}{
 numeric value as a factor for the calulation of the gage capability index.
 The default value for facCgk is \sQuote{0.1}.
}
  \item{n}{
 numeric value between \sQuote{0} and \sQuote{1} giving the percentage of the tolerance field 
 (values between the upper and lower specification limits given by tolerance)
 where the values of x should be positioned. Limit lines will be drawn. Default
 value is \sQuote{0.2}.
}
  \item{type}{
  what type of plot should be drawn in the run chart. Possible types see
  \code{\link{plot}}.
  }
  \item{col}{
 color of the curve in the run chart.
}
  \item{pch}{
 variable specifies the symbols of the run chart. Details see \code{\link{par}}.
}
  \item{xlim}{
 vector of length 2 giving the limits for the x axis of the run chart.
}
  \item{ylim}{
 vector of length 2 giving the limits for the y axis of the run chart.
}
  \item{main}{
 an overall title for the plot: see \code{\link{title}}.
}
  \item{conf.level}{
 confidence level for internal t.test checking the significance of the bias between target and mean of x.
 The default value is \sQuote{0.95}. The result of the t.test is shown in the histogram on the left side.
}
  \item{cex.val}{
 numeric value giving the size of the text in the legend. See also \code{\link{par}}.
}
  \item{cgOut}{
  logical value deciding wether the Cg and Cgk values should be plotted in a legend. Only
  available for the function \code{cgHist}, \code{cgToleranceView} and \code{cgRunChart}.
  The default value for cgOut is \sQuote{TRUE}. 
  }                                                                      
}
\details{
 The calculation of the potential and actual gage capability are based on the 
 following formulae: \cr
 
 \itemize{
   \item Cg = (facCg * tolerance[2]-tolerance[1])/ref.interval \cr
    
   \item Cgk = (facCgk * abs(target-mean(x))/(ref.interval/2)  
}
 If the usage of the historical process variation is preferred the values for 
 the tolerance \code{tolerance} must be adjusted manually. That means in case of the 
 6 sigma methodolgy for example, that tolerance = 6 * sigma[process].
 
}
\value{
 Function returns a list of numeric values. The first element contains the calculated 
 centralized gage potential index Cg and the second contains the non-centralized 
 gage capability index Cgk.
}
\references{
 \itemize{
   \item DIETRICH, Edgar; SCHULZE, Alfred: Pruefprozesseignung,
    3rd ed. Munich: Carl Hanser, 2007.\cr
    
  \item DIETRICH, Edgar et al: Eignungsnachweis von
   Messsystemen, 3rd ed. Munich: Carl Hanser, 2008.
 }   
}
\author{
     Thomas Roth: \email{thomas.roth@tu-berlin.de} \cr
     Etienne Stockhausen: \email{stocdarf@mailbox.tu-berlin.de}
}

\note{
 Support for other distributions than normal might be included later in an update.\cr
 For a more detailed example which shows the usage \code{cg()} please read the vignette for the package 
 \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Measure.html}.
}

\seealso{
  \code{\link{gageLin}} \cr
  \code{\link{gageRR}} \cr
  \code{\link{plot}} \cr
  \code{\link{par}} \cr
  \url{http://www.r-qualitytools.org/Measure.html}\cr
  \url{http://webapps.r-qualitytools.org/brew/CG/cg.html}
}
\examples{            
#simple example with default values                                
cg(rnorm(125,mean = 10.01 ,sd = 0.1), target = 10, tolerance = c(8,12)) 
#example with larger n and adjusted ref. interval                   
cg(rnorm(25,mean = 1.01 ,sd = 0.5), ref.interval=pnorm(5.5/2)-pnorm(-5.5/2), n=0.3)     
#example with changed factors for Cg and Cgk   
cg(rnorm(75, sd = 0.1), facCg = 0.15, facCgk = 0.075, tolerance = c(-10,10)/6)             
}