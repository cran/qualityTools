\name{cg}
\alias{cg}
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
cg(x, target, tolerance, ref.interval, facCg, facCgk, n = 0.2, type, col, pch, xlim, ylim, conf.level = 0.95, cex.val = 1.5)
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
 this relates to pnorm(3) - pnorm(-3) which is exactly 99.73002 percent
 If the calculation is based on an other sigma value \code{ref.interval} needs 
 to be adjusted. To give an example: If the sigma-level is given by 5.15 the 
 \code{ref.interval} relates to pnorm(5.15/2)-pnorm(-5.15/2) which is exactly
 0.989976 percent.
 }
  \item{facCg}{
 numeric value as a factor for the calculation of the gage potential index.
 The default Value for facCg is 0.2.
}
  \item{facCgk}{
 numeric value as a factor for the calulation of the gage capability index.
 The default value for facCgk is 0.1.
}
  \item{n}{
 numeric value between 0 and 1 giving the percentage of the tolerance field 
 (values between the upper and lower specification limits given by tolerance)
 where the values of x should be positioned. Limit lines will be drawn. Default
 value is 0.2.
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
  \item{conf.level}{
 confidence level for internal t.test checking the significance of the bias between target and mean of x.
 The default value is 0.95. The result of the t.test is shown in the histogram on the left side.
}
  \item{cex.val}{
 numeric value giving the size of the text in the legend. See also \code{\link{par}}.
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
   \item Dietrich, Edgar and Schulze, Alfred (2007) Pruefprozesseignung,
    3rd ed. Munich: Carl Hanser.\cr
    
  \item Dietrich, Edgar et al (2008) Eignungsnachweis von
   Messsystemen, 3rd ed. Munich: Carl Hanser.
 }   
}
\author{
     Thomas Roth: thomas.roth@tu-berlin.de \cr
     Etienne Stockhausen: stocdarf@mailbox.tu-berlin.de
}

\note{
 Support for other distributions than normal might be included later in an update.
}

\seealso{
  \code{\link{plot}} \cr
  \code{\link{par}}
}
\examples{                                            
cg(rnorm(125,mean = 10.01 ,sd = 0.1), target = 10, tolerance = c(8,12))                    #simple example with default values
cg(rnorm(25,mean = 1.01 ,sd = 0.5), ref.interval=pnorm(5.5/2)-pnorm(-5.5/2), n=0.3)#example with larger n and adjusted ref. interval 
cg(rnorm(75, sd = 0.1), facCg = 0.15, facCgk = 0.075, tolerance = c(-10,10)/6)             #example with changed factors for Cg and Cgk
}