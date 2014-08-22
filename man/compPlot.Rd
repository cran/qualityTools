\name{compPlot}
\alias{compPlot}
\keyword{Measurement Systems Analysis}
\keyword{Six Sigma}
\title{
 Function to create comparison Plots
}
\description{
\code{compPlot} creates comparison x-y plots of an object of class \code{\link{gageRR}}.\cr
The averages of the multiple readings by each operator on each part are plotted against each\cr
other with the operators as indices. This plot compares the values obtained by one operator\cr
to those of another. 
}
\usage{
compPlot(x, main, xlab, ylab, col, cex.lab, fun = NULL, ...)
}
\arguments{
  \item{x}{
 needs to be an object of class \code{\link{gageRR}}.
}
   \item{main}{
  a main title for the plot
  }
  \item{xlab}{
  a label for the x axis
  }
  \item{ylab}{
  a label for the y axis
  }
  \item{col}{
  plotting color
  }
  \item{cex.lab}{
  the magnification to be used for x and y labels relative to the current setting of cex
  }
  \item{fun}{
 (optional) function that will be applied to the multiple readings of each part.\cr
 \code{fun} should be an object of class \code{function} like \code{\link{mean}},\code{\link{median}}, \code{\link{sum}}, etc. \cr
 By default, \code{fun} is set to \sQuote{NULL} and all readings will be plotted.
  }
  \item{\ldots}{
 arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}). 
}
}
\references{
The idea of the plot and the example given by \code{example(compPlot)} are out of:
\itemize{
 \item CHRYSLER Group LLC; FORD Motor Company; GENERAL MOTORS Corporation: Measurement System Analysis (MSA),\cr
       p.115, 4rd ed. Southfield: AIAG, 2010.\cr
}
}
\author{
     Thomas Roth: \email{thomas.roth@tu-berlin.de} \cr
     Etienne Stockhausen: \email{stocdarf@mailbox.tu-berlin.de}
}
\note{
Please do read the vignette for the package \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org}.
}

\seealso{
  \code{\link{gageRR}} \cr
  \code{\link{par}} \cr
  \url{http://www.r-qualitytools.org} \cr
  \url{http://webapps.r-qualitytools.org/brew/Gage/gageRR.html}
}
\examples{
#create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, 
      randomize = FALSE)
#vector of responses       
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,       
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
#appropriate responses      
response(gdo)=y  
#perform and gageRR                                                                  
gdo=gageRR(gdo)                                                                    
compPlot(gdo,pch=19)                                                            
}
