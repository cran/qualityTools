\name{averagePlot}
\alias{averagePlot}
\keyword{Measurement Systems Analysis} 
\keyword{Six Sigma}
\title{
 Function to create average Plots
}
\description{
\code{averagePlot} creates all x-y plots of averages by size out of an object of class \code{\link{gageRR}}.\cr
Therfore the averages of the multiple readings by each operator on each part are plotted with the reference\cr
value or overall part averages as the index. 
}
\usage{
 averagePlot(x, main, xlab, ylab, col, ask=TRUE, single=FALSE, ...)
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
  \item{ask}{
 a logical value. If \sQuote{TRUE} (default) the user is asked for input, before a new figure is drawn.
}
  \item{single}{
 a logical value.If \sQuote{TRUE} a new graphic device will be opened for each plot.\cr
 By default \code{single} is set to \sQuote{FALSE}. For further information see details.
}
  \item{\dots}{
 arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}). 
}
}
\details{
 \code{averagePlot} will split the screen into maximal 3x2 subscreens in which plots will be plotted.\cr
 If the six subscreens are not enough the function will (by default) ask the user before plotting \cr
 the next few plots. If \code{ask} is set to \sQuote{FALSE} the function will open as many graphic \cr
 devices as necessary to show all plots.\cr
 There are two possible ways to avoid the described internal routine of splitting the screen: \cr
 \itemize{
  \item If \code{single} is set to \sQuote{TRUE} all plots will be plotted in seperate single devices.\cr
  \item If \code{par(mfrow)} is unequal to \sQuote{c(1,1)} the function will adapt the setting given by \code{\link{par}}. 
 }
}

\references{
The idea of the plot and the example given by \code{example(averagePlot)} are out of:
\itemize{
 \item CHRYSLER Group LLC; FORD Motor Company; GENERAL MOTORS Corporation: Measurement System Analysis (MSA),\cr
       p.114, 4rd ed. Southfield: AIAG, 2010.\cr
}
}
\author{
     Thomas Roth: \email{thomas.roth@tu-berlin.de} \cr
     Etienne Stockhausen: \email{stocdarf@mailbox.tu-berlin.de}
}
\note{
Please do read the vignette for the package \code{\link{qualityTools}} at \url{http://www.user.tu-berlin.de/kalicete/qualityTools}.
}

\seealso{
  \code{\link{gageRR}} \cr
  \code{\link{par}} \cr
  \url{http://www.r-qualitytools.org}\cr
  \url{http://webapps.r-qualitytools.org/brew/Gage/gageRR.html}
}
\examples{
# create gageRR-object
gdo = gageRRDesign(Operators = 3, Parts = 10, Measurements = 3, 
                   randomize = FALSE) 
# vector of responses                   
y = c(0.29,0.08, 0.04,-0.56,-0.47,-1.38,1.34,1.19,0.88,0.47,0.01,0.14,-0.80,       
      -0.56,-1.46, 0.02,-0.20,-0.29,0.59,0.47,0.02,-0.31,-0.63,-0.46,2.26,
      1.80,1.77,-1.36,-1.68,-1.49,0.41,0.25,-0.11,-0.68,-1.22,-1.13,1.17,0.94,
      1.09,0.50,1.03,0.20,-0.92,-1.20,-1.07,-0.11, 0.22,-0.67,0.75,0.55,0.01,
      -0.20, 0.08,-0.56,1.99,2.12,1.45,-1.25,-1.62,-1.77,0.64,0.07,-0.15,-0.58,
      -0.68,-0.96,1.27,1.34,0.67,0.64,0.20,0.11,-0.84,-1.28,-1.45,-0.21,0.06,
      -0.49,0.66,0.83,0.21,-0.17,-0.34,-0.49,2.01,2.19,1.87,-1.31,-1.50,-2.16)
# appropriate responses      
response(gdo)=y   
# perform and gageRR                                                                 
gdo=gageRR(gdo)                                                                    
averagePlot(gdo,pch=19)                                                            
}
