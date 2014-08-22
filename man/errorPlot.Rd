\name{errorPlot}
\alias{errorPlot}
\keyword{Measurement Systems Analysis}
\keyword{Six Sigma}
\title{
 Function to create error Charts
}
\description{
The data from an object of class \code{\link{gageRR}} can be analyzed by running \dQuote{Error Charts}\cr
of the individual deviations from the accepted rference values. These \dQuote{Error Charts} are provided by \cr
the function \code{errorPlot}.
}
\usage{
errorPlot(x, main, xlab, ylab, col, pch, type, ylim, legend=TRUE, ...)
}
\arguments{
  \item{x}{
 needs to be an object of class \code{\link{gageRR}}.
  }
  \item{main}{
  a main title for the plot.
  }
  \item{xlab}{
  a label for the x axis.
  }
  \item{ylab}{
  a label for the y axis.
  }
  \item{col}{
  plotting color.
  }
  \item{pch}{
  an integer specifying a symbol or a single character to be used as the default in plotting points.
  }
  \item{type}{
  graphical parameter (see \code{\link{plot}}).
  }
  \item{ylim}{
  the y limits of the plot
  }
  
  \item{legend}{
 a logical value specifying whether a legend is plotted automatically. By default \code{legend} is set\cr
 to \sQuote{TRUE}. If the argument \code{legend} is set to \sQuote{FALSE} an individual legend can be added\cr
 by using the function \code{\link{legend}} afterwards.
}
  \item{\dots}{
 arguments to be passed to methods, such as graphical parameters (see \code{\link{par}}). 
}
}
\details{
The plotted values are can be calculated in two ways:  \cr
\itemize{
 \item Error = Observed Value - Reference Value     (not yet implemented) \cr
 \item Error = Observed Value - Average Measurement of Part               \cr
}
The first way is not yet implemented, because it is not yet possible to give a refrence value to the object in \code{x}.\cr 
This will be implemented later! Therefore \code{errorPlot} uses 
the second way above to calculate the plotted error.\cr \cr
Graphical parameters such as \code{col} or \code{pch} can be given as single characters or as\cr
vectors containing characters or number for the parameters of the individual operators.
}

\references{
The idea of the plot and the example given by \code{example(errorPlot)} are out of:
\itemize{
 \item CHRYSLER Group LLC; FORD Motor Company; GENERAL MOTORS Corporation: Measurement System Analysis (MSA),\cr
       p.112, 4rd ed. Southfield: AIAG, 2010.\cr
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
  \url{http://www.r-qualitytools.org}\cr
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
# perform and gageRR    
gdo=gageRR(gdo)                                                                    
errorPlot(gdo)                                                            
}
