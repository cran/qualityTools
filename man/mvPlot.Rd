\name{mvPlot}
\alias{mvPlot}
\keyword{Design of Experiments}
\keyword{Six Sigma}
\title{
Multi-Vari-Charts
}
\description{
Draws a Multi-Vari Chart for 2, 3 or 4 factors.
}
\usage{
mvPlot(response, fac1, fac2, fac3 = NA, fac4 = NA, sort = TRUE, col, pch, 
       cex.txt = 1, las = 1, labels = FALSE, quantile = TRUE, FUN = NA)
}
\arguments{
  \item{response}{
the values of the response in a vector.\code{response} must be declared.  
}
  \item{fac1}{
vector providing factor 1 as shown in the example.\code{fac1} must be declared. 
}
  \item{fac2}{
vector providing factor 2 as shown in the example.\code{fac2} must be declared. 
}
  \item{fac3}{
(optional) vector providing factor 3 as shown in the example. \cr
By default \code{fac3} is set to \sQuote{NA}.
}
  \item{fac4}{
(optional) vector providing factor 4 as shown in the example. \cr
By default \code{fac3} is set to \sQuote{NA}.
}
  \item{sort}{
logical value indicating whether the sequence of the factors given by \code{fac1} - \code{fac4} should be \cr
reordered to minimize the space needed to visualize the Multi-Vari-Chart.\cr
By default \code{sort} is set to \sQuote{TRUE}.
}
  \item{col}{
graphical parameter. Vector containing numerical values or character strings giving the colors for the different factors.\cr
By default \code{col} starts with the value \sQuote{3} and is continued as needed.
}
  \item{pch}{
graphical parameter. Vector containing numerical values or single characters giving plotting points for the different factors.\cr
See \code{\link{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter\cr
(and not NA nor NULL). By default \code{pch} starts with the value \sQuote{1} and is continued as needed. 
}
  \item{cex.txt}{
a numerical value giving the amount by which plotting labels at the single points should be magnified relative to the default.\cr
By default \code{cex.txt} is set to \sQuote{1}.
}
  \item{las}{
graphical parameter for the style of x-axis labels. See \code{\link{par}} for further information.
}
  \item{labels}{
logical value indicating whether the single points should be labeld with the row-number of the \code{data.frame}\cr
invisibly returned by the function \code{mvPlot}. By default \code{labels} is set to \sQuote{FALSE}.
}
  \item{quantile}{
logical value indicating whether the quanitiles (0.00135, 0.5 & 0.99865) should be visualized for the single groups.\cr
By default \code{quantile} is set to \sQuote{TRUE}.
}
  \item{FUN}{
function to be used for calculation of \code{response} for unique settings of the factors e.g. the \code{\link{mean}}.\cr
By default \code{FUN} is set to \sQuote{NA} and therfore omitted.
}
}
\value{
\code{mvPlot} returns invisibly a \code{data.frame}in which all plotted points are listed.\cr
The option \code{labels} can be used to plot the row-numbers at the single points and to ease the identification.
}
\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}\cr
Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}
\seealso{
\url{http://www.r-qualitytools.org}
}
\examples{
#Example I
examp1 = expand.grid(c("Engine1","Engine2","Engine3"),c(10,20,30,40))                  
examp1 = as.data.frame(rbind(examp1, examp1, examp1))
examp1 = cbind(examp1, rnorm(36, 1, 0.02))
names(examp1) = c("factor1", "factor2", "response")
test1=mvPlot(response = examp1[,3], fac1 = examp1[,2],
             fac2 = examp1[,1],sort=FALSE,las=2,FUN=mean) 

#Example II
examp2=expand.grid(c("Op I","Op II","Op III"),c(1,2,3,4),
                   c("20.11.1987","21.11.1987"))
examp2=as.data.frame(rbind(examp2, examp2, examp2))
examp2=cbind(examp2, rnorm(72, 22, 2))
names(examp2) = c("factor1", "factor2", "factor3", "response")
test2=mvPlot(response = examp2[,4], fac1 = examp2[,1],
            fac2 = examp2[,2], fac3 = examp2[,3], sort=TRUE, FUN=mean, 
            labels=TRUE)

#Example III
examp3 = expand.grid(c("A","B","C"),c("I","II","III","IV"),c("H","I"),
                     c(1,2,3,4,5))
examp3 = as.data.frame(rbind(examp3, examp3, examp3))
examp3 = cbind(examp3, rnorm(360, 0, 2))
names(examp3) = c("factor1", "factor2", "factor3", "factor4", "response")
test3=mvPlot(response = examp3[,5], fac1 = examp3[,1],
             fac2 = examp3[,2], fac3 = examp3[,3], fac4 = examp3[,4], sort=TRUE, 
             quantile=TRUE, FUN=mean)

 
                              

                

}