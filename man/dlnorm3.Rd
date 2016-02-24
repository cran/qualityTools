\name{lnorm3}
\alias{lnorm3}
\alias{dlnorm3}
\alias{plnorm3}
\alias{qlnorm3}
\keyword{Distribution Identification}

\title{
 The Lognormal Distribution (3 Parameter)
}
\description{
 Density function, distribution function and quantile function 
 for the Lognormal distribution.
}
\usage{
dlnorm3(x, meanlog, sdlog, threshold)
plnorm3(q, meanlog, sdlog, threshold)
qlnorm3(p, meanlog, sdlog, threshold, ...)
}
\arguments{
  \item{x, q}{
 vector of quantiles
}
  \item{p}{
 vector of probabilities
}
  \item{meanlog, sdlog}{
 mean and standard deviation of the distribution on the
 log scale with default values of \sQuote{0} and \sQuote{1} respectively.
}
  \item{threshold}{
 threshold parameter by default 0
}
  \item{\dots}{
 Arguments that can be passed into \code{\link{uniroot}}.
}
}
\details{
 The Lognorm distribution with \sQuote{meanlog} parameter zeta, \sQuote{meansd} parameter sigma and
 \sQuote{threshold} parameter theta has density given by

    f(x) = (1/(sqrt(2*pi)*sigma*(x-theta)))                 \cr
           *exp(-(((log((x-theta))-zeta)^2)/(2*(sigma)^2)))
          
 The cumulative distribution function is given by
    
    F(x) = pnorm((log((x-theta))-zeta)/sigma)
}
\value{
 \code{\link{dlnorm3}} gives the density, \code{\link{plnorm3}} gives the
 distribution function and \code{\link{qlnorm3}} gives the quantile function.
}
\references{
   Johnson, L., Kotz, S., Balakrishnan, N. (1995) Continuous Univariate Distributions-Volume 1,
   2nd ed. New York: John Wiley & Sons. 
}
\author{
 Thomas Roth \email{thomas.roth@tu-berlin.de}\cr 
 Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}
\note{
 \code{\link{qlnorm3}}  calls \code{\link{uniroot}} for each value of the argument \sQuote{p}. 
 The solution is consequently not exact; the \dots can be used to obtain a more accurate
 solution if necessary.
}

\seealso{
 \code{\link{uniroot}}
}
\examples{
#Simple Example
dlnorm3(x=2,meanlog=0,sdlog=1/8,threshold=1)
temp=plnorm3(q=2,meanlog=0,sdlog=1/8,threshold=1)
temp
qlnorm3(p=temp,meanlog=0,sdlog=1/8,threshold=1)
#
##Visualized Example
##prepare screen
#dev.new()
#split.screen(matrix(c(0,0.5,0,1, 0.5,1,0,1),byrow=TRUE,ncol=4))
##generate values 
#x=seq(0,4,length=1000)
##plot different density functions
#screen(1)                                                                              
#plot(x,y=dlnorm3(x,0,1/8,1),col="green",xlim=c(0,3),type="l",lwd=2,xlab="x",
#     ylab="f(x)",main="Density Function of Log-Normal-Distribution")
#lines(x,y=dlnorm3(x,0,0.5,0),lwd=2,col="red")
#lines(x,y=dlnorm3(x,0,1,0),lwd=2,col="blue")
#lines(x,y=dlnorm3(x,1,1/8,0),lwd=2,col="orange")
##add legend
#legend("topleft",legend=c(expression(paste(zeta," = 0 ")*
#       paste(sigma, " = 1/8 ")*paste(theta," = 1")),
#       expression(paste(zeta," = 0 ")*paste(sigma, " = 0.5 ")*
#       paste(theta," = 0")),expression(paste(zeta," = 0 ")*
#       paste(sigma, " = 1 ")*paste(theta," = 0")),
#       expression(paste(zeta," = 1 ")*paste(sigma, " = 1/8 ")*
#       paste(theta," = 0"))),col=c("green","red","blue","orange"),
#       text.col="black",lwd=2,bty="o",inset=0.04)
#abline(v=0,lty=2,col="grey")
#abline(h=0,lty=2,col="grey")
##plot different distribution functions
#screen(2)
#plot(x,y=plnorm3(x,0,1/8,1),col="green",xlim=c(0,3),type="l",lwd=2,xlab="x",
#     ylab="F(x)",
#     main="Cumulative Distribution Function of Log-Normal-Distribution")
#lines(x,y=plnorm3(x,0,0.5,0),lwd=2,col="red")
#lines(x,y=plnorm3(x,0,1,0),lwd=2,col="blue")
#lines(x,y=plnorm3(x,1,1/8,0),lwd=2,col="orange")
##add legend
#legend("topleft",legend=c(expression(paste(zeta," = 0 ")*
#       paste(sigma, " = 1/8 ")*paste(theta," = 1")),
#       expression(paste(zeta," = 0 ")*paste(sigma, " = 0.5 ")*
#       paste(theta," = 0")),expression(paste(zeta," = 0 ")*
#       paste(sigma, " = 1 ")*paste(theta," = 0")),
#       expression(paste(zeta," = 1 ")*paste(sigma, " = 1/8 ")*
#       paste(theta," = 0"))),col=c("green","red","blue","orange"),
#       text.col="black",lwd=2,bty="o",inset=0.04)
#abline(v=0,lty=2,col="grey")
#abline(h=0,lty=2,col="grey")
#close.screen(all=TRUE)
}

