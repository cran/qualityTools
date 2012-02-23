\name{gamma3}
\alias{gamma3}
\alias{dgamma3}
\alias{pgamma3}
\alias{qgamma3}
\keyword{Distribution Identification}
\title{
 The gamma Distribution (3 Parameter)
}
\description{
 Density function, distribution function and quantile function
 for the gamma distribution.
}
\usage{
dgamma3(x, shape, scale, threshold)
pgamma3(q, shape, scale, threshold)
qgamma3(p, shape, scale, threshold, ...)
}

\arguments{
   \item{x, q}{
 vector of quantiles
}
  \item{p}{
 vector of probabilities
}
  \item{shape}{
 shape parameter by default 1
}
  \item{scale}{
 scale parameter by default 1
}
  \item{threshold}{
 threshold parameter by default 0
}
  \item{\dots}{
 Arguments that can be passed into \code{\link{uniroot}}.
}
}
\details{
 The gamma distribution with \sQuote{scale} parameter alpha, \sQuote{shape} parameter c and
 \sQuote{threshold} parameter zeta has density given by

    f(x) = (c/alpha) (((x-zeta)/alpha)^(c-1)) exp(-((x-zeta)/alpha)^c)

 The cumulative distribution function is given by

    F(x) = 1 - exp(-((x-zeta)/alpha)^c)
}
\value{
 \code{\link{dgamma3}} gives the density, \code{\link{pgamma3}} gives the
 distribution function and \code{\link{qgamma3}} gives the quantile function.
}
\references{
   Johnson, L., Kotz, S., Balakrishnan, N. (1995) Continuous Univariate Distributions-Volume 1,
   2nd ed. New York: John Wiley & Sons.
}
\author{
 Thomas Roth \email{thomas.roth@tu-berlin.de} \cr
 Etienne Stockhausen \email{stocdarf@mailbox.tu-berlin.de}
}
\note{
 \code{\link{qgamma3}}  calls \code{\link{uniroot}} for each value of the argument \sQuote{p}.
 The solution is consequently not exact; the \dots can be used to obtain a more accurate
 solution if necessary.
}

\seealso{
 \code{\link{uniroot}}
}
\examples{
##Simple Example
dgamma3(x=1,scale=1,shape=5,threshold=0)
temp=pgamma3(q=1,scale=1,shape=5,threshold=0)
temp
qgamma3(p=temp,scale=1,shape=5,threshold=0)
#
##Visualized Example
##prepare screen
#dev.new()
#split.screen(matrix(c(0,0.5,0,1, 0.5,1,0,1),byrow=TRUE,ncol=4))
##generate values
#x=seq(0,3,length=1000)
##plot different density functions
#screen(1)
#plot(x,y=dgamma3(x,threshold=0,shape=0.5,scale=1),col="green",
#     xlim=c(0,2.5),ylim=c(0,2.5),type="l",lwd=2,xlab="x",
#     ylab="f(x)",main="Density Function of gamma-Distribution")
#lines(x,y=dgamma3(x,threshold=0,shape=1,scale=1),lwd=2,col="red")
#lines(x,y=dgamma3(x,threshold=0,shape=1.5,scale=2),lwd=2,col="blue")
#lines(x,y=dgamma3(x,threshold=0,shape=5,scale=1),lwd=2,col="orange")
##add legend
#legend("topright",legend=c(expression(paste(alpha, " = 1 ")*
#       paste(c, " = 0.5 ")*paste(zeta," = 0")),
#       expression(paste(alpha, " = 1 ")*paste(c, " = 1 ")*
#       paste(zeta," = 0")),expression(paste(alpha, " = 2 ")*
#       paste(c, " = 1.5 ")*paste(zeta," = 0")),
#       expression(paste(alpha, " = 1 ")*paste(c, " = 5 ")*
#       paste(zeta," = 0"))),col=c("green","red","blue","orange"),
#       text.col="black",lwd=2,bty="0",inset=0.04)
#abline(v=0,lty=2,col="grey")
#abline(h=0,lty=2,col="grey")
##plot different distribution functions
#screen(2)
#plot(x,y=pgamma3(x,threshold=0,shape=0.5,scale=1),col="green",
#     xlim=c(0,2.5),ylim=c(0,1),type="l",lwd=2,xlab="x",ylab="F(x)",
#     main="Cumulative Distribution Function of gamma-Distribution")
#lines(x,y=pgamma3(x,threshold=0,shape=1,scale=1),lwd=2,col="red")
#lines(x,y=pgamma3(x,threshold=0,shape=1.5,scale=2),lwd=2,col="blue")
#lines(x,y=pgamma3(x,threshold=0,shape=5,scale=1),lwd=2,col="orange")
##add legend
#legend("bottomright",legend=c(expression(paste(alpha, " = 1 ")*
#       paste(c, " = 0.5 ")*paste(zeta," = 0")),
#       expression(paste(alpha, " = 1 ")*paste(c, " = 1 ")*
#       paste(zeta," = 0")),expression(paste(alpha, " = 2 ")*
#       paste(c, " = 1.5 ")*paste(zeta," = 0")),
#       expression(paste(alpha, " = 1 ")*paste(c, " = 5 ")*
#       paste(zeta," = 0"))),col=c("green","red","blue","orange"),
#       text.col="black",lwd=2,bty="0",inset=0.04)
#abline(v=0,lty=2,col="grey")
#abline(h=0,lty=2,col="grey")
#close.screen(all=TRUE)
}
