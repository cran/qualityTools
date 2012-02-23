\name{optimum}
\Rdversion{1.1}
\alias{optimum}
\keyword{Multiple Response Optimization}
\keyword{Six Sigma}
\title{
Optimal factor settings 
}
\description{
Calculates the best factors settings with regard to defined desirabilities and constraints.\cr
Two approaches are currently supported, (I) evaluating (all) possible factor settings and (II) using the function \code{\link{optim}} or \code{gosolnp} of the \code{Rsolnp} package.\cr
Using optim \code{\link{optim}} initial values for the factors to be optimized over can be set via start.\cr
The optimality of the solution depends critically on the starting parameters which is why it is recommended to use \sQuote{type="gosolnp"} although calculation takes a while.
}
\usage{
optimum(fdo, constraints, steps = 25, type = "grid", start, ...)
}
\arguments{
  \item{fdo}{
  an object of class \code{\link{facDesign}} with \code{\link{fits}} and \code{\link{desires}} set.
}
  \item{constraints}{
constraints for the factors such as list(A = c(-2,1), B = c(0, 0.8)).
}                                                             
  \item{steps}{
number of grid points per factor if type = \dQuote{grid}.
}
  \item{type}{
type of search. \dQuote{grid},\dQuote{optim} or \dQuote{gosolnp} are supported (see DESCRIPTION).
}
  \item{start}{
numerical vector giving the initial values for the factors to be optimized over.
}

  \item{\dots}{
further aguments.
}
}
\details{
It is recommened to use \sQuote{type="gosolnp"}. Derringer and Suich (1994) desirabilities do not have continuous first derivatives, more precisely they have points where their derivatives do not exist,\cr
start should be defined in cases where \sQuote{type = "optim"} fails to calculate the best factor setting.
}
\value{
\code{optimum} returns an object of class desOpt.
}

\author{
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\note{
 For a  example which shows the usage of \code{optimum()} in context please read the vignette for the package 
 \code{\link{qualityTools}} at \url{http://www.r-qualitytools.org/Improve.html}.
}

\seealso{
\code{\link{optim}}\cr
\code{\link{desirability}} \cr
\code{\link{desires}}         \cr
\code{\link[Rsolnp]{gosolnp}}    \cr
\url{http://www.r-qualitytools.org/Improve.html}
}
\examples{
#BEWARE BIG EXAMPLE --Simultaneous Optimization of Several Response Variables--
#Source: DERRINGER, George; SUICH, Ronald: Simultaneous Optimization of Several 
#        Response Variables. Journal of Quality Technology Vol. 12, No. 4, 
#        p. 214-219

#Define the response suface design as given in the paper and sort via 
#Standard Order
fdo = rsmDesign(k = 3, alpha = 1.633, cc = 0, cs = 6)
fdo = randomize(fdo, so = TRUE)

#Attaching the 4 responses
y1 = c(102,120,117,198,103,132,132,139,102,154,96,163,116,153,133,133,140,142,
       145,142)
y2 = c(900,860,800,2294,490,1289,1270,1090,770,1690,700,1540,2184,1784,1300,
       1300,1145,1090,1260,1344)
y3 = c(470,410,570,240,640,270,410,380,590,260,520,380,520,290,380,380,430,
       430,390,390)
y4 = c(67.5,65,77.5,74.5,62.5,67,78,70,76 ,70,63 ,75,65,71 ,70,68.5,68,68,69,
       70)
response(fdo) = data.frame(y1, y2, y3, y4)[c(5,2,3,8,1,6,7,4,9:20),]

#setting names and real values of the factors
names(fdo) = c("silica", "silan", "sulfur")
highs(fdo) = c(1.7, 60, 2.8)
lows(fdo) = c(0.7, 40, 1.8)

#summary of the response surface design
summary(fdo)

#setting the desires
desires(fdo) = desirability(y1, 120, 170, scale = c(1,1), target = "max")
desires(fdo) = desirability(y2, 1000, 1300, target = "max")
desires(fdo) = desirability(y3, 400, 600, target = 500)
desires(fdo) = desirability(y4, 60, 75, target = 67.5)
desires(fdo)

#Have a look at some contourPlots
par(mfrow = c(2,2))
contourPlot(A, B, y1, data = fdo)
contourPlot(A, B, y2, data = fdo)
wirePlot(A, B, y1, data = fdo)
wirePlot(A, B, y2, data = fdo)


#setting the fits as in the paper
fits(fdo) = lm(y1 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2), 
               data = fdo)
fits(fdo) = lm(y2 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2), 
               data = fdo)
fits(fdo) = lm(y3 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2), 
               data = fdo)
fits(fdo) = lm(y4 ~ A + B + C + A:B + A:C + B:C + I(A^2) + I(B^2) + I(C^2),
               data = fdo)
#fits(fdo)

#calculate the same best factor settings as in the paper using type = "optim"
optimum(fdo, type = "optim")

#calculate (nearly) the same best factor settings as in the paper using type = "grid"
optimum(fdo, type = "grid")
}