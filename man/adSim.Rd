\name{adSim}
\alias{adSim}
\title{
  Bootstrap-based Anderson-Darling Test for Univariate Distributions}
\description{
  Anderson-Darling test for univariate distributions with bootstrap-based p-value determination. It also enables the p-value determination from tabled critical values. }
\usage{
adSim(x, distribution = "normal", b = 10000)
}

\arguments{
  \item{x}{
     A numeric vector.
}
  \item{distribution}{
     A character string. Distributions "cauchy", "exponential", "gumbel", "gamma", "log-normal", "lognormal", "logistic", "normal" and "weibull" are recognized.
}
  \item{b}{
     A numeric value giving the frequency of bootstraps. Any value in [1000,1000000] is allowed. If b is set to NA, the Anderson-Darling test will be applied without simulation. b should be chosen carefully. High values such as 1 mio can easily make your computer run up to 3 hours (depending on the distribution, on the sample size and on your computer system). 
}
}
\details{
 First parameter estimation for the tested distribution is performed. In the majority of cases Maximum-Likelihood-Estimation is used as fitting method and is mainly provided by fitdistr() from package MASS. Parameters of normal and log-normal distribution are fitted by mean and standard deviation. Cauchy parameters are fitted by the sums of the weighted order statistic, when p-value determination should be done from tabled critical values. The Anderson-Darling statistic is calculated based on the estimated parameters. Parametric bootstrapping provides the distribution of the Anderson-Darling test which is used in order to determine a p-value. Simulation-based Anderson-Darling distribution and critical values for selected quantiles are printable. When simulation is not desired, a p-value is obtained from tabled critical values (no exact expression exists except for log-normal, normal and exponential distribution). Tabled critical values for selected quantiles are printable as well.
}
\value{
An object of class "adSim", a list with six components,

	\item{distribution }{the distribution the Anderson-Darling test was applied for}
	\item{parameter_estimation }{the estimated parameters}
	\item{Anderson_Darling }{the value of the Anderson-Darling test}
	\item{p_value }{the corresponding p-value (simulated or tabled value)}
	\item{crititical_values }{the corresponding critical values (simulated or tabled 0.75, 0.90, 0.95, 0.975 and 0.99 quantiles)}
	\item{simAD }{bootstrap-based Anderson-Darling distribution }
}
\references{
 Stute, W./ Manteiga, W./ Quindimil, M.(1993): Bootstrap based goodness-of-fit-tests; Metrika, Vol. 40, p.243-256; Physica Verlag \cr
 D'Agostino, R.B./ Stephens, M.A. (1986): Goodness-Of-Fit Techniques; illustrated edition; p.97-193; Marcel Dekker Inc.}

\author{
Marco Wannicke \cr
Thomas Roth \email{thomas.roth@tu-berlin.de}
}

\seealso{
\url{http://www.r-qualitytools.org}
}
\examples{
x <- rnorm(25,32,2)
adSim(x)
adSim(x,"logistic",2000)
adSim(x,"cauchy",NA)}

\keyword{ Anderson-Darling Test }
\keyword{ Bootstrap }
