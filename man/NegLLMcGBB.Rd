% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gbeta1.R
\name{NegLLMcGBB}
\alias{NegLLMcGBB}
\title{Negative Log Likelihood value of McDonald Generalized Beta  Binomial Distribution}
\usage{
NegLLMcGBB(x,freq,a,b,c)
}
\arguments{
\item{x}{vector of binomial random variables.}

\item{freq}{vector of frequencies.}

\item{a}{single value for shape parameter alpha representing as a.}

\item{b}{single value for shape parameter beta representing as b.}

\item{c}{single value for shape parameter gamma representing as c.}
}
\value{
The output of \code{NegLLMcGBB} will produce a single numeric value.
}
\description{
This function will calculate the negative log likelihood value when the vector of binomial random
variables and vector of corresponding frequencies are given with the shape parameters a,b and c.
}
\details{
\deqn{0 < a,b,c }
\deqn{freq \ge 0}
\deqn{x = 0,1,2,...}
}
\examples{
No.D.D <- 0:7            #assigning the random variables
Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies

NegLLMcGBB(No.D.D,Obs.fre.1,.2,.3,1)    #acquiring the negative log likelihood value

}
\references{
Manoj, C., Wijekoon, P. & Yapa, R.D., 2013. The McDonald Generalized Beta-Binomial Distribution: A New
Binomial Mixture Distribution and Simulation Based Comparison with Its Nested Distributions in Handling
Overdispersion. International Journal of Statistics and Probability, 2(2), pp.24-41.

Available at: \doi{10.5539/ijsp.v2n2p24}.

Janiffer, N.M., Islam, A. & Luke, O., 2014. Estimating Equations for Estimation of Mcdonald Generalized
Beta - Binomial Parameters. , (October), pp.702-709.

Roozegar, R., Tahmasebi, S. & Jafari, A.A., 2015. The McDonald Gompertz Distribution: Properties and Applications.
Communications in Statistics - Simulation and Computation, (May), pp.0-0.

Available at: \doi{10.1080/03610918.2015.1088024}.
}
