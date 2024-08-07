% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MultiBin.R
\name{fitMultiBin}
\alias{fitMultiBin}
\title{Fitting the Multiplicative Binomial Distribution when binomial
random variable, frequency, probability of success and theta parameter are given}
\usage{
fitMultiBin(x,obs.freq,p,theta)
}
\arguments{
\item{x}{vector of binomial random variables.}

\item{obs.freq}{vector of frequencies.}

\item{p}{single value for probability of success.}

\item{theta}{single value for theta parameter.}
}
\value{
The output of \code{fitMultiBin} gives the class format \code{fitMuB} and \code{fit} consisting a list

\code{bin.ran.var} binomial random variables.

\code{obs.freq} corresponding observed frequencies.

\code{exp.freq} corresponding expected frequencies.

\code{statistic} chi-squared test statistics.

\code{df} degree of freedom.

\code{p.value} probability value by chi-squared test statistic.

\code{fitMuB} fitted probability values of \code{dMultiBin}.

\code{NegLL} Negative Log Likelihood value.

\code{p} estimated probability value.

\code{theta} estimated theta parameter value.

\code{AIC} AIC value.

\code{call} the inputs of the function.

Methods \code{summary}, \code{print}, \code{AIC}, \code{residuals} and \code{fitted}
can be used to extract specific outputs.
}
\description{
The function will fit the Multiplicative Binomial distribution
when random variables, corresponding frequencies, probability of success and theta parameter
are given. It will provide the expected frequencies, chi-squared test statistics value,
p value and degree of freedom  value so that it can be seen if this distribution
fits the data.
}
\details{
\deqn{obs.freq \ge 0}
\deqn{x = 0,1,2,..}
\deqn{0 < p < 1}
\deqn{0 < theta }
}
\examples{
No.D.D <- 0:7       #assigning the random variables
Obs.fre.1 <- c(47,54,43,40,40,41,39,95)     #assigning the corresponding frequencies

#estimating the parameters using maximum log likelihood value and assigning it
parameters <- EstMLEMultiBin(x=No.D.D,freq=Obs.fre.1,p=0.1,theta=.3)

pMultiBin <- bbmle::coef(parameters)[1]    #assigning the estimated probability value
thetaMultiBin <- bbmle::coef(parameters)[2]  #assigning the estimated theta value

#fitting when the random variable,frequencies,probability and theta are given
results <- fitMultiBin(No.D.D,Obs.fre.1,pMultiBin,thetaMultiBin)
results

#extracting the AIC value
AIC(results)

#extract fitted values
fitted(results)

}
\references{
\insertRef{johnson2005univariate}{fitODBOD}
\insertRef{kupper1978use}{fitODBOD}
\insertRef{paul1985three}{fitODBOD}
}
\seealso{
\code{\link[bbmle]{mle2}}
}
