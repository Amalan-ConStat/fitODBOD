% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LMultiBin.R
\name{NegLLLMBin}
\alias{NegLLLMBin}
\title{Negative Log Likelihood value of Lovinson Multiplicative Binomial distribution}
\usage{
NegLLLMBin(x,freq,p,phi)
}
\arguments{
\item{x}{vector of binomial random variables.}

\item{freq}{vector of frequencies.}

\item{p}{single value for probability of success.}

\item{phi}{single value for phi parameter.}
}
\value{
The output of \code{NegLLLMBin} will produce a single numeric value.
}
\description{
This function will calculate the negative log likelihood value when the vector of binomial random
variable and vector of corresponding frequencies are given with the input parameters.
}
\details{
\deqn{freq \ge 0}
\deqn{x = 0,1,2,..}
\deqn{0 < p < 1}
\deqn{0 < phi }
}
\examples{
No.D.D <- 0:7       #assigning the random variables
Obs.fre.1 <- c(47,54,43,40,40,41,39,95)    #assigning the corresponding frequencies

NegLLLMBin(No.D.D,Obs.fre.1,.5,3)    #acquiring the negative log likelihood value

}
\references{
\insertRef{elamir2013multiplicative}{fitODBOD}
}
