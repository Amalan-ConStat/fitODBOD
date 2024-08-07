% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Kumaraswamy.R
\name{dKumBin}
\alias{dKumBin}
\title{Kumaraswamy Binomial Distribution}
\usage{
dKumBin(x,n,a,b,it=25000)
}
\arguments{
\item{x}{vector of binomial random variables}

\item{n}{single value for no of binomial trial}

\item{a}{single value for shape parameter alpha representing a}

\item{b}{single value for shape parameter beta representing b}

\item{it}{number of iterations to converge as a proper
probability function replacing infinity}
}
\value{
The output of \code{dKumBin} gives a list format consisting

\code{pdf}           probability function values in vector form.

\code{mean}          mean of the Kumaraswamy Binomial Distribution.

\code{var}           variance of the Kumaraswamy Binomial Distribution.

\code{over.dis.para} over dispersion value of the Kumaraswamy Distribution.
}
\description{
These functions provide the ability for generating probability function values and
cumulative probability function values for the Kumaraswamy Binomial Distribution.
}
\details{
Mixing Kumaraswamy distribution with Binomial distribution will create the
Kumaraswamy Binomial distribution.  The probability function and cumulative
probability function can be constructed and are denoted below.

The cumulative probability function is the summation of probability
function values.

\deqn{P_{KumBin}(x)= ab{n \choose x} \sum_{j=0}^{it} (-1)^j{b-1 \choose j}B(x+a+aj,n-x+1) }
\deqn{a,b > 0}
\deqn{x = 0,1,2,...n}
\deqn{n = 1,2,3,...}
\deqn{it > 0}

The mean, variance and over dispersion are denoted as
\deqn{E_{KumBin}[x]= nbB(1+\frac{1}{a},b) }
\deqn{Var_{KumBin}[x]= n^2 b(B(1+\frac{2}{a},b)-bB(1+\frac{1}{a},b)^2)+
                       nb(B(1+\frac{1}{a},b)-B(1+\frac{2}{a},b)) }
\deqn{over dispersion= \frac{(bB(1+\frac{2}{a},b)-(bB(1+\frac{1}{a},b))^2)}
                       {(bB(1+\frac{1}{a},b)-(bB(1+\frac{1}{a},b))^2)} }

Defined as \eqn{B(a,b)} is the beta function.

\strong{NOTE} : If input parameters are not in given domain conditions necessary error
messages will be provided to go further.
}
\examples{
\dontrun{
#plotting the random variables and probability values
col <- rainbow(5)
a <- c(1,2,5,10,.85)
plot(0,0,main="Kumaraswamy binomial probability function graph",xlab="Binomial random variable",
ylab="Probability function values",xlim = c(0,10),ylim = c(0,0.5))
for (i in 1:5) {
lines(0:10,dKumBin(0:10,10,a[i],a[i])$pdf,col = col[i],lwd=2.85)
points(0:10,dKumBin(0:10,10,a[i],a[i])$pdf,col = col[i],pch=16)
  }
}

dKumBin(0:10,10,4,2)$pdf  #extracting the pdf values
dKumBin(0:10,10,4,2)$mean #extracting the mean
dKumBin(0:10,10,4,2)$var  #extracting the variance
dKumBin(0:10,10,4,2)$over.dis.para #extracting the over dispersion value

\dontrun{
#plotting the random variables and cumulative probability values
col <- rainbow(5)
a <- c(1,2,5,10,.85)
plot(0,0,main="Cumulative probability function graph",xlab="Binomial random variable",
ylab="Cumulative probability function values",xlim = c(0,10),ylim = c(0,1))
for (i in 1:5) {
lines(0:10,pKumBin(0:10,10,a[i],a[i]),col = col[i])
points(0:10,pKumBin(0:10,10,a[i],a[i]),col = col[i])
  }
}

pKumBin(0:10,10,4,2)    #acquiring the cumulative probability values

}
\references{
\insertRef{xiaohu2011kumaraswamy}{fitODBOD}
}
