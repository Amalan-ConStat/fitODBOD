% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Triangle.R
\name{EstMLETriBin}
\alias{EstMLETriBin}
\title{Estimating the mode value for Triangular Binomial Distribution}
\usage{
EstMLETriBin(x,freq)
}
\arguments{
\item{x}{vector of binomial random variables.}

\item{freq}{vector of frequencies.}
}
\value{
The output of \code{EstMLETriBin} will produce the classes of \code{ml} and \code{mlTB}
format consisting

\code{min}  Negative log likelihood value.

\code{mode}  Estimated mode value.

\code{AIC}   AIC value.

\code{call} the inputs for the function.

Methods \code{print}, \code{summary}, \code{coef} and \code{AIC} can be used to
extract specific outputs.
}
\description{
The function will estimate the mode value using the maximum log likelihood method for the
Triangular Binomial Distribution when the binomial random variables and corresponding frequencies
are given.
}
\details{
\deqn{0 < mode=c < 1}
\deqn{x = 0,1,2,...}
\deqn{freq \ge 0}

\strong{NOTE} : If input parameters are not in given domain conditions necessary error
 messages will be provided to go further.
}
\examples{
No.D.D <- 0:7   #assigning the random variables
Obs.fre.1 <- c(47,54,43,40,40,41,39,95)   #assigning the corresponding frequencies

\dontrun{
#estimating the mode value and extracting the mode value
results <- EstMLETriBin(No.D.D,Obs.fre.1)

# extract the mode value and summary
coef(results)
summary(results)

AIC(results)  #show the AIC value
}

}
\references{
Horsnell, G. (1957). Economic acceptance sampling schemes. Journal of the Royal Statistical Society,
Series A, 120:148-191.

Karlis, D. & Xekalaki, E., 2008. The Polygonal Distribution. In Advances in Mathematical and Statistical
Modeling. Boston: Birkhuser Boston, pp. 21-33.

Available at: \doi{10.1007/978-0-8176-4626-4_2}.

Okagbue, H. et al., 2014. Using the Average of the Extreme Values of a Triangular Distribution for a
Transformation, and Its Approximant via the Continuous Uniform Distribution. British Journal of Mathematics
& Computer Science, 4(24), pp.3497-3507.

Available at: \doi{10.9734/BJMCS/2014/12299}.
}
