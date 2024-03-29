% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\docType{data}
\name{Epidemic_Cold}
\alias{Epidemic_Cold}
\title{Family Epidemics}
\format{
A data frame with 6 columns and 5 rows
\describe{
\item{\code{Cases}}{No of Further Cases}
\item{\code{Families}}{No of Families}
\item{\code{Father}}{Father with Status of Introducing Cases}
\item{\code{Mother}}{Mother with Status of Introducing Cases}
\item{\code{SChild}}{School Child with Status of Introducing Cases}
\item{\code{PSChild}}{Pre-School Child with Status of Introducing Cases}
}
}
\source{
Extracted from

Heasman, M. A. and Reid, D. D. (1961). "Theory and observation in family epidemics of the common cold."
Br. J. pleu. SOC. Med., 15, 12-16.
}
\usage{
Epidemic_Cold
}
\description{
In this investigation, families of the same size, two parents and three children, living in different
circumstances of domestic overcrowding were visited at fortnightly intervals. The date of onset and the clinical
nature of upper respiratory infectious experienced by each member of the family were charted on a time scale
marked off in days. Family epidemics of acute coryza-or common colds-were thus available for analysis.
}
\details{
By inspection of the epidemic time charts, it was possible to identify new or primary introductions of illness
into the household by the onset of a cold after a lapse of 10 days since the last such case in the same home.
Two such cases occurring on the same or succeeding days were classified as multiple primaries. Thereafter, the
links in the epidemic chain of spread were defined by an interval of one day or more between successive cases
in the same family. These family epidemics could then be described thus 1-2-1, 1-1-1-0, 2-1-0, etc. It must be
emphasized that although this method of classification is somewhat arbitrary, it was completed before the
corresponding theoretical distributions were worked out and the interval chosen agrees with the distribution
of presumptive incubation periods of the common cold seen in field surveys (e.g. Badger, Dingle, Feller,
Hodges, Jordan, and Rammelkamp, 1953).
}
\examples{

Epidemic_Cold$Cases
sum(Epidemic_Cold$SChild)

}
\keyword{datasets}
