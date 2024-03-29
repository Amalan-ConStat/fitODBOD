% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.R
\docType{data}
\name{Plant_DiseaseData}
\alias{Plant_DiseaseData}
\title{Plant Disease Incidence data}
\format{
A data frame with 2 columns and 10 rows
\describe{
\item{\code{Dis.plant}}{Diseased Plants}
\item{\code{fre}}{Observed frequencies}
}
}
\source{
Extracted from

Hughes, G., 1993. Using the Beta-Binomial Distribution to Describe Aggregated Patterns of Disease Incidence.
Phytopathology, 83(9), p.759.

Available at: \doi{10.1094/Phyto-83-759}.
}
\usage{
Plant_DiseaseData
}
\description{
Cochran(1936) provided a data that comprise the number of tomato spotted wilt virus(TSWV) infected
tomato plants in the field trials in Australia. The field map was divided into 160 'quadrats'.
9 tomato plants in each quadrat. then the numbers of TSWV infected tomato plants were counted in each
quadrat.
Number of infected plants out of 9 plants per quadrat can be treated as a binomial variable.
the collection of all such responses from all 160 quadrats would form "binomial outcome data"
below provided is a data set similar to Cochran plant disease incidence data.
Marcus R(1984). orange trees infected with citrus tristeza virus (CTV) in an orchard in central
Israel.
We divided the field map into 84 "quadrats" of 4 rows x 3 columns and counted the total number
(1981 + 1982) of infected trees out of a maximum of n = 12 in each quadrat
}
\examples{
Plant_DiseaseData$Dis.plant      # extracting the binomial random variables
sum(Plant_DiseaseData$fre)       # summing all the frequencies

}
\keyword{datasets}
