% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{compressed}
\alias{compressed}
\title{Compressed Telemetry Data}
\format{
A data frame with 66449 rows and 7 variables:
\describe{
  \item{receiver}{receiver code}
  \item{valid}{is observation valid, 1 = T, 0 = F}
  \item{tag_id}{tag ID including the frequency and code}
  \item{start}{the time of first observation in "\%Y-\%m-\%d \%H:\%M:\%S" format}
  \item{end}{the time of last observation in "\%Y-\%m-\%d \%H:\%M:\%S" format}
  \item{n}{number of observations}
  \item{week}{the week in the study season}
}
}
\source{
Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
}
\usage{
compressed
}
\description{
An example dataset containing compressed observations
for 5 receivers and 1 frequency, 2018/2019 season
}
\keyword{datasets}
