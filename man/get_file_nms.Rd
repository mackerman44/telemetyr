% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_file_nms.R
\name{get_file_nms}
\alias{get_file_nms}
\title{Get File Names}
\usage{
get_file_nms(path = ".", receiver_codes = NULL)
}
\arguments{
\item{path}{a path to the directory containing all of the files downloaded from radio telemetry receivers
for a study season and using the Tracker software}

\item{receiver_codes}{character vector of receiver codes to download files for.
The default is \code{NULL}, which will keep all receiver codes found in the path folder}
}
\value{
a data frame of all files contained in \code{path}
}
\description{
Create a data frame of all downloaded files for a telemetry season
}
\author{
Kevin See and Mike Ackerman
}
