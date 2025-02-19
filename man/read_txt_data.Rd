% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_txt_data.R
\name{read_txt_data}
\alias{read_txt_data}
\title{Read in Raw .txt Format Tracker Data}
\usage{
read_txt_data(path = ".", receiver_codes = NULL)
}
\arguments{
\item{path}{a path to the directory containing all of the files downloaded from radio telemetry receivers
for a study season and using the Tracker software}

\item{receiver_codes}{character vector of receiver codes to download files for.
The default is \code{NULL}, which will keep all receiver codes found in the path folder}
}
\value{
a data frame containing all records in the .txt receiver downloads in \code{path}
}
\description{
Reads in the raw records from telemetry receivers stored in the .txt
format from the Tracker software
}
\author{
Kevin See and Mike Ackerman
}
