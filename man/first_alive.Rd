% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_alive.R
\name{first_alive}
\alias{first_alive}
\title{First Alive Vector}
\usage{
first_alive(y)
}
\arguments{
\item{y}{detection history matrix of 0's and 1's with one row per individual and one column per detection site.}
}
\value{
a vector containing integers
}
\description{
For each individual, return an integer marking the first occassion that individual is known to be alive.
}
\author{
Kevin See and Mike Ackerman
}
