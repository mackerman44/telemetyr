% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/known_alive.R
\name{known_alive}
\alias{known_alive}
\title{Known Alive Matrix}
\usage{
known_alive(y)
}
\arguments{
\item{y}{detection history matrix of 0's and 1's with one row per individual and one column per detection site.}
}
\value{
a vector containing 1's and NA's
}
\description{
Fill in detection history matrix with 1's at points where individual was known to be alive, and NA's elsewhere
}
\author{
Kevin See and Mike Ackerman
}
