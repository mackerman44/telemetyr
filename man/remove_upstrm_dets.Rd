% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_upstrm_dets.R
\name{remove_upstrm_dets}
\alias{remove_upstrm_dets}
\title{Remove Upstream Detections}
\usage{
remove_upstrm_dets(ch_long)
}
\arguments{
\item{ch_long}{capture history in long format, with columns named \code{tag_id}, \code{loc}, \code{first_obs}, \code{last_obs} and \code{n}. The \code{loc} column must be a factor with the levels corresponding to site codes in order from upstream to downstream}
}
\value{
a data frame containing all records in the .txt receiver downloads in \code{path}
}
\description{
Drops detections from an upstream receiver
}
\author{
Kevin See and Mike Ackerman
}
