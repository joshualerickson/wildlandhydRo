% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_USGS.R
\name{hourlyUSGS}
\alias{hourlyUSGS}
\title{Hourly USGS}
\usage{
hourlyUSGS(procDV, sites = NULL, days = 7, parallel = FALSE, ...)
}
\arguments{
\item{procDV}{A previously created \link[wildlandhydRo]{batch_USGSdv} object.}

\item{sites}{A \code{vector} of USGS NWIS sites. \code{optional}}

\item{days}{A \code{numeric} input of days.}

\item{parallel}{\code{logical} indicating whether to use future_map().}

\item{...}{arguments to pass on to \link[furrr]{future_map}.}
}
\description{
This function generates hourly NWIS flow data from \url{https://waterservices.usgs.gov/nwis/iv/}.
It takes the instantaneous data and floors to 1-hour data by taking the mean.
}
