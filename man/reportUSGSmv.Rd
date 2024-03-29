% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_USGS.R
\name{reportUSGSmv}
\alias{reportUSGSmv}
\title{USGS Report Monthly}
\usage{
reportUSGSmv(procDV, sites = NULL)
}
\arguments{
\item{procDV}{A previously created \link[wildlandhydRo]{batch_USGSdv} object.}

\item{sites}{A \code{character} USGS NWIS site.}
}
\description{
Uses \link[dataRetrieval]{readNWISstat} to gather monthly mean flows. Furthermore,
monthly quantiles are generated similar to \link[wildlandhydRo]{reportUSGSdv}.
}
