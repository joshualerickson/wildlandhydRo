% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_StreamStats.R
\name{dl_ws}
\alias{dl_ws}
\title{Delineate watersheds}
\usage{
dl_ws(df)
}
\arguments{
\item{df}{A filtered data.frame}
}
\value{
hopefully a geojson feature collection. If not then either in AK or server is not responding correctly and a NULL is returned
}
\description{
Hijacked streamstats function delineateWatershed() to add a retry() call because
frequently the function will send a 'set vector 0/0' but maybe just needs to be ran again?
This is a possible work-around but is likely to go away because I have suspicion it doesn't work...
}
