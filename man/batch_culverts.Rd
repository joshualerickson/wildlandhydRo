% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_StreamStats.R
\name{batch_culverts}
\alias{batch_culverts}
\title{Batch Culvert Sizes}
\usage{
batch_culverts(ss, rre, bfw, geo = 1)
}
\arguments{
\item{ss}{A \link[wildlandhydRo]{batch_StreamStats} object}

\item{rre}{A \link[wildlandhydRo]{batch_RRE} object. \code{optional}}

\item{bfw}{A vector of Bankfull Width's (BFW). \code{optional}}

\item{geo}{A geologic parameter, e.g. \code{0-1}}
}
\value{
Returns a data.frame with flood frequencies and culvert size estimations.
}
\description{
This function takes the results from \link[wildlandhydRo]{batch_RRE}
along with the results from \link[wildlandhydRo]{batch_StreamStats} to calculate
culvert sizes based on generated flood frequencies
from \insertCite{ries2017streamstats}{wildlandhydRo}, \insertCite{omang1986methods}{wildlandhydRo} and
\insertCite{parrett2004methods}{wildlandhydRo}. The culvert size estimates use methods from
\insertCite{american1983handbook}{wildlandhydRo}. Right now, this is only good for western Montana so if data
includes other states then they will be \strong{removed} from this analysis. In the future, possibly open for a more
dynamic approach, e.g. (nationwide).
}
\examples{
\dontrun{

#bring in previous batch_StreamStats() and batch_RRE() objects, e.g., three_sites, rre_peak.

culverts_all <- batch_culverts(ss = three_sites, rre = ree, bfw = c(10,12,11))
}
}
\references{
{
\insertAllCited{}
}
}
