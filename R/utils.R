
#'Water Year These functions are hi-jacked from smwrBase package.
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the water year.
#'
#' @param x an object of class "Date" or "POSIXt." Missing values are permitted and
#'result in corresponding missing values in the output.
#' @param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#' @return An ordered factor or numeric vector corresponding to the water year.
#' @note The water year is defined as the period from October 1 to September 30.
#'The water year is designated by the calendar year in which it ends. Thus, the
#'year ending September 30, 1999, is the "1999 water year."
#' @seealso
#Flip for production/manual
#'\code{\link[lubridate]{year}}
#\code{year} (in lubridate package)

waterYear <- function(x, numeric=FALSE) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated verion
  ##    2010Feb17 DLLorenz Added option to return numerics
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}
#'Log-Pearson Type III distribution
#'
#'Density, cumulative probability, quantiles, and random generation for the
#'log-Pearson Type III distribution.
#'
#'Elements of x, q, or p that are missing will result in missing values in the
#'returned data.
#'
#' LogPearsonIII
#' @aliases LogPearsonIII dlpearsonIII plpearsonIII qlpearsonIII rlpearsonIII
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(\code{n}) > 1, then the length is taken
#'to be the number required.
#' @param meanlog vector of means of the distribution of the log-transformed
#'data.
#' @param sdlog vector of standard deviation of the distribution of the
#'log-transformed data.
#' @param skew vector of skewness of the distribution of the log-transformed
#'data.
#' @return Either the density (\code{dlpearsonIII}), cumulative probability
#'(\code{plpearsonIII}), quantile (\code{qlpearsonIII}), or random sample
#'(\code{rlpearsonIII}) for the described distribution.
#' @note The log-Pearson Type III distribution is used extensively in
#'flood-frequency analysis in the United States.
#' @seealso
#Flip for production/manual
#'\code{\link{dpearsonIII}}, \code{\link[stats]{dlnorm}}
#\code{\link{dpearsonIII}}, \code{dlnorm} (in stats package)
#' @keywords manip distribution

dlpearsonIII <- function(x, meanlog = 0, sdlog = 1, skew = 0) {
  retval <- dpearsonIII(log(x), meanlog, sdlog, skew)/x
  return(ifelse(x == 0, 0, retval))
}

#' LogPearsonIII

plpearsonIII <- function(q, meanlog = 0, sdlog = 1, skew = 0) {
  return(ppearsonIII(log(q), meanlog, sdlog, skew))
}

#' LogPearsonIII

qlpearsonIII <- function(p, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(qpearsonIII(p, meanlog, sdlog, skew)))
}

#' LogPearsonIII

rlpearsonIII <- function(n, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(rpearsonIII(n, meanlog, sdlog, skew)))
}

#'Pearson Type III distribution
#'
#'Density, cumulative probability, quantiles, and random generation for the
#'Pearson Type III distribution.
#'
#'Elements of \code{x}, \code{q}, or \code{p} that are missing will result in
#'missing values in the returned data.
#'
#' PearsonIII
#' @aliases PearsonIII dpearsonIII ppearsonIII qpearsonIII rpearsonIII
#' @param x,q vector of quantiles. Missing values are permitted and result in
#'corresponding missing values in the output.
#' @param p vector of probabilities.
#' @param n number of observations. If length(\code{n}) > 1, then the length is taken
#'to be the number required.
#' @param mean vector of means of the distribution of the data.
#' @param sd vector of standard deviation of the distribution of the data.
#' @param skew vector of skewness of the distribution of the data.
#' @return Either the density (\code{dpearsonIII}), cumulative probability
#'(\code{ppearsonIII}), quantile (\code{qpearsonIII}), or random sample
#'(\code{rpearsonIII}) for the described distribution.

#' @note The log-Pearson Type III distribution is used extensively in flood-frequency
#'analysis in the United States. The Pearson Type III forms the basis
#'for that distribution.
#' @seealso
#Flip for production/manual
#'\code{\link{dlpearsonIII}}, \code{\link[stats]{dnorm}}
#\code{\link{dlpearsonIII}}, \code{dnorm} (in stats package)

dpearsonIII <- function(x, mean = 0, sd = 1, skew = 0) {
  ## Coding history:
  ##    2009Aug12 DLLorenz Initial version with combined code
  ##    2009Aug14 DLLorenz Debugged p- functions
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##    2014Jan10 DLLorenz Vectorized for skew
  ##
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the dgamma function or the dnorm function to return the
  ## quantiles desired.
  ##
  ## Replicate all values to ensure consistent results
  Nout <- max(length(x), length(mean), length(sd), length(skew))
  x <- rep(x, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- dnorm(x, mean, sd)
  }
  if(all(skeworg == 0))
    return(ret0)
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  x <- ifelse(skew > 0, x + mn - mean, mn - x + mean)
  rets <- dgamma(x, shape, rate)
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) +
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets)
}

#' PearsonIII

ppearsonIII <- function(q, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the pgamma function or the pnorm function to return the
  ## quantiles desired.
  Nout <- max(length(q), length(mean), length(sd), length(skew))
  q <- rep(q, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- pnorm(q, mean, sd)
  }
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  q <- ifelse(skew > 0, q + mn - mean, mn + mean - q)
  rets <- pgamma(q, shape, rate)
  ## Adjust for negative skew
  rets <- ifelse(skew > 0, rets, 1-rets)
  ## Adjust for near 0 skew
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) +
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets)
}

#' PearsonIII

qpearsonIII <- function(p, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the qgamma function or the qnorm function to return the
  ## quantiles desired.
  Nout <- max(length(p), length(mean), length(sd), length(skew))
  p <- rep(p, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- qnorm(p)
  }
  shape <- 4/skew^2
  rets <- ifelse(skew > 0, (qgamma(p, shape) - shape)/sqrt(shape),
                 (shape - qgamma(1 - p, shape))/sqrt(shape))
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) +
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets * sd + mean)
}

#' PearsonIII
#' @importFrom stats dgamma pgamma qgamma rnorm rgamma

rpearsonIII <- function(n, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the qgamma function or the qnorm function to return the
  ## quantiles desired.
  Nout <- if(length(n) == 1) n else length(n)
  mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- rnorm(n)
  }
  shape <- 4/skew^2
  rets <- (rgamma(n, shape) - shape)/sqrt(shape)
  rets[skew < 0] <- -rets[skew < 0]
  if(any(ckskew))
    rets[ckskew] <- ret0[ckskew]
  return(rets * sd + mean)
}
