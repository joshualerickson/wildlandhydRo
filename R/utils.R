
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


#smoothing function
smooth_func <- function(percentiles, smooth.span){
  percentiles <- percentiles %>%
    mutate( p75 = predict(loess(p75~day.of.year, data = percentiles, span = smooth.span)),
            p25 = predict(loess(p25~day.of.year, data = percentiles, span = smooth.span)),
            p10 = predict(loess(p10~day.of.year, data = percentiles, span = smooth.span)),
            p05 = predict(loess(p05~day.of.year, data = percentiles, span = smooth.span)),
            min = predict(loess(min~day.of.year, data = percentiles, span = smooth.span)))
}



#function used to filter through different names but same result

attribute_filter_darea = function(ss){

  if(ss$state %in% c('ID', 'OR', 'WA')){

    ss$DRNAREA

  } else if (ss$state %in% 'MT'){

    ss$CONTDA
  } else{}

}

attribute_filter_forest = function(ss){

  if(ss$state %in% c( 'WA')){

    ss$CANOPY_PCT

  } else if (ss$state %in% c('OR', 'MT', 'ID')){

    ss$FOREST
  } else{}

}
