

#' Delineate watersheds
#' @description Hijacked streamstats function delineateWatershed() to add a retry() call because
#' frequently the function will send a 'set vector 0/0' but really just needs to be ran again.
#' This is the work-around.
#' @param df A filtered data.frame
#' @return hopefully a geojson feature collection. if not then either in AK or server is not responding correctly
#' @importFrom streamstats delineateWatershed
#' @importFrom magrittr "%>%"
#' @importFrom purrr pluck

dl_ws <- function(df){

  for(i in 1:3){

  dl <- tryCatch({delineateWatershed(df$lon, df$lat, rcode = df$state, crs = df$crs)},
                 error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  dlt <- pluck(dl$featurecollection)
  dl <- if(!is.null(dlt)) {dl} else NULL
  if(!is.null(dl)) return(dl)

  }
df
}
