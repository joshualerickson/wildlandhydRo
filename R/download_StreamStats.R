#' @title Download Multiple Stream Stats Locations
#'
#' Looking at stream stats
#' @param data A data.frame with lat and lon variables
#' @param lon A numeric vector of longitude values
#' @param lat A numeric vector of latitude values
#' @param state An abbreviated State character, e.g. "MT"
#' @param group A vector to group by, e.g. "data$site"
#' @param crs A numeric crs value, e.g. 4326 (default)
#' @return Returns an sf (simple feature) object with associated basin characteristics.
#' @export
#'
#' @examples \dontrun{
#' # Bring in data
#'
#' data <- tibble(Lat = c(48.660, 48.652),
#'                  Lon = c(-115.542, -115.517),
#'                    Site = c("who knows", "oh yeah"))
#'
#' two_sites <- download_StreamStats(lon = data$Lon, lat = data$Lat,
#'                                   state = "MT", group = data$Site,
#'                                   crs = 4326)
#'
#' }



download_StreamStats <- function(lon, lat, state, group = NULL, crs = NULL){




  if (is.null(group)){
    usgs_raws <- cbind.data.frame(lon = lon, lat = lat)
   watersheds <-  usgs_raws %>% mutate(group = row_number()) %>% group_by(group) %>%
     select(lon, lat) %>% nest() %>%
     mutate(ws = map(data,~tryCatch(delineateWatershed(.$lon,.$lat , rcode = state, crs = ifelse(is.null(crs), 4326, crs)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})))

  } else {
    usgs_raws <- cbind.data.frame(lon = lon, lat = lat, group = group)
    watersheds <- usgs_raws %>% group_by(group) %>%
  select(lon, lat) %>% nest() %>%
  mutate(ws = map(data,~tryCatch(delineateWatershed(.$lon,.$lat , rcode = state, crs = ifelse(is.null(crs), 4326, crs)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})))

  }



usgs_ws_polys <- tibble()
usgs_flow_stats <- tibble()

for (i in 1:nrow(watersheds)) {
  tryCatch({
    usgs_ws <- pluck(watersheds$ws) %>% pluck(i)  %>%
      streamstats::writeGeoJSON(., file.path(tempdir(),"ss_tmp.json")) %>%
      geojsonsf::geojson_sf() %>% st_as_sf()
    usgs_ws$group <- pluck(watersheds$group) %>% pluck(i) %>% paste()

    usgs_ws_polys <- plyr::rbind.fill(usgs_ws_polys, usgs_ws)

    flow_stats <- streamstats::computeChars(workspaceID = watersheds$ws[[i]]$workspaceID, rcode = "MT")

    flow_stats <- flow_stats$parameters

    flow_stats$group <- pluck(watersheds$group) %>% pluck(i) %>% paste()

    usgs_flow_stats <- plyr::rbind.fill(usgs_flow_stats, flow_stats)

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

}

usgs_flow_stats <- usgs_flow_stats %>% select(code, value, group) %>% pivot_wider(names_from = code, values_from = value)

usgs_poly <- usgs_ws_polys %>% select(group, HUCID, geometry) %>% left_join(usgs_flow_stats, by = "group") %>% st_as_sf()

return(usgs_poly)
}


