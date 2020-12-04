

#' Delineate watersheds
#' @description Hijacked streamstats function delineateWatershed() to add a retry() call because
#' frequently the function will send a 'set vector 0/0' but maybe just needs to be ran again?
#' This is a possible work-around but is likely to go away because I have suspicion it doesn't work...
#' @param df A filtered data.frame
#' @return hopefully a geojson feature collection. If not then either in AK or server is not responding correctly and a NULL is returned
#' @importFrom magrittr "%>%"
#' @importFrom purrr pluck

dl_ws <- function(df){

  for(i in 1:2){

    dl <- tryCatch({streamstats::delineateWatershed(df$lon, df$lat, rcode = df$state, crs = df$crs)},
                   error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    dlt <- pluck(dl$featurecollection)
    dl <- if(!is.null(dlt)) {dl} else NULL
    if(!is.null(dl)) return(dl)

  }
  df
}



#' Delineate watersheds
#' @description Hijacked streamstats function computeChars() and writeGeoJSON to run a for loop getting the geojson polygon and associated
#' polygon stats
#' @param watersheds A nested data.frame
#' @param st A vector with state abbreviations
#' @return two tibbles. one with flow stats and the other with basin stats
#' @importFrom magrittr "%>%"
#' @importFrom purrr pluck
#' @importFrom streamstats computeChars writeGeoJSON
#' @importFrom geojsonsf geojson_sf
#' @importFrom plyr rbind.fill
#' @importFrom dplyr tibble
#' @importFrom sf st_as_sf
#'
#'

get_flow_basin <- function(watersheds, st) {
  usgs_ws_polys <- tibble()
  usgs_flow_stats <- tibble()

  for (i in 1:nrow(watersheds)) {
    tryCatch({
      usgs_ws <- pluck(watersheds$ws) %>% pluck(i)  %>%
        streamstats::writeGeoJSON(., file.path(tempdir(),"ss_tmp.json")) %>%
        geojsonsf::geojson_sf() %>% st_as_sf()

      usgs_ws$group <- pluck(watersheds$group) %>% pluck(i) %>% paste()
      usgs_ws$wkID <- watersheds$ws[[i]]$workspaceID
      usgs_ws$state <- paste(st[[i]])
      usgs_ws_polys <- plyr::rbind.fill(usgs_ws_polys, usgs_ws)

      flow_stats <- streamstats::computeChars(workspaceID = watersheds$ws[[i]]$workspaceID, rcode = st[[i]])

      flow_stats <- flow_stats$parameters

      flow_stats$group <- pluck(watersheds$group) %>% pluck(i) %>% paste()

      usgs_flow_stats <- plyr::rbind.fill(usgs_flow_stats, flow_stats)

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

  bundle_list <- list(usgs_flow_stats = usgs_flow_stats, usgs_ws_polys = usgs_ws_polys)
  return(bundle_list)
}







#' @title Download Multiple Stream Stats Locations
#'
#' @description Takes longitude and latitude vectors and returns an sf object. Uses \link[AOI]{geocode_rev} to get
#' the state identifier and \link[streamstats]{computeChars} and \link[streamstats]{delineateWatershed} to generate basin
#' delineation(s) and characteristics,
#' which use methods from \insertCite{ries2017streamstats}{wildlandhydRo}
#' @param data A \code{data.frame} with \code{lon,lat} variables. \code{optional}
#' @param lon A \code{numeric} vector of longitude values
#' @param lat A \code{numeric} vector of latitude values
#' @param group A vector to group by. \code{optional}
#' @param crs A \code{numeric} crs value
#' @references {
#' \insertAllCited{}
#' }
#' @importFrom Rdpack reprompt
#' @importFrom geojsonsf geojson_sf
#' @importFrom sf st_as_sf
#' @importFrom purrr map
#' @importFrom tidyr nest pivot_wider
#' @importFrom dplyr group_by left_join select filter mutate row_number tibble
#' @importFrom plyr rbind.fill
#' @importFrom magrittr "%>%"
#' @return Returns an sf (simple feature) object with associated basin characteristics.
#' @export
#'
#' @examples \dontrun{
#' # Bring in data
#'
#' data <- tibble(Lat = c(48.30602, 48.62952, 48.14946),
#'                  Lon = c(-115.54327, -114.75546, -116.05935),
#'                    Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))
#'
#' three_sites <- batch_StreamStats(lon = data$Lon, lat = data$Lat, group = data$Site,
#'                                   crs = 4326)
#'
#' }



batch_StreamStats <- function(data = NULL,lon, lat, group, crs = 4326){

  # masking
if(!is.null(data)){
  lon <- data %>% mutate(lon = {{ lon }}) %>% select(lon)
  lat <- data %>% mutate(lat = {{ lat }}) %>% select(lat)
  if(!missing(group)){group <- data %>% mutate(group = {{ group }}) %>% select(group)}
  }

  if(is.null(data)){

    lon <- data.frame(lon = lon)
    lat <- data.frame(lat = lat)
    if(!missing(group)){group <- data.frame(group = {{ group }})}
    if(missing(group)){group <- data.frame(group = 1)}
  }


  if(!nrow(lon) == nrow(lat)) {stop("lat and lon are not the same length")}


  # Create a vector of state abbreviations

  st <- vector()

  for(i in 1:nrow(lon)){

    state <-   AOI::geocode_rev(c(lat[i,],lon[i,])) %>% dplyr::select(state)
    state <-  state.abb[grep(paste(state$state), state.name)]
    st <- append(st, state)
  }

# delineateWatershed ----
  # Separate into whether group is null or not

  if (missing(group)){

    usgs_raws <- cbind.data.frame(lat = lat, lon = lon, state = st, crs = crs)

   watersheds <-  usgs_raws %>% mutate(group = row_number()) %>% group_by(group) %>%
     nest() %>%
     mutate(ws = map(data,~dl_ws(.)))

  } else {

if(!nrow(group) == nrow(lat)) {stop("group is not the same length as lat and lon")}

    usgs_raws <- cbind.data.frame(lat = lat, lon = lon, group = group, state = st, crs = crs)

    watersheds <- usgs_raws %>% group_by(group) %>%
      nest() %>%
  mutate(ws = map(data,~dl_ws(.)))

  }

# compute basin characteristics ----

bundled_list <- get_flow_basin(watersheds, st = st)

# Bring it together ----

usgs_flow_stats <- bundled_list$usgs_flow_stats %>%
    select(code, value, group) %>%
    pivot_wider(names_from = code, values_from = value)

usgs_poly <- bundled_list$usgs_ws_polys  %>%
  select(group, HUCID,wkID,state, geometry) %>%
  left_join(usgs_flow_stats, by = "group") %>% st_as_sf()

# return usgs_poly if not using groups

if(is.null(group)){return(usgs_poly)}

#if using groups let the user know which ones if any didn't parse
if(nrow(filter(usgs_raws, !group %in% usgs_poly$group)) > 0) {print(paste0("Group(s) not generated: ", filter(usgs_raws, !group %in% usgs_poly$group) %>% select(group)))}


return(usgs_poly)
}







#' @title Batch USGS Regional Regression Estimates (RRE)
#' @description Provides the USGS regressions from a \link[wildlandhydRo]{batch_StreamStats} object or manually entered params.
#' Uses methods from \insertCite{ries2017streamstats}{wildlandhydRo} to generate RRE's.
#' @param data A \link[wildlandhydro]{batch_StreamStats} with \code{state and wkID} variables
#' @param state An abbreviated State \code{character}, e.g. "MT"
#' @param wkID A workspace ID generated in \link[wildlandhydRo]{batch_StreamStats}
#' @param group A vector to group by. \code{optional}
#' @return Returns a data.frame with associated regional regression estimates.
#' @examples \dontrun{
#' # Bring in data
#'
#' data <- tibble(state = "MT", wkID = "MT20201116184949291000")
#'
#' rre_peak <- batch_RRE(state = data$state, wkID = data$wkID)
#'
#' #### Or use batch_StreamStats object
#'
#' data <- tibble(Lat = c(48.3060, 48.6293, 48.1494),
#'                  Lon = c(-115.5432, -114.7554, -116.0593),
#'                    Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))
#'
#' three_sites <- batch_StreamStats(lon = data$Lon, lat = data$Lat, group = data$Site,
#'                                   crs = 4326)
#'
#' rre_peak <- batch_RRE(state = three_sites$state, wkID = three_sites$wkID, group = three_sites$group)
#'
#' }
#' @importFrom Rdpack reprompt
#' @importFrom dplyr select tibble all_of
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET write_disk
#' @importFrom plyr rbind.fill
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'


batch_RRE <- function(data = NULL, state, wkID, group) {

# data masking

if(!is.null(data)){
  state <- data %>% sf::st_drop_geometry() %>% mutate(state = {{ state }}) %>% select(state)

  wkID <- data %>% sf::st_drop_geometry() %>% mutate(wkID = {{ wkID }}) %>% select(wkID)
if(!missing(group)){group <- data %>% sf::st_drop_geometry() %>% mutate(group = {{ group }}) %>% select(group)}
} else {

  state <- data.frame(state = state)
  wkID <- data.frame(wkID = wkID)
  if(!missing(group)){group <- data.frame(group = {{ group }})}
  if(missing(group)){group <- data.frame(group = dplyr::row_number())}
}

  if(!nrow(state) == nrow(wkID)) {stop("wkID and state are not the same length")}

  variables <- c( "Name", "code", "Description", "Value", "Equation")

  #if less than 1 ID then just run below code
if (nrow(wkID) <= 1) {

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state[1,],"&workspaceID=",
    wkID[1,],
    "&includeflowtypes=true"
  )


  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "peak_tmp.json"),overwrite = TRUE))


  peak_s <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))


  peak <- (peak_s$RegressionRegions[[1]])[[6]][[1]] %>%
    select(all_of(variables)) %>%
    mutate(group = {{ group }}) %>%
    as.data.frame() #could change in future to be more dynamic


} else {

  #iterate through workspaceID's and states
 peak <- tibble()

  for(i in 1:nrow(wkID)){

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state[i,],"&workspaceID=",
    wkID[i,],
    "&includeflowtypes=true"
  )


  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "peak_tmp.json"),overwrite = TRUE))

  if (httr::http_error(error)) {
    stop(sprintf("Downloading site %s failed, server is not responding or down.",
                    group[i]))
  }

  peak_s <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))



  peak_s <- (peak_s$RegressionRegions[[1]])[[6]][[1]] %>%
    select(all_of(variables)) #could change in future to be more dynamic

   # what to name the groups if used
if(missing(group)){

  peak_s <- peak_s %>% mutate(group = paste0(i)) %>% as.data.frame()

} else {

  if(!nrow(group) == nrow(state)) {stop("group is not the same length as wkID or state")}
  peak_s <- peak_s %>% mutate(group = paste0(group[i,])) %>% as.data.frame()

  }

  #route into tibble()

 peak <- plyr::rbind.fill(peak, peak_s)

}

}
 return(peak)

}


#' Batch Culvert Sizes
#'
#' @description  This function takes the results from \link[wildlandhydRo]{batch_RRE}
#' along with the results from \link[wildlandhydRo]{batch_StreamStats} to calculate
#' culvert sizes based on generated flood frequencies
#' from \insertCite{ries2017streamstats}{wildlandhydRo}, \insertCite{omang1986methods}{wildlandhydRo} and
#' \insertCite{parrett2004methods}{wildlandhydRo}. The culvert size estimates use methods from
#' \insertCite{american1983handbook}{wildlandhydRo}. Right now, this is only good for western Montana so if data
#' includes other states then they will be \strong{removed} from this analysis. In the future, possibly open for a more
#' dynamic approach, e.g. (nationwide).
#' @param ss A \link[wildlandhydRo]{batch_StreamStats} object
#' @param rre A \link[wildlandhydRo]{batch_RRE} object. \code{optional}
#' @param bfw A vector of Bankfull Width's (BFW). \code{optional}
#' @param geo A geologic parameter, e.g. \code{0-1}
#' @return Returns a data.frame with flood frequencies and culvert size estimations.
#' @examples \dontrun{
#'
#' #bring in previous batch_StreamStats() and batch_RRE() objects, e.g., three_sites, rre_peak.
#'
#' culverts_all <- batch_culverts(ss = three_sites, rre = ree, bfw = c(10,12,11))
#' }
#' @importFrom Rdpack reprompt
#' @importFrom readr parse_number
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr across filter mutate select
#' @imortFrom ape where
#' @importFrom plyr rbind.fill
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'

batch_culverts <- function(ss, rre = NULL, bfw,geo = 1) {



  if (missing(bfw)) {

    ss <- ss %>% mutate(geo = {{ geo }})

  } else {
    bfw_test <- data.frame(bfw_test = {{ bfw }})
    if(!nrow(bfw_test) == nrow(ss)){stop("bfw is not the same length as ss")}
    ss <- ss %>% mutate(bfw = {{ bfw }}, geo = {{ geo }})
  }



  ss <- ss %>% dplyr::filter(state %in% "MT")

  if(nrow(ss)<1) {stop("No states from Montana")}

  if (!"group" %in% colnames(ss)) {stop("Need a 'group' variable, like 'group' from batch_StreamStats()")}


  #need bfw as a vector

  Omang_parrett_hull_flows <- data.frame()

  parrett_and_johnson <- data.frame()

  for (i in 1:nrow(ss)){

    drain_area <- ss$CONTDA[[i]]

    precip_drain <- ss$PRECIP[[i]]

    bf_known <- ss$bfw[[i]]

    geo_known <- ss$geo[[i]]

    for_known <- ss$FOREST[[i]]



    bf_regres <- if(precip_drain < 30) {
      3.99*drain_area^0.441
    } else if (precip_drain > 45) {

      7.7*drain_area^0.441
    } else {

      6.04*drain_area^0.441
    }




    if (!missing(bfw)) {

      omp <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                        basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                       0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                       0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                       0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                        bankfull_width = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_known^1.14),
                                           0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_known^1.02),
                                           0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_known^1.01),
                                           0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_known^1)),
                        source = c("Omang, Parrett and Hull"),
                        group = ss$group[[i]])

      pj <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                        basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                       8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                       13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                       18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                        bankfull_width = c(0.281*(bf_known^1.98),
                                           1.75*(bf_known^1.72),
                                           2.34*(bf_known^1.69),
                                           2.99*(bf_known^1.66)),
                        source = c("Parrett & Johnson"), group = ss$group[[i]])
    } else {

      omp <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                        basin_char = c(0.037*(drain_area^0.95)*(precip_drain^1.52)*geo_known,
                                       0.324*(drain_area^0.84)*(precip_drain^1.26)*geo_known,
                                       0.451*(drain_area^0.82)*(precip_drain^1.22)*geo_known,
                                       0.594*(drain_area^0.8)*(precip_drain^1.2)*geo_known),
                        bankfull_width_regression = c(0.041*(drain_area^0.47)*(precip_drain^0.86)*(bf_regres^1.14),
                                           0.465*(drain_area^0.4)*(precip_drain^0.61)*(bf_regres^1.02),
                                           0.663*(drain_area^0.38)*(precip_drain^0.58)*(bf_regres^1.01),
                                           0.899*(drain_area^0.37)*(precip_drain^0.55)*(bf_regres^1)),
                        source = c("Omang, Parrett and Hull"),
                        group = ss$group[[i]])

      pj <-  data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                        basin_char = c(0.268*(drain_area^0.927)*(precip_drain^1.6)*(for_known+1)^(-0.508),
                                       8.5*(drain_area^0.835)*(precip_drain^1.14)*(for_known+1)^(-0.639),
                                       13.2*(drain_area^0.823)*(precip_drain^1.09)*(for_known+1)^(-0.652),
                                       18.7*(drain_area^0.812)*(precip_drain^1.06)*(for_known+1)^(-0.664)),
                        bankfull_width_regression = c(0.281*(bf_regres^1.98),
                                           1.75*(bf_regres^1.72),
                                           2.34*(bf_regres^1.69),
                                           2.99*(bf_regres^1.66)),
                        source = c("Parrett & Johnson"),
                        group = ss$group[[i]])
    }

    Omang_parrett_hull_flows <- plyr::rbind.fill(Omang_parrett_hull_flows, omp)
    parrett_and_johnson <- plyr::rbind.fill(parrett_and_johnson, pj)
  }
  #culvert estimation function


  if(is.null(rre)) {

    together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson)

    together <- together %>% mutate(RI = parse_number(ReturnInterval))

    if (missing(bfw)) {

      together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width_regression"), names_to = "Method") %>%
        mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

    } else {
      together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
        mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

    }

  } else {
  culvert_usgs <- rre

    culvert_usgs <- culvert_usgs %>% select(basin_char = Value, ReturnInterval = Name) %>%
      mutate(source = "USGS Regression", group = rre$group) %>% dplyr::filter(ReturnInterval %in% c("2 Year Peak Flood", "25 Year Peak Flood",
                                                                                             "50 Year Peak Flood", "100 Year Peak Flood"))
    }

  culvert_usgs <- culvert_usgs %>% dplyr::filter(group %in% ss$group)

  together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson, culvert_usgs)

  together <- together %>% mutate(RI = parse_number(ReturnInterval))



  if (missing(bfw)) {

    together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width_regression"), names_to = "Method") %>%
      mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

  } else {
    together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
      mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

  }

  return(together_long)
}

#' Culvert Size
#'
#' @description This function let's you enter any numeric vector of flows and returns recommended culvert
#' sizes based on \insertCite{american1983handbook}{wildlandhydRo}.
#' @param x A vector of \code{numeric} flows
#'
#' @return A \code{data.frame}
#' @importFrom Rdpack reprompt
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'
#' @examples
culvert_size <- function(x) {


  ifelse(x == 0, "(No Data)",
         ifelse(x < 11 & x > 0, "(24 in)",
                ifelse(x >= 11 & x < 30,"(36 in)",
                       ifelse(x >= 30 & x < 65,"(48 in)",
                              ifelse(x >= 65 & x <110,"(60 in)",
                                     ifelse(x >= 110 & x < 180,"(72 in)",
                                            ifelse(x >= 180 & x < 290,"(84 in)",
                                                   ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)"))))))))}
