#' @title Download Multiple Stream Stats Locations
#'
#' @description Takes longitude and latitude vectors and returns an sf object. Uses \link[AOI]{geocode_rev} to get
#' the state identifier and \link[streamstats]{computeChars} and \link[streamstats]{delineateWatershed} to generate basin
#' delineation(s) and characteristics,
#' which use methods from \insertCite{ries2017streamstats}{wildlandhydRo}
#' @param lon A \code{numeric} vector of longitude values
#' @param lat A \code{numeric} vector of latitude values
#' @param group A vector to group by
#' @param crs A \code{numeric} crs value
#' @references {
#' \insertAllCited{}
#' }
#' @importFrom Rdpack reprompt
#' @return Returns an sf (simple feature) object with associated basin characteristics.
#' @export
#'
#' @examples \dontrun{
#' # Bring in data
#'
#' data <- tibble(Lat = c(48.30602, 48.62952, 48.14946),
#'                  Lon = c(-115.54327, -114.75546, -116.05935),
#'                    Site = c("Granite Creek", "Libby Creek", "WF Blue Creek"))
#'
#' three_sites <- batch_StreamStats(lon = data$Lon, lat = data$Lat, group = data$Site,
#'                                   crs = 4326)
#'
#' }



batch_StreamStats <- function(lon, lat, group = NULL, crs = 4326){

  # Create a vector of state abbreviations using AOI::geocode_rev()

  st <- vector()

  for(i in seq_along(lon)){

    state <-   AOI::geocode_rev(c(lat[i],lon[i])) %>% dplyr::select(state)
    state <-  state.abb[grep(paste(state$state), state.name)]
    st <- append(st, state)
  }

# delineateWatershed ----
  # Separate into whether group is null or not

  if (is.null(group)){

    usgs_raws <- cbind.data.frame(lat = lat, lon = lon, state = st)

   watersheds <-  usgs_raws %>% mutate(group = row_number()) %>% group_by(group) %>%
     nest() %>%
     mutate(ws = map(data,~tryCatch(delineateWatershed(.$lon,.$lat , rcode = .$state, crs = ifelse(crs == 4326, 4326, crs)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})))

  } else {

    usgs_raws <- cbind.data.frame(lat = lat, lon = lon, group = group, state = st)

    watersheds <- usgs_raws %>% group_by(group) %>%
      nest() %>%
  mutate(ws = map(data,~tryCatch(delineateWatershed(.$lon,.$lat , rcode = .$state, crs = ifelse(crs == 4326, 4326, crs)), error=function(e){cat("ERROR :",conditionMessage(e), "\n")})))

  }

# compute basin characteristics ----

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

# Bring it together ----

usgs_flow_stats <- usgs_flow_stats %>% select(code, value, group) %>% pivot_wider(names_from = code, values_from = value)

usgs_poly <- usgs_ws_polys  %>% select(group, HUCID,wkID,state, geometry) %>% left_join(usgs_flow_stats, by = "group") %>% st_as_sf()

return(usgs_poly)
}







#' @title Batch USGS Reginal Regression Estimates (RRE)
#' @description Provides the USGS regressions from a \link[wildlandhydRo]{batch_StreamStats} object or manually entered params.
#' Uses methods from \insertCite{ries2017streamstats}{wildlandhydRo} to generate RRE's.
#' @param state An abbreviated State \code{character}, e.g. "MT"
#' @param wkID A workspace ID generated in \link[wildlandhydRo]{batch_StreamStats}
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
#' data <- tibble(Lat = c(48.30602, 48.62952, 48.14946),
#'                  Lon = c(-115.54327, -114.75546, -116.05935),
#'                    Site = c("Granite Creek", "Libby Creek", "WF Blue Creek"))
#'
#' three_sites <- batch_StreamStats(lon = data$Lon, lat = data$Lat, group = data$Site,
#'                                   crs = 4326)
#'
#' rre_peak <- batch_RRE(state = three_sites$state, wkID = three_sites$wkID, group = three_sites$group)
#'
#' }#' @importFrom Rdpack reprompt
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'


batch_RRE <- function(state, wkID, group = NULL) {


  #if less than 1 ID then just run below code
if (length(wkID) <= 1) {

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
    wkID,
    "&includeflowtypes=true"
  )


  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "peak_tmp.json"),overwrite = TRUE))


  peak <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))


  peak_s <- (peak_s$RegressionRegions[[1]])[[6]][[1]] %>%
    select(Name, code, Description, Value, Equation) #could change in future to be more dynamic


} else {

  #iterate through workspaceID's and states
 peak <- tibble()

  for(i in seq_along(wkID)){

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state[[i]],"&workspaceID=",
    wkID[[i]],
    "&includeflowtypes=true"
  )


  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "peak_tmp.json"),overwrite = TRUE))


  peak_s <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))

  peak_s <- (peak_s$RegressionRegions[[1]])[[6]][[1]] %>%
    select(Name, code, Description, Value, Equation) #could change in future to be more dynamic

   # what to name the groups if used
if(is.null(group)){
  peak_s
} else {
  peak_s <- peak_s %>% mutate(group = paste(group[[i]])) %>% as.data.frame()
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
#' @param rre A \link[wildlandhydRo]{batch_RRE} object
#' @param ss A \link[wildlandhydRo]{batch_StreamStats} object
#' @param bfw A vector of Bankfull Width's (BFW)
#' @param geo A geologic parameter, e.g. \code{0-1}
#' @return Returns a data.frame with flood frequencies and culvert size estimations.
#' @examples
#' @importFrom Rdpack reprompt
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'

batch_culverts <- function(ss,rre = NULL, bfw = NULL,acw = NULL, geo = 1) {

  if (is.null(bfw)) {

    ss <- ss %>% mutate(geo = geo)

  } else {

    ss <- ss %>% mutate(bfw = bfw, geo = geo)
  }


  ss <- ss %>% filter(state %in% "MT")

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




    if (!is.null(bfw)) {

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

  culvert_size <- function(x) {
    ifelse(x == 0, "(No Data)",
           ifelse(x < 11 & x > 0, "(24 in)",
                  ifelse(x >= 11 & x < 30,"(36 in)",
                         ifelse(x >= 30 & x < 65,"(48 in)",
                                ifelse(x >= 65 & x <110,"(60 in)",
                                       ifelse(x >= 110 & x < 180,"(72 in)",
                                              ifelse(x >= 180 & x < 290,"(84 in)",
                                                     ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)"))))))))}
  if(is.null(rre)) {

    together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson)

    together <- together %>% mutate(RI = parse_number(ReturnInterval))

    if (is.null(bfw)) {

      together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width_regression"), names_to = "Method") %>%
        mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

    } else {
      together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
        mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

    }

  } else {
  culvert_usgs <- rre

  if (is.null(culvert_usgs)) {

    culvert_usgs <- data.frame(ReturnInterval = c("2 Year Peak Flood", "25 Year Peak Flood", "50 Year Peak Flood", "100 Year Peak Flood"),
                               basin_char = rep(0), source = "USGS Regression", group = rre$group)

  } else {

    culvert_usgs <- culvert_usgs %>% select(basin_char = Value, ReturnInterval = Name) %>%
      mutate(source = "USGS Regression", group = rre$group) %>% filter(ReturnInterval %in% c("2 Year Peak Flood", "25 Year Peak Flood",
                                                                                             "50 Year Peak Flood", "100 Year Peak Flood"))}

  culvert_usgs <- culvert_usgs %>% filter(group %in% ss$group)

  together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson, culvert_usgs)

  together <- together %>% mutate(RI = parse_number(ReturnInterval))



  if (is.null(bfw)) {

    together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width_regression"), names_to = "Method") %>%
      mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

  } else {
    together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
      mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

  }
  }
  return(together_long)
}
