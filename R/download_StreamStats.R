
#' @title Download Multiple Stream Stats Locations
#'
#' @description Takes sf point object and returns catchment characteristics and watershed boundary (sf). Uses \link[AOI]{geocode_rev} to get
#' the state identifier and \link[streamstats]{computeChars} and \link[streamstats]{delineateWatershed} to generate basin
#' delineation(s) and characteristics,
#' which use methods from \insertCite{ries2017streamstats}{wildlandhydRo}
#' @param data A \code{data.frame} with \code{lon,lat} variables. \code{optional}
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
#' data <- data %>% sf::st_as_sf(coords('Lon', 'Lat'))
#' three_sites <- batch_StreamStats(data, group = 'Site',
#'                                   crs = 4326)
#'
#' }



batch_StreamStats <- function(data, group, crs = 4326){

    if(!'POINT' %in% sf::st_geometry_type(data)){"Need a sf POINT geometry"}
    data <- data %>% sf::st_transform(crs = crs)
    lon <- data.frame(lon = sf::st_coordinates(data)[,1])
    lat <- data.frame(lat = sf::st_coordinates(data)[,2])
    if(missing(group)){

      group <- data %>%
        sf::st_drop_geometry() %>%
        mutate(group = dplyr::row_number()) %>%
        select(group)
    } else {

      group <- data  %>%
      sf::st_drop_geometry() %>%
        select({{group}})
    }


  # Create a vector of state abbreviations

  st <- vector()

  for(i in 1:nrow(lon)){

    state <-   AOI::geocode_rev(c(lat[i,],lon[i,])) %>% dplyr::select(state)
    state <-  state.abb[grep(paste(state$state), state.name)]
    st <- append(st, state)
  }

# delineateWatershed ----
  # Separate into whether group is null or not
  dl_ws <- function(df){

    for(i in 1:2){

      dl <- tryCatch({streamstats::delineateWatershed(df$lon, df$lat,
                                                      rcode = df$state,
                                                      crs = df$crs)},
                     error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      dl['group'] <- df['group']
      dlt <- pluck(dl$featurecollection)
      dl <- if(!is.null(dlt)) {dl} else NULL
      if(!is.null(dl)) return(dl)
    }
     df
  }


if(!nrow(group) == nrow(lat)) {stop("group is not the same length as lat and lon")}

    usgs_raws <- data.frame(lat = lat, lon = lon, group = group[,1], state = st, crs = crs)

    watersheds <-  usgs_raws %>%
      split(.$group) %>%
      furrr::future_map(safely(~dl_ws(.))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']])


# compute basin characteristics ----
  get_flow_basin <- function(watersheds) {

    usgs_ws <- streamstats::writeGeoJSON(watersheds, file.path(tempdir(),"ss_tmp.json")) %>%
      geojsonsf::geojson_sf() %>% st_as_sf()
    #
    usgs_ws <- usgs_ws %>% mutate(wkID = watersheds$workspaceID,
                                  state = stringr::str_sub(watersheds$workspaceID,end = 2),
                                  group = watersheds$group)

    flow_stats <- streamstats::computeChars(workspaceID = usgs_ws$wkID, rcode = usgs_ws$state)

    flow_stats <- flow_stats$parameters

    flow_stats <- flow_stats %>% mutate(wkID = usgs_ws$wkID)

    final <- left_join(usgs_ws %>% select(wkID,state, group), flow_stats, by = 'wkID')

  }

   final_df <- furrr::future_map(watersheds, safely(~get_flow_basin(.)))%>%
     purrr::map(~.x[['result']]) %>%
     plyr::rbind.fill()

     final_df_wrangle <- final_df %>%
     dplyr::select(code, 'value', group, wkID, state, geometry) %>%
     sf::st_as_sf()%>%
     sf::st_drop_geometry() %>%
     pivot_wider(names_from = code, values_from = value)

     final_sf <- final_df %>%
     dplyr::group_by(group) %>%
     dplyr::slice(n=1) %>%
     dplyr::select(group,geometry) %>%
     dplyr::right_join(final_df_wrangle, by = 'group') %>%
     dplyr::relocate(geometry,.after = dplyr::last_col()) %>%
     sf::st_as_sf()

   dropped_geom <- final_sf %>% sf::st_drop_geometry()

   if(nrow(filter(usgs_raws, !group %in% dropped_geom$group)) > 0) {

     print(paste0("Group(s) not generated: ",
                  filter(usgs_raws, !group %in% dropped_geom$group) %>% select(group)))
   } else {
       print("All groups delineated")
   }

   return(final_sf)

}


#' @title Batch USGS Regional Regression Estimates (RRE)
#' @description Provides the USGS regressions from a \link[wildlandhydRo]{batch_StreamStats} object or manually entered params.
#' Uses methods from \insertCite{ries2017streamstats}{wildlandhydRo} to generate RRE's.
#' @param data A previously created \link[wildlandhydro]{batch_StreamStats} object.
#' @return Returns a data.frame with associated regional regression estimates.
#' @examples \dontrun{
#' # Bring in data
#'
#' #### use batch_StreamStats object
#'
#' data <- tibble(Lat = c(48.30602, 48.62952, 48.14946),
#'                  Lon = c(-115.54327, -114.75546, -116.05935),
#'                    Site = c("Granite Creek", "Louis Creek", "WF Blue Creek"))
#'
#' data <- data %>% sf::st_as_sf(coords('Lon', 'Lat'))
#'
#' three_sites <- batch_StreamStats(data, group = 'Site',
#'                                   crs = 4326)
#'
#' rre_peak <- batch_RRE(three_sites)
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


batch_RRE <- function(data) {

if(!'sf' %in% class(data)){stop('need an sf object with state, wkID and group variables.')}
  data <- data %>% sf::st_drop_geometry()



get_peak_flow <- function(state, wkID, group){

variables <- c( "Name", "code", "Description", "Value", "Equation", "IntervalBounds.Lower", "IntervalBounds.Upper")

  base_url <- paste0(
    "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json?rcode=",state,"&workspaceID=",
    wkID,
    "&includeflowtypes=true"
  )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "peak_tmp.json"),overwrite = TRUE))
  if(error$status_code == 500){

    stop(message('Sever Error 500'))

  } else if(error$status_code == 400){

    stop(message('Sever Error 400'))

  } else if(error$status_code == 404){

    stop(message('Sever Error 404'))

  } else {
  peak_s <- jsonlite::fromJSON(file.path(tempdir(),"peak_tmp.json"))

  peak_s <- (peak_s$RegressionRegions[[1]])[[6]][[1]] %>%
    select(any_of(variables)) %>% mutate(group = group)
  }
 }
if(length(data$group)>1){


  peak_rre <- data %>%
  split(.$group) %>%
  furrr::future_map(safely(~get_peak_flow(.$state, .$wkID, .$group))) %>%
  purrr::keep(~length(.) != 0) %>%
  purrr::map(~.x[['result']])


 peak_rre <- peak_rre %>%
    plyr::rbind.fill() %>%
   dplyr::mutate(return_interval = 1/(readr::parse_number(Name)*0.01))

    peak_group <- peak_rre %>%
    group_by(group) %>%
    dplyr::slice(n=1)



  if(nrow(filter(data, !group %in% peak_group$group)) > 0) {

    print(paste0("Group(s) not generated: ",
                 filter(data, !group %in% peak_group$group) %>% select(group)))
  } else {

    print(paste0('All Groups Delineated'))


  }

} else {

    peak_rre <- get_peak_flow(data$state, data$wkID, data$group)

    peak_rre <- peak_rre %>%
      plyr::rbind.fill() %>%
    dplyr::mutate(return_interval = 1/(readr::parse_number(Name)*0.01))


}

return(peak_rre)
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
#' @importFrom ape where
#' @importFrom plyr rbind.fill
#' @references {
#' \insertAllCited{}
#' }
#' @export
#'

batch_culverts <- function(ss, rre, bfw,geo = 1) {



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

    if(!missing(bfw)){bf_known <- ss$bfw[[i]]}

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

      omp <- data.frame(ReturnInterval = c(2,25,50,100),
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

      pj <-  data.frame(ReturnInterval = c(2,25,50,100),
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

      omp <- data.frame(ReturnInterval = c(2,25,50,100),
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

      pj <-  data.frame(ReturnInterval = c(2,25,50,100),
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


  if(missing(rre)) {

    together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson)

    if (missing(bfw)) {

      together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width_regression"), names_to = "Method") %>%
        mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

    } else {
      together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
        mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

    }

  } else {


  culvert_usgs <- rre %>% select(basin_char = Value, ReturnInterval = return_interval) %>%
      mutate(source = "USGS Regression", group = rre$group) %>% dplyr::filter(ReturnInterval %in% c(2,25,50,100))


  culvert_usgs <- culvert_usgs %>% dplyr::filter(group %in% ss$group)

  together <- plyr::rbind.fill(Omang_parrett_hull_flows, parrett_and_johnson, culvert_usgs)




  if (missing(bfw)) {

    together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width_regression"), names_to = "Method") %>%
      mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

  } else {
    together_long <- together %>% pivot_longer(cols = c("basin_char", "bankfull_width"), names_to = "Method") %>%
      mutate(across(where(is.numeric), round, 0)) %>% mutate(Size = culvert_size(value))

  }
}

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

culvert_size <- function(x) {


  ifelse(x == 0, "(No Data)",
         ifelse(x < 11 & x > 0, "(24 in)",
                ifelse(x >= 11 & x < 30,"(36 in)",
                       ifelse(x >= 30 & x < 65,"(48 in)",
                              ifelse(x >= 65 & x <110,"(60 in)",
                                     ifelse(x >= 110 & x < 180,"(72 in)",
                                            ifelse(x >= 180 & x < 290,"(84 in)",
                                                   ifelse(x >= 290 & x < 400,"(96 in)","(Bridge or Big Culvert!)"))))))))}
