#' Get Basin Boundary NLDI
#' @description  This function uses the USGS water data API to link a point to a realized basin. This is
#' not the same as delineating from the exact point, rather this API uses NLDI to find the closest
#' basin downstream source point. There is a lot you can do with this API and I would recommend
#' looking at {nhdplusTools} as that has a lot of functionality and better documentation.
#' @param sf_pt A sf point object.
#'
#' @return An sf object with added \code{comid} and \code{basin}.
#' @notes \code{sf_pt} needs geometry column.
#' @export
#'

get_Basin <- function(sf_pt){


    if(!class(sf::st_geometry(sf_pt)[[1]])[[2]] == "POINT")stop({"Need a sf_POINT object"})

    #just added indexs to group by

    sf_pt <- sf_pt %>% dplyr::mutate(rowid = dplyr::row_number())

    nldi_basin_function <- function(sf_pt){

    clat <- sf_pt$geometry[[1]][[2]]
    clng <- sf_pt$geometry[[1]][[1]]
    rowid <- sf_pt$rowid
    ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

    error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

    nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))


    nldiURLs <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin")

    nldi_data <- sf::read_sf(nldiURLs)

    nldi_data <- nldi_data %>%
      dplyr::mutate(comid = nld$features$properties$identifier,
                    rowid = rowid)

    }

    final_basin <- sf_pt %>%
      split(.$rowid) %>%
      furrr::future_map(safely(~nldi_basin_function(.))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()%>%
      sf::st_as_sf() %>%
      dplyr::left_join(sf::st_drop_geometry(sf_pt), by = 'rowid') %>%
      dplyr::select(-rowid)

}

#' Get Basin Stats
#'
#' @description This function will get pre-loaded basin statistics (zonal stats)
#' for a provided comid. A comid can be generated in the get_Basin().
#' @param data A previously created get_Basin() object or an sf object with comid column.
#'
#' @return An sf object with stats.
#' @export
#'
get_BasinStats <- function(data){

  #just added indexs to group by

  comid <- data %>% dplyr::mutate(rowid = dplyr::row_number())


  get_basin_stats_function <- function(comid){

  rowid <- comid$rowid


  local_characteristic <-  nhdplusTools::get_nldi_characteristics(list(featureSource = "comid", featureID = as.character(comid$comid)),
                                                    type = "local")

  local_characteristic <- local_characteristic %>% rbind.fill() %>% mutate(comid = comid$comid,
                                                                           rowid = rowid) %>%
    dplyr::filter(stringr::str_detect(characteristic_id, "CAT_RECHG|CAT_ET|CAT_PET|CAT_PPT7100_ANN|CAT_TWI|CAT_BFI")) %>%
    dplyr::select(comid, characteristic_id, characteristic_value) %>%
    tidyr::pivot_wider(names_from = "characteristic_id", values_from = "characteristic_value") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("CAT"), as.numeric),
           CAT_PPT7100_ANN = CAT_PPT7100_ANN*0.0393701,
           Deficit = CAT_PET-CAT_ET)


  cat <- dplyr::right_join(comid, local_characteristic, by = 'comid') %>% st_as_sf()
  }

  final_basin_stats <- comid %>%
    split(.$rowid) %>%
    furrr::future_map(safely(~get_basin_stats_function(.))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    plyr::rbind.fill() %>%
    sf::st_as_sf() %>%
    dplyr::left_join(sf::st_drop_geometry(comid), by = 'rowid') %>%
    dplyr::select(-rowid)
}
