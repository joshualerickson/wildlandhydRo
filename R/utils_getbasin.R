#' Get Basin Boundary NLDI
#'
#' @param sf_pt A sf point object or atomic list.
#'
#' @return An sf object.
#' @export
#'

get_Basin <- function(sf_pt){

  if(is.atomic(sf_pt)) {
    point <- sf_pt
    clat <- point[[1]]
    clng <- point[[2]]
    point <- data.frame(clat = clat, clng = clng)
    point <- sf::st_as_sf(point, coords = c("clat", "clng")) %>% sf::st_set_crs(4269)

  } else {

    if(!class(sf::st_geometry(sf_pt)[[1]])[[2]] == "POINT")stop({"Need a sf_POINT object"})

    point <- sf_pt
    clat <- point$geometry[[1]][[2]]
    clng <- point$geometry[[1]][[1]]

  }

  ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                clng,"%20", clat, "%29")

  error_ids <- httr::GET(url = ids,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "nld_tmp.json"),overwrite = TRUE))

  nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))


  nldiURLs <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/",nld$features$properties$identifier,"/basin")


    nldi_data <- sf::read_sf(nldiURLs)


}
library(tidyverse)
drain <- get_Basin(c(48.228,-115.23))

drain %>% ggplot() + geom_sf()
