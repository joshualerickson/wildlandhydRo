
# future function for USGSdv

prepping_USGSdv <- function(site_no, parameterCd, start_date, end_date) {

  readNWISdv(siteNumbers = site_no,
             parameterCd = parameterCd,
             startDate = start_date,
             endDate = end_date,
             statCd = "00003") %>%
    renameNWISColumns() %>%
    mutate(
      drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
      Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
      lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
      long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
      altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()
    )
}

# future function for peaks USGS

peaks_USGS <- function(site_no){

  dataRetrieval::readNWISpeak(site_no)%>% select(peak_va, peak_dt, site_no) %>%
    mutate(wy = wildlandhydRo:::waterYear(peak_dt, TRUE))

}


# future function for hourly USGS data


hr_USGS <- function(data){
  # download url (metric by default!)
  base_url <- paste0(
    "https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
    data$sites,
    "&period=P",data$choice_days,"D&parameterCd=00060&siteStatus=all"
  )

  # try to download the data
  error <- httr::GET(url = base_url,
                     httr::write_disk(path = file.path(tempdir(),
                                                       "usgs_tmp.csv"),
                                      overwrite = TRUE))
  # read RDB file to R
  df <- utils::read.table(file.path(tempdir(),"usgs_tmp.csv"),
                          header = TRUE,
                          sep = "\t",
                          stringsAsFactors = FALSE)
  #remove excess data
  df <- df[-1,]

  df <- renameNWISColumns(df) %>%
    select(site_no, datetime, dplyr::contains("_Flow"))
  #add metadata

  df <- df %>%
    mutate(Station = paste0(data$station)) %>%
    dplyr::rename(value = contains("_Flow")) %>%
    relocate(Station)

}


