

#' Process USGS daily values
#' @description This function is basically a wrapper around \link[dataRetrieval]{readNWISdv} but includes
#' added variables like water year, lat/lon, station name, altitude and tidied dates.
#' @param sites A vector of USGS NWIS sites
#'
#' @return A \code{data.frame} with daily mean flow and added meta-data.
#' @export
#'
#' @importFrom dataRetrieval readNWISdv renameNWISColumns readNWISsite
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @examples
#'
#'

proc_USGSdv <- function(sites, parameterCd = "00060", start_date = "", end_date = "", statCd = "00003") {



  site_id_usgs <- data.frame(sites = sites)


  usgs_raw_dv <- data.frame()

  for (i in 1:nrow(site_id_usgs)) {

  tryCatch({
    usgs_data <-

    message(sprintf("Downloading site: %s, with id: %s\n", # some feedback on the download progress
                    site_id_usgs$sites[i],
                    readNWISsite(site_id_usgs$sites[[i]]) %>% select(station_nm) %>% as.character()))

  discharge <- readNWISdv(siteNumbers = site_id_usgs$sites[[i]], parameterCd = parameterCd,
                          startDate = start_date, endDate = end_date, statCd = statCd) %>%
    renameNWISColumns() %>%
    mutate(
      drainage_area = readNWISsite(site_id_usgs$sites[[i]]) %>% select(drain_area_va) %>% as.numeric(),
      Station = readNWISsite(site_id_usgs$sites[[i]]) %>% select(station_nm) %>% as.character(),
      lat = readNWISsite(site_id_usgs$sites[[i]]) %>% select(dec_lat_va) %>% as.numeric(),
      long = readNWISsite(site_id_usgs$sites[[i]]) %>% select(dec_long_va) %>% as.numeric(),
      altitude = readNWISsite(site_id_usgs$sites[[i]]) %>% select(alt_va) %>% as.numeric())


  usgs_raw_dv <- plyr::rbind.fill(usgs_raw_dv, discharge)},

  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }
  usgs_raw_dv <- usgs_raw_dv %>%
    mutate(year = year(Date), month = month(Date),
           day = day(Date),month_day = str_c(month, day, sep = "-"),
           wy = smwrBase::waterYear(Date, TRUE),
           month_abb = factor(month.abb[month], levels = month.abb),
           month_day = str_c(month, day, sep = "-"))

  #Getting some more statistics: standard deviation of the log(dailyFlow) per station.
  usgs_raw_dv <- usgs_raw_dv %>%
    mutate(month_day = str_c(month, day, sep = "-"))

  #here we get the monthly mean per station per wy. Also create 'Station' as a factor for styling in app
  usgs_raw_dv <- usgs_raw_dv %>%
    group_by(Station, wy, month_abb) %>%
    mutate(Q_month = mean(Flow, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Station = factor(Station))

return(usgs_raw_dv)
}


#' Water Year Stats (USGS)
#' @description This function uses the results of the \link[wildlandhydRo]{proc_USGSdv} object to
#' generate mean, maximum, median and standard deviation per water year. It also includes peaks from
#' \link[dataRetrieval]{readNWISpeak} and normalization
#' by \code{log(Flow)/log(drainage area)} and \code{log(Flow)/log(all time mean flow)}.
#' @param procDV A previously created \link[wildlandhydRo]{proc_USGSdv} object.
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup
#' @importFrom stringr str_c str_remove_all
#' @importFrom smwrBase waterYear
#'
#' @return
#' @export
#'
#' @examples
wyUSGS <- function(procDV) {


#this is where we create the minimum and maximum per water year (wy).
  #Also, get the all time mean flow (atmf) so that we can use it later for normalization.
    #By keeping it all together (normalization) makes it easier for exploration.

usgs_raw <- procDV %>% mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))

usgs_min_max_wy <- usgs_raw %>%
  group_by(.data$Station) %>%
  mutate(atmf = mean(Flow, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Station,wy, site_no) %>%
  summarise(Maximum = round(max(Flow, na.rm = TRUE),2),
            Minimum = round(min(Flow, na.rm = TRUE),2),
            Mean = round(mean(Flow, na.rm = TRUE),2),
            Median = round(median(Flow, na.rm = TRUE),2),
            Standard_Deviation = round(sd(Flow, na.rm = TRUE),2),
            Max_dnorm = log(Maximum)/log(drainage_area),
            Min_dnorm = log(Minimum)/log(drainage_area),
            Mean_dnorm = log(Mean)/log(drainage_area),
            Med_dnorm = log(Median)/log(drainage_area),
            Max_sdnorm = log(Maximum)/sd(log(Flow), na.rm = TRUE),
            Min_sdnorm = log(Minimum)/sd(log(Flow), na.rm = TRUE),
            Mean_sdnorm = log(Mean)/sd(log(Flow), na.rm = TRUE),
            Med_sdnorm = log(Median)/sd(log(Flow), na.rm = TRUE),
            Max_avg = log(Maximum)/log(atmf),
            Min_avg = log(Minimum)/log(atmf),
            Mean_avg = log(Mean)/log(atmf),
            Med_avg = log(Median)/log(atmf),
            log_drainage = log(drainage_area),
            flow_sum = sum(Flow),
            Drainage_area = drainage_area,
            drainage_area_cut = cut(drainage_area, breaks =  c(0,50, 150, 400, 600, 800, 1000, 2000, 5000, Inf), dig.lab = 10),
            drainage_area_cut = str_remove_all(drainage_area_cut, c("\\[|\\]" = "", "\\(|\\)" = "", "," = "-")),
            drainage_area_cut = factor(drainage_area_cut, levels = c("0-50","50-150", "150-400", "400-600", "600-800", "800-1000", "1000-2000", "2000-5000", "5000-Inf"))) %>%
            slice_head(n=1) %>%
            ungroup()

# add peaks
peak_sites <- data.frame(peaks = unique(usgs_min_max_wy$site_no))


peaks <- data.frame()
for (i in 1:nrow(peak_sites)) {

  peak <- dataRetrieval::readNWISpeak(peak_sites$peaks[[i]]) %>%
                            select(peak_va, peak_dt, site_no)
  peak <- peak %>%
            mutate(wy = smwrBase::waterYear(peak_dt, TRUE))

  peaks <- plyr::rbind.fill(peaks, peak)

}

usgs_min_max_wy <- usgs_min_max_wy %>% left_join(peaks, by = c("site_no", "wy")) %>%
  select(Station, site_no, wy, Peak = peak_va, peak_dt, everything())

return(usgs_min_max_wy)
}

#' Water Year & Monthly Stats (USGS)
#'
#' @description This function uses the results of the \link[wildlandhydRo]{proc_USGSdv} object to
#' generate mean, maximum, median and standard deviation per water year per month.
#' @param procDV A previously created \link[wildlandhydRo]{proc_USGSdv} object.
#' @return A \code{data.frame}
#' @export
#' @importFrom dplyr group_by mutate across summarise rename right_join ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time ymd
#' @importFrom stringr str_c
#' @importFrom ape where
#'
#' @examples
wymUSGS <- function(procDV) {

  usgs_raw_min_max_wy_month <- procDV %>%
    group_by(Station, wy, month_abb, month) %>%
    summarise(Maximum = round(max(Flow, na.rm = TRUE),2),
              Minimum = round(min(Flow, na.rm = TRUE),2),
              Mean = round(mean(Flow, na.rm = TRUE),2),
              Median = round(median(Flow, na.rm = TRUE),2),
              Standard_Deviation = round(sd(Flow, na.rm = TRUE),2),
              flow_sum = sum(Flow)) %>% ungroup() %>%
    mutate(year_month = str_c(wy, month,"1", sep = "-"),
           year_month =  parse_date_time(year_month, orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
           year_month = ymd(as.character(year_month)))

usgs_raw_min_max_wy_month<- usgs_raw_min_max_wy_month %>%
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), 0)))

  return(usgs_raw_min_max_wy_month)
}

#' Month-Only Stats (USGS)
#'
#' @description This function uses the results of the \link[wildlandhydRo]{proc_USGSdv} object to
#' generate mean, maximum, median and standard deviation for month-only.
#' @param procDV A previously created \link[wildlandhydRo]{proc_USGSdv} object.
#' @return A \code{data.frame}
#' @export
#' @importFrom dplyr group_by summarise mutate relocate
#'
#' @examples
#'
#'

monthUSGS <- function(procDV) {

  usgs_raw_min_max_month <-
    procDV  %>%
    group_by(Station, month_abb) %>%
    summarise(Maximum = round(max(Flow, na.rm = TRUE),2),
              Minimum = round(min(Flow, na.rm = TRUE),2),
              Standard_Deviation = round(sd(Flow, na.rm = TRUE),2),
              Mean = round(mean(Flow, na.rm = TRUE),2),
              Median = round(median(Flow, na.rm = TRUE),2)) %>%
              ungroup()

  usgs_raw_min_max_month<- usgs_raw_min_max_month %>%
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), 0)))

return(usgs_raw_min_max_month)
}


#' Hourly USGS
#'
#' @param cleanDV
#' @param days
#'
#' @return
#' @export
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom dplyr mutate rename group_by mutate relocate summarise ungroup
#' @importFrom dataRetrieval renameNWISColumns readNWISsite
#' @importFrom httr GET write_disk http_error
#' @importFrom plyr rbind.fill
#'
#' @examples
#'
hourlyUSGS <- function(Sites = NULL, procDV = NULL, days = 7) {

  if(length(days) > 1){stop("only length 1 vector")}
  if(!is.null(Sites) & !is.null(procDV)){stop("Can't use both Sites and procDV")}

  choice_days <- days
  #create iteration value

  if(!is.null(procDV)){

  sites <- unique(procDV$site_no)
  station <- unique(procDV$Station)

  }

  if(!is.null(Sites) & length(Sites) == 1){

    sites <- unique(Sites)

    station <- readNWISsite(sites) %>% select(station_nm) %>% as.character()

    }

  if(!is.null(Sites) & length(Sites) > 1) {

      sites <- unique(Sites)

      station <- vector()
    for(i in 1:length(sites)){

      station_1 <- readNWISsite(sites[[i]]) %>% select(station_nm) %>% as.character()
      station <- append(station,station_1)
    }

  }


  #create blank dataframe to store the information in
  usgs_download_hourly <- data.frame()

  #run for loop over api, pulling necessary data.

  for (i in seq_along(sites)){
    # loop over selection, and download the data

    tryCatch({

      message(sprintf("Downloading site: %s, with id: %s\n",
                      sites[i],
                      station[i]))

    # download url (metric by default!)
    base_url <- paste0(
      "https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=",
      sites[i],
      "&period=P",choice_days,"D&parameterCd=00060&siteStatus=all"
    )

    # try to download the data
    error <- httr::GET(url = base_url,
                       httr::write_disk(path = file.path(tempdir(),
                                                         "usgs_tmp.csv"),overwrite = TRUE))
    if (httr::http_error(error)) {
      warning(sprintf("Downloading site %s failed, removed empty file.",
                      station[i]))
    }

    # read RDB file to R
    df <- utils::read.table(file.path(tempdir(),"usgs_tmp.csv"),
                            header = TRUE,
                            sep = "\t",
                            stringsAsFactors = FALSE)
    #remove excess data
    df <- df[-1,]

    df <- renameNWISColumns(df) %>%
      select(site_no, datetime, contains("_Flow"))
    #add metadata

    df <- df %>%
      mutate(Station = paste0(station[i])) %>%
      dplyr::rename(value = contains("_Flow")) %>%
      relocate(Station)

    # cleanup temporary file (if it exists)
    if(file.exists(file.path(tempdir(),"usgs_tmp.json"))){
      file.remove(file.path(tempdir(), "usgs_tmp.json"))
    }

    #combine df with blank dataframe (usgs_download_hourly)

    usgs_download_hourly <- plyr::rbind.fill(usgs_download_hourly, df)


    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

  #final step: clean up data

  usgs_download_hourly <- usgs_download_hourly %>%
    mutate(date = ymd_hm(datetime), value = as.numeric(value1),
           fl_val = ifelse(is.na(value), paste("Error"), "Good"))

  usgs_download_hourly <- usgs_download_hourly %>%
    mutate(date=floor_date(date, "1 hour")) %>%
    group_by(Station,date,fl_val) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    ungroup()


}
