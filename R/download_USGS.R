

#' Batch USGS daily values
#'
#' @param Sites
#'
#' @return A data.frame
#' @export
#'
#' @importFrom dataRetrieval readNWISdv renameNWISColumns readNWISsite
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select
#'
#' @examples
#'
#'

clean_USGSdv <- function(Sites, parameterCd = "00060", start_date = "", end_date = "", statCd = "00003") {



  site_id_usgs <- data.frame(Sites = Sites)


  usgs_raw_dv <- data.frame()

  for (i in 1:nrow(site_id_usgs)) {

  tryCatch({
    usgs_data <-

    message(sprintf("Downloading site: %s, with id: %s\n", # some feedback on the download progress
                    site_id_usgs$Sites[i],
                    readNWISsite(site_id_usgs$Sites[[i]]) %>% select(station_nm) %>% as.character()))

  discharge <- readNWISdv(siteNumbers = site_id_usgs$Sites[[i]], parameterCd = parameterCd,
                          startDate = start_date, endDate = end_date, statCd = statCd) %>%
    renameNWISColumns() %>%
    mutate(
      drainage_area = readNWISsite(site_id_usgs$Sites[[i]]) %>% select(drain_area_va) %>% as.numeric(),
      Station = readNWISsite(site_id_usgs$Sites[[i]]) %>% select(station_nm) %>% as.character(),
      lat = readNWISsite(site_id_usgs$Sites[[i]]) %>% select(dec_lat_va) %>% as.numeric(),
      long = readNWISsite(site_id_usgs$Sites[[i]]) %>% select(dec_long_va) %>% as.numeric(),
      altitude = readNWISsite(site_id_usgs$Sites[[i]]) %>% select(alt_va) %>% as.numeric())


  usgs_raw_dv <- plyr::rbind.fill(usgs_raw_dv, discharge)},

  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }
  usgs_raw_dv <- usgs_raw_dv %>%
    mutate(year = year(Date), month = month(Date), day = day(Date),month_day = str_c(month, day, sep = "-"),
           wy = smwrBase::waterYear(Date, TRUE), month_abb = factor(month.abb[month], levels = month.abb),
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


#' Water Year Stats
#'
#' @param Sites
#' @param parameterCd
#' @param start_date
#' @param end_date
#' @param statCd
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup
#' @importFrom stringr str_c str_remove_all
#' @importFrom smwrBase waterYear
#'
#' @return
#' @export
#'
#' @examples
wyUSGS <- function(cleanDV) {


#this is where we create the minimum and maximum per water year (wy). Also, get the all time mean flow (atmf) so that we can use it later for normalization. By keeping it all together (normalization) makes it easier
usgs_raw <- cleanDV %>% mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))

usgs_min_max_wy <- usgs_raw %>%  group_by(.data$Station) %>%
  mutate(atmf = mean(Flow, na.rm = TRUE)) %>% ungroup() %>%
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

  slice_head(n=1) %>% ungroup()

peak_sites <- data.frame(peaks = unique(usgs_min_max_wy$site_no))
# add peaks
peaks <- data.frame()
for (i in 1:nrow(peak_sites)) {

  peak <- dataRetrieval::readNWISpeak(peak_sites$peaks[[i]]) %>% select(peak_va, peak_dt, site_no)
  peak <- peak %>% mutate(wy = smwrBase::waterYear(peak_dt, TRUE))
  peaks <- plyr::rbind.fill(peaks, peak)

}

usgs_min_max_wy <- usgs_min_max_wy %>% left_join(peaks, by = c("site_no", "wy")) %>%
  select(Station, site_no, wy, Peak = peak_va, peak_dt, everything())

return(usgs_min_max_wy)
}

#' Water Year & Monthly Stats
#'
#' @param cleanDV
#'
#' @return
#' @export
#' @importFrom dplyr group_by mutate across summarise rename right_join ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time ymd
#' @importFrom stringr str_c
#' @importFrom ape where
#'
#' @examples
wymUSGS <- function(cleanDV) {

  usgs_raw_min_max_wy_month <- cleanDV %>%
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

#' Month-Only Stats
#'
#' @param cleanDV
#'
#' @return
#' @export
#' @importFrom dplyr group_by summarise mutate
#'
#' @examples
#'
#'

monthUSGS <- function(cleanDV) {

  usgs_raw_min_max_month <-
    cleanDV  %>% group_by(Station, month_abb) %>%
    summarise(Maximum = round(max(Flow, na.rm = TRUE),2),
              Minimum = round(min(Flow, na.rm = TRUE),2),
              Standard_Deviation = round(sd(Flow, na.rm = TRUE),2),
              Mean = round(mean(Flow, na.rm = TRUE),2),
              Median = round(median(Flow, na.rm = TRUE),2)) %>% ungroup()

  usgs_raw_min_max_month<- usgs_raw_min_max_month %>% mutate(across(where(is.numeric), ~replace(., is.infinite(.), 0)))

return(usgs_raw_min_max_month)
}
