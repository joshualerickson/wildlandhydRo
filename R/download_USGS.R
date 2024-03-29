

#' Process USGS daily values
#' @description This function is basically a wrapper around \link[dataRetrieval]{readNWISdv} but includes
#' added variables like water year, lat/lon, station name, altitude and tidied dates.
#' @param sites A vector of USGS NWIS sites
#' @param parameterCd A USGS code for metric, default is "00060".
#' @param start_date A character of date format, e.g. \code{"1990-09-01"}
#' @param end_date A character of date format, e.g. \code{"1990-09-01"}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{data.frame} with daily mean flow and added meta-data.
#' @export
#'
#' @importFrom dataRetrieval readNWISdv renameNWISColumns readNWISsite
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select

batch_USGSdv <- function(sites, parameterCd = "00060", start_date = "", end_date = "", parallel = FALSE, ...) {


  logic <- nchar(sites) > 8

if(TRUE %in% logic){stop("Only sites with 8 character length")}


  site_id_usgs <- data.frame(sites = sites)

  prepping_USGSdv <- function(site_no) {

    gage_data <- readNWISdv(siteNumbers = site_no,
               parameterCd = parameterCd,
               startDate = start_date,
               endDate = end_date,
               statCd = "00003") %>%
      renameNWISColumns()

    # could use attr(gage_data, 'siteInfo') but not a big deal IMO

    gage_info <- tibble(
        site_no = site_no,
        drainage_area = readNWISsite(site_no) %>% select(drain_area_va) %>% as.numeric(),
        Station = readNWISsite(site_no) %>% select(station_nm) %>% as.character(),
        lat = readNWISsite(site_no) %>% select(dec_lat_va) %>% as.numeric(),
        long = readNWISsite(site_no) %>% select(dec_long_va) %>% as.numeric(),
        altitude = readNWISsite(site_no) %>% select(alt_va) %>% as.numeric()
      )

    left_join(gage_data, gage_info, by = 'site_no')
  }

  if(isTRUE(parallel)){

    usgs_raw_dv <- site_id_usgs %>%
      split(.$sites) %>%
      furrr::future_map(safely(~prepping_USGSdv(.$sites)),
                        ...) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>% plyr::rbind.fill()

  } else {

    usgs_raw_dv <- site_id_usgs %>%
                split(.$sites) %>%
                purrr::map(safely(~prepping_USGSdv(.$sites))) %>%
                purrr::keep(~length(.) != 0) %>%
                purrr::map(~.x[['result']]) %>% plyr::rbind.fill()

  }


  #NEED TO ADD QC FOR MISSING SERVER CALLS


if(nrow(usgs_raw_dv) < 1){message("server couldn't get data")}


  ###### might add this in the future if problems or concerns arise hijacked from lauren deCicco#####
                      #### but with slider might not need it ####
  #Check for missing days, if so, add NA rows:
  # if(as.numeric(diff(range(dailyQ$Date))) != (nrow(dailyQ)+1)){
  #   fullDates <- seq(from=min(dailyQ$Date),
  #                    to = max(dailyQ$Date), by="1 day")
  #   fullDates <- data.frame(Date = fullDates,
  #                           agency_cd = dailyQ$agency_cd[1],
  #                           site_no = dailyQ$site_no[1],
  #                           stringsAsFactors = FALSE)
  #   dailyQ <- full_join(dailyQ, fullDates,
  #                       by=c("Date","agency_cd","site_no")) %>%
  #     arrange(Date)
  # }

  usgs_raw_dv <- usgs_raw_dv %>%
    mutate(Date = lubridate::as_date(Date),
           year = year(Date),
           month = month(Date),
           day = day(Date),
           month_day = str_c(month, day, sep = "-"),
           wy = waterYear(Date, TRUE),
           month_abb = factor(month.abb[month], levels = month.abb),
           month_day = str_c(month, day, sep = "-"))

  #here we get the monthly mean per station per wy. Also create 'Station' as a factor for styling in app
  usgs_raw_dv <- usgs_raw_dv %>%
    group_by(Station, wy, month_abb) %>%
    mutate(Q_month = mean(Flow, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Station = factor(Station))

return(usgs_raw_dv)
}


#' Water Year Stats (USGS)
#' @description This function uses the results of the \link[wildlandhydRo]{batch_USGSdv} object to
#' generate mean, maximum, median and standard deviation per water year. It also includes peaks from
#' \link[dataRetrieval]{readNWISpeak}; annual base-flow and Base-flow Index (BFI) (total-flow/base-flow) from \link[lfstat]{baseflow}
#'  ; annual coefficient of variance \code{sd of flow/mean of flow}; and normalization methods
#' \code{Flow/drainage area}, \code{Flow/(all time mean flow)} and \code{Flow/log(standard deviation)}. The
#' window for all the methods are annual, e.g. 1. This leaves it up to the user to explore different windows if inclined.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} vector with NWIS site numbers.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[wildlandhydRo]{batch_USGSdv}.
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup everything row_number n
#' @importFrom stringr str_c str_remove_all
#' @importFrom stats median sd
#' @importFrom lfstat baseflow
#'
#' @return
#' @export
wyUSGS <- function(procDV, sites = NULL, parallel = FALSE, ...) {


if(missing(procDV)) {

  if(isTRUE(parallel)){

    usgs_raw <- batch_USGSdv(sites = sites, parallel = parallel, ...) %>%
      mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))

  } else {

    usgs_raw <- batch_USGSdv(sites = sites, ...) %>%
    dplyr::mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))

  }

} else {

usgs_raw <- procDV %>%
  mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))}


#this is where we create the minimum and maximum per water year (wy).
  #Also, get the all time mean flow (atmf) so that we can use it later for normalization.
    #By keeping it all together (normalization) makes it easier for exploration.
usgs_min_max_wy <- usgs_raw %>%
  group_by(Station) %>%
  mutate(atmf = mean(Flow, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Station,wy, site_no) %>%
  summarise(Count = n(),
            Maximum = round(max(Flow, na.rm = TRUE),2),
            Minimum = round(min(Flow, na.rm = TRUE),2),
            Mean = round(mean(Flow, na.rm = TRUE),2),
            Median = round(median(Flow, na.rm = TRUE),2),
            Standard_Deviation = round(sd(Flow, na.rm = TRUE),2),
            coef_var = Standard_Deviation/Mean,
            flow_sum = sum(Flow),
            bflow = baseflow(Flow),
            bflowsum = sum(bflow, na.rm = TRUE),
            bfi = bflowsum/flow_sum,
            Max_dnorm = Maximum/drainage_area,
            Min_dnorm = Minimum/drainage_area,
            Mean_dnorm = Mean/drainage_area,
            Med_dnorm = Median/drainage_area,
            Max_sdnorm = log(Maximum)/sd(log(Flow), na.rm = TRUE),
            Min_sdnorm = log(Minimum)/sd(log(Flow), na.rm = TRUE),
            Mean_sdnorm = log(Mean)/sd(log(Flow), na.rm = TRUE),
            Med_sdnorm = log(Median)/sd(log(Flow), na.rm = TRUE),
            sdNorm = sd(log(Flow), na.rm = TRUE),
            Max_avg = Maximum/atmf,
            Min_avg = Minimum/atmf,
            Mean_avg = Mean/atmf,
            Med_avg = Median/atmf,
            drainage_area_cut = cut(drainage_area, breaks =  c(0,50, 150, 400, 600, 800, 1000, 2000, 5000, Inf), dig.lab = 10),
            drainage_area_cut = str_remove_all(drainage_area_cut, c("\\[|\\]" = "", "\\(|\\)" = "", "," = "-")),
            drainage_area_cut = factor(drainage_area_cut, levels = c("0-50","50-150", "150-400", "400-600", "600-800", "800-1000", "1000-2000", "2000-5000", "5000-Inf"))) %>%
            slice_head(n=1) %>% select(-bflow) %>%
            ungroup()

# add peaks
peak_sites <- data.frame(peaks = unique(usgs_min_max_wy$site_no))

  peaks_USGS <- function(site_no){

    dataRetrieval::readNWISpeak(site_no)%>% select(peak_va, peak_dt, site_no) %>%
      mutate(wy = wildlandhydRo:::waterYear(peak_dt, TRUE))

  }

if(isTRUE(parallel)){

  peaks <- peak_sites %>% split(.$peaks) %>%
    furrr::future_map(safely(~peaks_USGS(.$peaks)), ...) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>% plyr::rbind.fill()

} else {

  peaks <- peak_sites %>% split(.$peaks) %>%
    purrr::map(safely(~peaks_USGS(.$peaks))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>% plyr::rbind.fill()

}


usgs_min_max_wy <- usgs_min_max_wy %>% left_join(peaks, by = c("site_no", "wy")) %>%  dplyr::select(Station, site_no, wy, Peak = peak_va, peak_dt, dplyr::everything())

return(usgs_min_max_wy)
}

#' Water Year & Monthly Stats (USGS)
#'
#' @description This function uses the results of the \link[wildlandhydRo]{batch_USGSdv} object to
#' generate mean, maximum, median and standard deviation per water year per month. Also included is
#' monthly base-flow and Base-flow Index (BFI) (total-flow/base-flow) from \link[lfstat]{baseflow}
#' monthly coefficient of variance \code{sd of flow/mean of flow}. Again, the
#' window for all the methods are monthly, e.g. 1. This leaves it up to the user to explore different windows if inclined.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} vector with NWIS site numbers.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[wildlandhydRo]{batch_USGSdv}.
#' @return A \code{data.frame}
#' @export
#' @importFrom dplyr group_by mutate across summarise rename right_join ungroup n
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time ymd
#' @importFrom stringr str_c
#' @importFrom ape where
#'
wymUSGS <- function(procDV, sites = NULL, parallel = FALSE, ...) {

  if(missing(procDV)) {

    if(isTRUE(parallel)){

      usgs_raw <- batch_USGSdv(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- batch_USGSdv(sites = sites, parallel = FALSE, ...)

    }


  } else {

    usgs_raw <- procDV }

  usgs_raw_min_max_wy_month <- usgs_raw %>%
    group_by(Station, wy, month_abb, month) %>%
    summarise(Count = n(),
              Maximum = round(max(Flow, na.rm = TRUE),2),
              Minimum = round(min(Flow, na.rm = TRUE),2),
              Mean = round(mean(Flow, na.rm = TRUE),2),
              Median = round(median(Flow, na.rm = TRUE),2),
              Standard_Deviation = round(sd(Flow, na.rm = TRUE),2),
              coef_var = Standard_Deviation/Mean,
              flow_sum = sum(Flow),
              bflow = baseflow(Flow),
              bflowsum = sum(bflow, na.rm = TRUE),
              bfi = bflowsum/flow_sum) %>%
    select(-bflow) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    mutate(year_month = str_c(wy, month,"1", sep = "-"),
           year_month =  parse_date_time(year_month, orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
           year_month = ymd(as.character(year_month)))

usgs_raw_min_max_wy_month<- usgs_raw_min_max_wy_month %>%
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), 0)))

  return(usgs_raw_min_max_wy_month)
}

#' Month-Only Stats (USGS)
#'
#' @description This function uses the results of the \link[wildlandhydRo]{batch_USGSdv} object to
#' generate mean, maximum, median and standard deviation for month-only.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} vector with NWIS site numbers.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map} and \link[wildlandhydRo]{batch_USGSdv}.
#' @return A \code{data.frame}
#' @export
#' @importFrom dplyr group_by summarise mutate relocate
#'
#'

monthUSGS <- function(procDV, sites = NULL, parallel = FALSE, ...) {

  if(missing(procDV)) {

    if(isTRUE(parallel)){

      usgs_raw <- batch_USGSdv(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- batch_USGSdv(sites = sites, parallel = FALSE, ...)

    }

  } else {

    usgs_raw <- procDV }

  usgs_raw_min_max_month <-
    usgs_raw  %>%
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
#' @description This function generates hourly NWIS flow data from \url{https://waterservices.usgs.gov/nwis/iv/}.
#' It takes the instantaneous data and floors to 1-hour data by taking the mean.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{vector} of USGS NWIS sites. \code{optional}
#' @param days A \code{numeric} input of days.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return
#' @export
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom dplyr mutate rename rename_with group_by mutate relocate summarise ungroup contains
#' @importFrom dataRetrieval renameNWISColumns readNWISsite
#' @importFrom httr GET write_disk http_error
#' @importFrom plyr rbind.fill
#'
hourlyUSGS <- function(procDV, sites = NULL, days = 7, parallel = FALSE, ...) {

  if(length(days) > 1){stop("only length 1 vector")}
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  choice_days <- days
  #create iteration value

  if(!missing(procDV)){

  sites <- unique(procDV$site_no)
  station <- unique(procDV$Station)

  }

  if(!is.null(sites) & length(sites) == 1){

    sites <- unique(sites)

    station <- readNWISsite(sites) %>% select(station_nm) %>% as.character()

    }

  if(!is.null(sites) & length(sites) > 1) {

      sites <- unique(sites)

      station <- vector()
    for(i in 1:length(sites)){

      station_1 <- readNWISsite(sites[[i]]) %>% select(station_nm) %>% as.character()
      station <- append(station,station_1)
    }

  }

  #create blank dataframe to store the information in
  site_station_days <- data.frame(sites = sites, station = station, choice_days = rep(choice_days))

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
  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_download_hourly <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~hr_USGS(.))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  } else {

    usgs_download_hourly <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~hr_USGS(.)))%>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  }

  #final step: clean up data

  usgs_download_hourly <- usgs_download_hourly %>%
  mutate(date = ymd_hm(datetime),
         value = ifelse(is.na(value1), NA_real_, as.numeric(value1)),
           fl_val = ifelse(is.na(value), paste("Error"), "Good"))

  usgs_download_hourly <- usgs_download_hourly %>%
    mutate(date=floor_date(date, "1 hour")) %>%
    group_by(Station,site_no, date,fl_val) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup()
}


#' Plot Water Year Base-flow and Total Flow
#'
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param wy A \code{numeric} or \code{character} water year, e.g. 1990:2000, 1990, or c("1990", "2000").
#' @param parallel \code{logical} indicating whether to use future_map() if using \code{sites} argument.
#' @param ... arguments to pass on to \link[furrr]{future_map}, \link[wildlandhydRo]{batch_USGSdv} and \link[lfstat]{baseflow}.
#' @importFrom dplyr rename filter
#' @importFrom ggplot2 ggplot geom_line theme_light labs scale_color_manual facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom lfstat baseflow
#' @return
#' @export

plot_baseflow <- function(procDV, sites = NULL, wy, parallel = FALSE, ...) {

  if(missing(wy))stop({"Need wy!"})

  if(missing(procDV)) {

    if(isTRUE(parallel)){

       usgs_raw <- batch_USGSdv(sites = sites, parallel = TRUE, ...)

    } else {

      usgs_raw <- batch_USGSdv(sites = sites, parallel = FALSE, ...)

    }


  } else {

    usgs_raw <- procDV }

  usgs_baseflow <- usgs_raw  %>%
    filter(wy %in% {{ wy }}) %>%
    group_by(Station) %>% mutate(bf = lfstat::baseflow(Flow, ...)) %>%
    rename(`Total Flow` = "Flow", `Base-flow` = "bf")

if(length(unique(usgs_baseflow$Station))>1){

  usgs_baseflow %>% pivot_longer(cols = c(`Total Flow`, `Base-flow`)) %>%
    ggplot() +
    geom_line(aes(Date, value, color = name)) +
    theme_light() +
    labs(x = "Water Year", y = "Discharge", title = "Base-flow to Total Flow", color = "Flow Types") +
    scale_color_manual(values = c("red", "black"), labels = c("Base-flow", "Total Flow")) +
    facet_wrap(~Station, scales = "free")

} else {

  usgs_baseflow %>% pivot_longer(cols = c(`Total Flow`, `Base-flow`)) %>%
    ggplot() +
    geom_line(aes(Date, value, color = name)) +
    theme_light() +
    labs(x = "Water Year", y = "Discharge", title = "Base-flow to Total Flow", color = "Flow Types") +
    scale_color_manual(values = c("red", "black"), labels = c("Base-flow", "Total Flow"))
}
}



#' USGS Report Daily
#' @description This function uses the \link[dataRetrieval]{readNWISstat} to gather daily
#' statistics like quantiles/percentiles. Be aware, the \code{current_daily_mean_flow} variable is calculated from the \link[wildlandhydRo]{hourlyUSGS}
#' by taking the daily mean of the hourly data. Thus, the \code{current_daily_mean_flow} will look different than the \link[wildlandhydRo]{hourlyUSGS} and
#' \href{https://waterdata.usgs.gov/nwis/rt}{USGS Current Water Data} site instantaneous values as it should.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param days A \code{numeric} input of days.
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#' @importFrom dataRetrieval readNWISstat
#' @importFrom lubridate mday
#' @importFrom dplyr tibble
#' @return
#' @export
#'
reportUSGSdv <- function(procDV, sites = NULL, days = 10, parallel = FALSE, ...) {


  if(is.null(sites)) {sites <- unique(procDV[['site_no']])}
  choice_days <- days
  #create iteration value

  if(!missing(procDV)){

    sites <- unique(procDV$site_no)
    station <- unique(procDV$Station)

  }

  if(!is.null(sites) & length(sites) == 1){

    sites <- unique(sites)

    station <- readNWISsite(sites) %>% select(station_nm) %>% as.character()

  }

  if(!is.null(sites) & length(sites) > 1) {

    sites <- unique(sites)

    station <- vector()
    for(i in 1:length(sites)){

      station_1 <- readNWISsite(sites[[i]]) %>% select(station_nm) %>% as.character()
      station <- append(station,station_1)
    }

  }

  #create blank dataframe to store the information in
  site_station_days <- data.frame(sites = sites, station = station, choice_days = rep(choice_days))


    usgs_statsdv_fun <- function(data) {dataRetrieval::readNWISstat(data$sites, parameterCd = "00060", statType = 'all') %>%
      mutate(Station = readNWISsite(data$sites) %>% select(station_nm) %>% as.character())}

  #run over api, pulling necessary data.

  if(isTRUE(parallel)){

    usgs_statsdv <- site_station_days %>%
      split(.$sites) %>%
      furrr::future_map(safely(~usgs_statsdv_fun(.))) %>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  } else {

    usgs_statsdv <- site_station_days %>%
      split(.$sites) %>%
      purrr::map(safely(~usgs_statsdv_fun(.)))%>%
      purrr::keep(~length(.) != 0) %>%
      purrr::map(~.x[['result']]) %>%
      plyr::rbind.fill()

  }

#join it with original

  u_hour <- hourlyUSGS(usgs_statsdv, days = days, ...)
  u_hour <- u_hour %>%
    mutate(Date = lubridate::as_date(date),
           year = year(Date),
           month = month(Date),
           day = day(Date),
           month_day = str_c(month, day, sep = "-")) %>%
    group_by(Station, month_day, Date) %>%
    summarise(current_daily_mean_flow = mean(value, na.rm = TRUE))

  usgs_statsdv <- usgs_statsdv %>%
    mutate(month_day = str_c(month_nu, day_nu, sep = "-"))

  t <- vector()
  for(i in 0:days){
    time <- lubridate::date(Sys.time()) - i
    time <- time %>% paste(month(.), mday(.), sep = "-") %>% str_remove("...........")
    t <- append(t, time)
  }
  usgs_statsdv <- usgs_statsdv %>% filter(month_day %in% t)

  u_hour <- u_hour %>% filter(month_day %in% t)

  usgs_statsdv <- usgs_statsdv %>% left_join(u_hour, by = c("Station", "month_day"))

}


#' USGS Report Monthly
#' @description Uses \link[dataRetrieval]{readNWISstat} to gather monthly mean flows. Furthermore,
#' monthly quantiles are generated similar to \link[wildlandhydRo]{reportUSGSdv}.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} USGS NWIS site.
#' @importFrom dataRetrieval readNWISstat
#' @importFrom dplyr summarize arrange desc
#' @return
#' @export
#'
reportUSGSmv <- function(procDV, sites = NULL) {

if(missing(procDV) & is.null(sites))stop("Need at least one argument")

  if(is.null(sites)) {sites <- unique(procDV[['site_no']])}


    usgs_statsmv <- data.frame()
    for(i in 1:length(sites)){

      usgs_st_m <- dataRetrieval::readNWISstat(sites[[i]], parameterCd = "00060",
                                               statType = 'mean',
                                               statReportType = 'monthly') %>%
        mutate(Station = readNWISsite(sites[[i]]) %>% select(station_nm) %>% as.character())

      usgs_statsmv <- plyr::rbind.fill(usgs_st_m,usgs_statsmv)
    }
    summary_stats <- usgs_statsmv %>%
      group_by(Station, month = month_nu) %>%
      summarize(p95_va = quantile(mean_va, probs = .95, na.rm = TRUE),
                p90_va = quantile(mean_va, probs = .90, na.rm = TRUE),
                p80_va = quantile(mean_va, probs = .80, na.rm = TRUE),
                p75_va = quantile(mean_va, probs = .75, na.rm = TRUE),
                p50_va = quantile(mean_va, probs = .50, na.rm = TRUE),
                p25_va = quantile(mean_va, probs = .25, na.rm = TRUE),
                p20_va = quantile(mean_va, probs = .20, na.rm = TRUE),
                p10_va = quantile(mean_va, probs = 0.1, na.rm = TRUE),
                p05_va = quantile(mean_va, probs = 0.05, na.rm = TRUE))

if(nrow(filter(usgs_statsmv, year_nu %in% stringr::str_extract(Sys.time(), "^.{4}"))) == 0){

  usgs_statsmv <- usgs_statsmv %>% arrange(desc(year_nu)) %>%
      filter(year_nu %in% as.character(as.numeric(stringr::str_extract(Sys.time(), "^.{4}")) - 1)) %>%
      rename(month = "month_nu", current_mean_monthly_flow = "mean_va") %>%
      left_join(summary_stats, by = c("month", "Station"))

} else {

  usgs_statsmv <- usgs_statsmv %>% arrange(desc(year_nu)) %>%
    filter(year_nu %in% stringr::str_extract(Sys.time(), "^.{4}") |
           year_nu %in% as.character(as.numeric(stringr::str_extract(Sys.time(), "^.{4}")) - 1)) %>%
    rename(month = "month_nu", current_mean_monthly_flow = "mean_va") %>%
    left_join(summary_stats, by = c("month", "Station"))
}

}


#' Plot USGS Report
#' @description This plots the results of the daily and monthly report from a \link[wildlandhydRo]{reportUSGSdv} or \link[wildlandhydRo]{reportUSGSmv} object.
#' This function will take more than one station but be cautious as it gets too muddled fast, i.e.
#' two is fine but three may be too much.
#'
#' @param report A previously created \link[wildlandhydRo]{reportUSGSdv} or \link[wildlandhydRo]{reportUSGSmv} object.
#' @param time A \code{character} vector indicating "daily" or "month".
#' @param smooth.span A \code{numeric} value indicating the smoothing parameter for the \link[stats]{loess} function. If NULL (default) no smoothing will happen.
#' @importFrom dplyr all_of
#' @importFrom stringr str_replace_all
#' @return A ggplot object
#' @export

plot_reportUSGS <- function(report, time = "daily", smooth.span = NULL) {

  if(missing(report))stop("Need a report dataframe.")


  if(time == "daily") {

    sty.rep <- function(data, breaks, title) {

      data +
        annotation_logticks(sides="l") +
        theme_bw() +
        labs(title= title,
             y = paste("Discharge")) +
        scale_fill_manual(name="Percentiles",breaks = breaks, labels = c("25<sup>th</sup> - 75<sup>th</sup>",
                                                                         "10<sup>th</sup> - 25<sup>th</sup>",
                                                                         "5<sup>th</sup> - 10<sup>th</sup>",
                                                                         "0 - 5<sup>th</sup>"),
                          values = rev(c("#FF0000","#FFA500","#FFFF00","#006400")))+
        guides(fill = guide_legend(override.aes = list(alpha = .15))) +
        scale_color_manual(name = "", values = "black") +
        theme(legend.position="bottom",
              legend.text = ggtext::element_markdown(),
              axis.ticks.x=element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }

    percentiles <- report %>%
      dplyr::rename_with(.cols = contains('_va'),.fn = ~str_replace_all(., "_va", "")) %>%
      mutate(month_day = fct_reorder(month_day, Date, .desc = T),
             day.of.year = as.numeric(strftime(Date,
                                               format = "%j")))
if(!is.null(smooth.span)){
    percentiles <- percentiles %>%
      group_by(Station) %>%
      nest() %>%
      mutate(smooth = map(data, ~smooth_func(.,smooth.span))) %>%
      unnest('smooth') %>% ungroup()
}
    title.text <- paste0(unique(percentiles$Station), "\n",
                         "Date of plot = ",min(percentiles$Date),' to ', max(percentiles$Date))

    label.text <- c("Normal","Drought Watch","Drought Warning",'Drought Emergency')

    simple.plot <- ggplot(data = percentiles, aes(x = Date)) +
      geom_ribbon(aes(ymin = p25, ymax = p75, fill = "Normal"), alpha = 0.5) +
      geom_ribbon(aes(ymin = p10, ymax = p25, fill = "Drought Watch"), alpha = 0.5) +
      geom_ribbon(aes(ymin = p05, ymax = p10, fill = "Drought Warning"), alpha = 0.5) +
      geom_ribbon(aes(ymin = min, ymax = p05, fill = 'Drought Emergency')) +
      scale_y_log10(labels = scales::comma) +
      geom_line( aes(x=Date, y=current_daily_mean_flow, color = 'Daily Flow'),size = 0.75)

    styled.plot <- sty.rep(simple.plot, breaks = label.text, title = title.text)



     if(length(unique(percentiles$Station))>1){
         styled.plot +
         facet_wrap(~Station, scales = "free") +
         labs(title = '')
      } else {

         styled.plot
      }

       } else if (time == "month") {


    per <- c("p05_va","p10_va","p20_va","p25_va","p50_va","p75_va","p80_va","p90_va","p95_va")
    percentiles <- report %>%
      pivot_longer(cols = all_of(per), names_to = "Percentile") %>%
      mutate(Percentile = str_replace_all(Percentile, "_va|p", ""))

    if(length(unique(percentiles$Station))>1){
      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", aes(color = Station)) +
        geom_hline(aes(yintercept = current_mean_monthly_flow, color = Station), linetype = 3, size = 1) +
        theme_bw() +
        labs(color = "Current Year Reading", title = "Monthly Stats: POR Percentiles and Current Year Stat.",
             y = "Monthly Flow Stats") +
        facet_wrap(~month, scales = "free")
    } else {

      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", color = "black") +
        geom_hline(aes(yintercept = current_mean_monthly_flow, color = Station), linetype = 3, size = 1) +
        theme_bw() +
        labs(color = "Current Year Reading", title = "Monthly Stats: POR Percentiles and Current Year Stat.",
             y = "Monthly Flow Stats") +
        facet_wrap(~month, scales = "free")
    }


    }

}


#' USGS Drought Plot
#' @description This function is hijacked from a blog post by Laura DeCicco at \url{https://waterdata.usgs.gov/blog/moving-averages/}.
#' I changed it a little to include compatibility with \link[wildlandhydRo]{batch_USGSdv} and also the ability to
#' change the rolling average, e.g. \code{rolln}. For more details visit the blog. Also, this function is expensive so it
#' will take about 5 to 15 seconds to produce plot depending on period of record.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.\code{optional}
#' @param site A USGS NWIS site. \code{optional}
#' @param rolln A \code{numeric} number of days in moving average
#' @param startDate A \code{character} indicating the start date.
#' @param endDate A \code{character} indicating the end date.
#' @param smooth.span A \code{numeric} value indicating the smoothing parameter for the \link[stats]{loess} function. If NULL (default) no smoothing will happen.
#' @return A ggplot. Works with plotly::ggplotly.
#' @export
#'
#'
#'
plot_USGSdrought <- function(procDV, site = NULL, rolln = 30, startDate = '2010-01-01', endDate = '2015-01-01', smooth.span = NULL) {
if(is.null(rolln)) stop("Need 'rolln' for moving average")
  if(rolln %in% c(0, Inf, -Inf))stop({"rolln needs to be positive."})
if(is.null(site) & missing(procDV)) stop("Need at least one argument")

  if(is.null(site)){

  dr_plot <- laura_DeCicco_fun(procDV = {{ procDV }}, rolln = {{ rolln }}, startDate = {{ startDate }}, endDate = {{ endDate }}, smooth.span = {{smooth.span}})

  } else if (missing(procDV)){

    dr_plot <- laura_DeCicco_fun(site = {{ site }}, rolln = {{ rolln }}, startDate = {{ startDate }}, endDate = {{ endDate }}, smooth.span = {{smooth.span}})
  }

  dr_plot$plot
}




#' USGS Flow Duration Curve
#' @description This function generates Flow Duration Curves.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.\code{optional}
#' @param site A USGS NWIS site. \code{optional}
#' @param startDate A \code{character} indicating the start date. minimum (default).
#' @param endDate A \code{character} indicating the end date. maximum (default)
#' @param span_season A \code{character} vector indicating a season, e.g. c('4-1', '6-30'). If NULL (default), yearly FDC will be produced.
#'
#' @return A ggplot. Works with plotly::ggplotly.
#' @export
#'
#'
#'
plot_USGSfdc <- function(procDV, site = NULL, startDate = '', endDate = '',span_season = NULL) {

  if(is.null(site) & missing(procDV)) stop("Need at least one argument")

  if(is.null(site)){

if(startDate == '' & endDate != ''){

  fdc_first <- procDV %>% dplyr::filter(Date >= min(Date), Date <= {{endDate}})
} else if (endDate == '' & startDate != ''){

  fdc_first <- procDV %>% dplyr::filter(Date >= {{startDate}}, Date <= max(Date))

} else if (startDate == '' | endDate == '') {

  fdc_first <- procDV %>% dplyr::filter(Date >= min(Date), Date <= max(Date))

} else {

   fdc_first <- procDV %>% dplyr::filter(Date >= {{startDate}}, Date <= {{endDate}} )

   }

  } else if (missing(procDV)){

    fdc_first <- wildlandhydRo::batch_USGSdv(sites = {{ site }}, start_date = {{ startDate }}, end_date = {{ endDate }})

  }

  if(!is.null(span_season)) {

    span1 <- as.Date(span_season[1], '%m-%d')
    span2 <- as.Date(span_season[2], '%m-%d')

    fdc <- fdc_first %>% dplyr::mutate(m_d = as.Date(month_day, format = '%m-%d'))

    if(span1 < span2){

      f1 <- fdc %>%
        dplyr::filter(m_d <= span1) %>%
        dplyr::mutate(season = paste(stringr::str_sub(span2, 6), ' to ', stringr::str_sub(span1, 6)))

      f2 <- fdc %>%
        dplyr::filter(m_d >= span2) %>%
        dplyr::mutate(season = paste(stringr::str_sub(span2, 6), ' to ', stringr::str_sub(span1, 6)))

    } else {

      f1 <- fdc %>%
        dplyr::filter(m_d >= span1) %>%
        dplyr::mutate(season = paste(stringr::str_sub(span1, 6), ' to ', stringr::str_sub(span2, 6)))

      f2 <- fdc %>%
        dplyr::filter(m_d <= span2) %>%
        dplyr::mutate(season = paste(stringr::str_sub(span1, 6), ' to ', stringr::str_sub(span2, 6)))

    }

    fdc_s1 <- rbind.data.frame(f1,f2)

    fdc_1 <- quantile(fdc_s1$Flow, probs = seq(0,1,.01)) %>% data.frame(flow = .) %>%
      dplyr::mutate(pct = rownames(.),
                    pct = readr::parse_number(pct),
                    season = paste(fdc_s1[1,]$season))

    if(span1 < span2){
      fdc_s2 <- fdc %>% dplyr::filter(!month_day %in% fdc_s1$month_day) %>%
        dplyr::mutate(season = paste(stringr::str_sub(span1, 6), ' to ', stringr::str_sub(span2, 6)))
    } else {
      fdc_s2 <- fdc %>% dplyr::filter(!month_day %in% fdc_s1$month_day) %>%
        dplyr::mutate(season = paste(stringr::str_sub(span2, 6), ' to ', stringr::str_sub(span1, 6)))
    }
    fdc_2 <- quantile(fdc_s2$Flow, probs = seq(0,1,.01)) %>% data.frame(flow = .) %>%
      dplyr::mutate(pct = rownames(.),
                    pct = readr::parse_number(pct),
                    season = paste(fdc_s2[1,]$season))


    fdc_final <- rbind.data.frame(fdc_1, fdc_2)


    p1 <- fdc_final %>% dplyr::mutate(`Percentile` = rev(pct)) %>%
      ggplot(aes(`Percentile`, flow, color = season)) + geom_line() +
      scale_y_log10()

  } else {

    fdc <- quantile(fdc_first$Flow, probs = seq(0,1,.01)) %>% data.frame(flow = .) %>%
      dplyr::mutate(pct = rownames(.),
                    pct = parse_number(pct),
                    `Percentile` = rev(pct))

    p1 <- fdc %>%
      ggplot(aes(`Percentile`, flow)) + geom_line() +
      scale_y_log10()
  }


  title.text <- paste("FDC Plot from ", min(fdc_first$Date), ' to ', max(fdc_first$Date))

  styled.plot <- p1 +
    scale_y_log10(labels = scales::comma) +
    annotation_logticks(sides=c('l')) +
    theme_light() +
    labs(title = title.text, y = 'Discharge (cfs)', color = "Season")+
    scale_x_continuous(breaks = seq(0,100,10))

  styled.plot
}
