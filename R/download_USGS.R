

#' Process USGS daily values
#' @description This function is basically a wrapper around \link[dataRetrieval]{readNWISdv} but includes
#' added variables like water year, lat/lon, station name, altitude and tidied dates.
#' @param sites A vector of USGS NWIS sites
#' @param parameterCd A USGS code for metric, default is "00060".
#' @param start_date A character of date format, e.g. \code{"1990-09-01"}
#' @param end_date A character of date format, e.g. \code{"1990-09-01"}
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

batch_USGSdv <- function(sites, parameterCd = "00060", start_date = "", end_date = "") {



  site_id_usgs <- data.frame(sites = sites)


  usgs_raw_dv <- data.frame()

  for (i in 1:nrow(site_id_usgs)) {

  tryCatch({
    usgs_data <-

    message(sprintf("Downloading site: %s, with id: %s\n", # some feedback on the download progress
                    site_id_usgs$sites[i],
                    readNWISsite(site_id_usgs$sites[[i]]) %>% select(station_nm) %>% as.character()))

  discharge <- readNWISdv(siteNumbers = site_id_usgs$sites[[i]], parameterCd = parameterCd,
                          startDate = start_date, endDate = end_date, statCd = "00003") %>%
    renameNWISColumns() %>% #below could be better, maybe attr(discharge,"siteInfo")[["some_name"]]
    mutate(
      drainage_area = readNWISsite(site_id_usgs$sites[[i]]) %>% select(drain_area_va) %>% as.numeric(),
      Station = readNWISsite(site_id_usgs$sites[[i]]) %>% select(station_nm) %>% as.character(),
      lat = readNWISsite(site_id_usgs$sites[[i]]) %>% select(dec_lat_va) %>% as.numeric(),
      long = readNWISsite(site_id_usgs$sites[[i]]) %>% select(dec_long_va) %>% as.numeric(),
      altitude = readNWISsite(site_id_usgs$sites[[i]]) %>% select(alt_va) %>% as.numeric())


  usgs_raw_dv <- plyr::rbind.fill(usgs_raw_dv, discharge)},

  error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

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
#' @importFrom lubridate year month day
#' @importFrom dplyr mutate filter group_by summarise slice_head ungroup everything row_number n
#' @importFrom stringr str_c str_remove_all
#' @importFrom stats median sd
#' @importFrom lfstat baseflow
#'
#' @return
#' @export
#'
#' @examples
wyUSGS <- function(procDV, sites = NULL) {

if(missing(procDV)) {

  usgs_raw <- batch_USGSdv(sites = sites) %>% mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))

} else {

usgs_raw <- procDV %>% mutate(Flow = ifelse(Flow <= 0 , Flow + 0.01, Flow))}


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
            Max_sdnorm = Maximum/sd(log(Flow), na.rm = TRUE),
            Min_sdnorm = Minimum/sd(log(Flow), na.rm = TRUE),
            Mean_sdnorm = Mean/sd(log(Flow), na.rm = TRUE),
            Med_sdnorm = Median/sd(log(Flow), na.rm = TRUE),
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


peaks <- data.frame()
for (i in 1:nrow(peak_sites)) {

  peak <- tryCatch({dataRetrieval::readNWISpeak(peak_sites$peaks[[i]]) %>%
                            select(peak_va, peak_dt, site_no)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  peak <- peak %>%
            mutate(wy = waterYear(peak_dt, TRUE))

  peaks <- plyr::rbind.fill(peaks, peak)

}

usgs_min_max_wy <- usgs_min_max_wy %>% left_join(peaks, by = c("site_no", "wy")) %>%
  select(Station, site_no, wy, Peak = peak_va, peak_dt, dplyr::everything())

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
#' @return A \code{data.frame}
#' @export
#' @importFrom dplyr group_by mutate across summarise rename right_join ungroup n
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate parse_date_time ymd
#' @importFrom stringr str_c
#' @importFrom ape where
#'
#' @examples
wymUSGS <- function(procDV, sites = NULL) {

  if(missing(procDV)) {

    usgs_raw <- batch_USGSdv(sites = sites)

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
#' @return A \code{data.frame}
#' @export
#' @importFrom dplyr group_by summarise mutate relocate
#'
#' @examples
#'
#'

monthUSGS <- function(procDV, sites = NULL) {

  if(missing(procDV)) {

    usgs_raw <- batch_USGSdv(sites = sites)

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
#'
#' @return
#' @export
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom dplyr mutate rename group_by mutate relocate summarise ungroup contains
#' @importFrom dataRetrieval renameNWISColumns readNWISsite
#' @importFrom httr GET write_disk http_error
#' @importFrom plyr rbind.fill
#'
#' @examples
#'
hourlyUSGS <- function(procDV, sites = NULL, days = 7) {

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
      select(site_no, datetime, dplyr::contains("_Flow"))
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
  mutate(date = ymd_hm(datetime),
         value = as.numeric(value1),
           fl_val = ifelse(is.na(value), paste("Error"), "Good"))

  usgs_download_hourly <- usgs_download_hourly %>%
    mutate(date=floor_date(date, "1 hour")) %>%
    group_by(Station,date,fl_val) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup()


}


#' Plot Water Year Base-flow and Total Flow
#'
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} USGS NWIS site.
#' @param wy A \code{numeric} or \code{character} water year, e.g. 1990:2000, 1990, or c("1990", "2000").
#' @importFrom dplyr rename filter
#' @importFrom ggplot2 ggplot geom_line theme_light labs scale_color_manual facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom lfstat baseflow
#' @return
#' @export

plot_baseflow <- function(procDV, sites = NULL, wy) {

  if(missing(wy))stop({"Need wy!"})

  if(missing(procDV)) {

    usgs_raw <- batch_USGSdv(sites = sites)

  } else {

    usgs_raw <- procDV }

  usgs_baseflow <- usgs_raw  %>%
    filter(wy %in% {{ wy }}) %>%
    group_by(Station) %>% mutate(bf = lfstat::baseflow(Flow)) %>%
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
#' statistics like quantiles.
#' @param procDV A previously created \link[wildlandhydRo]{batch_USGSdv} object.
#' @param sites A \code{character} USGS NWIS site.
#' @importFrom dataRetrieval readNWISstat
#' @importFrom lubridate mday
#' @importFrom dplyr tibble
#' @return
#' @export
#'
reportUSGSdv <- function(procDV, sites = NULL) {


  if(missing(procDV)) {

    usgs_statsdv <- data.frame()
    for(i in 1:length(sites)){

      usgs_st <- dataRetrieval::readNWISstat(sites[[i]], parameterCd = "00060", statType = 'all') %>%
      mutate(Station = readNWISsite(sites[[i]]) %>% select(station_nm) %>% as.character())

      usgs_statsdv <- plyr::rbind.fill(usgs_st,usgs_statsdv)
    }

  } else {

          sites_proc <- unique(procDV$site_no)

          usgs_statsdv <- data.frame()
          for(i in 1:length(sites_proc)){
            usgs_st <- dataRetrieval::readNWISstat(sites_proc[[i]], parameterCd = "00060", statType = 'all') %>%
              mutate(Station = readNWISsite(sites_proc[[i]]) %>% select(station_nm) %>% as.character())
            usgs_statsdv <- plyr::rbind.fill(usgs_st,usgs_statsdv)
          }



  }

#join it with original

  u_hour <- hourlyUSGS(usgs_statsdv, days = 10)
  u_hour <- u_hour %>%
    mutate(Date = lubridate::as_date(date),
           year = year(Date),
           month = month(Date),
           day = day(Date),
           month_day = str_c(month, day, sep = "-")) %>%
    group_by(Station, month_day) %>%
    summarise(current_flow = mean(value, na.rm = TRUE))

  usgs_statsdv <- usgs_statsdv %>%
    mutate(month_day = str_c(month_nu, day_nu, sep = "-"))

  t <- vector()
  for(i in 0:9){
    time <- lubridate::date(Sys.time()) - i
    time <- time %>% paste(month(.), mday(.), sep = "-") %>% str_remove("...........")
    t <- append(t, time)
  }
  usgs_statsdv <- usgs_statsdv %>% filter(month_day %in% t)

  u_hour <- u_hour %>% filter(month_day %in% t)

  usgs_statsdv <- usgs_statsdv %>% left_join(u_hour, by = c("Station", "month_day"))

  print(tibble(usgs_statsdv))
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

  if(missing(procDV)) {

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

    usgs_statsmv <- usgs_statsmv %>% arrange(desc(year_nu)) %>%
      filter(year_nu %in% stringr::str_extract(Sys.time(), "^.{4}")) %>%
      rename(month = "month_nu", current_flow = "mean_va") %>%
      left_join(summary_stats, by = c("month", "Station"))

  } else {

    sites_proc_m <- unique(provDV$site_no)

    usgs_statsmv <- data.frame()
    for(i in 1:length(sites_proc_m)){
      usgs_st_m <- dataRetrieval::readNWISstat(sites_proc_m[[i]],
                                              parameterCd = "00060",
                                              statType = 'mean',
                                              statReportType = 'monthly') %>%
        mutate(Station = readNWISsite(sites_proc_m[[i]]) %>% select(station_nm) %>% as.character())
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

    usgs_statsmv <- usgs_statsmv %>% arrange(desc(year_nu)) %>%
      filter(year_nu %in% stringr::str_extract(Sys.time(), "^.{4}")) %>%
      rename(month = "month_nu", current_flow = "mean_va") %>%
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
#' @importFrom dplyr all_of
#' @importFrom stringr str_replace_all
#' @return A ggplot object
#' @export
#'
#' @examples
plot_reportUSGS <- function(report, time = "daily") {

  if(missing(report))stop("Need a report dataframe.")


  if(time == "daily") {

      per <- c("p05_va","p10_va","p20_va","p25_va","p50_va","p75_va","p80_va","p90_va","p95_va")
      percentiles <- report %>%
        pivot_longer(cols = all_of(per), names_to = "Percentile") %>%
        mutate(Percentile = str_replace_all(Percentile, "_va|p", ""))

if(length(unique(percentiles$Station))>1){
      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", aes(color = Station)) +
        geom_hline(aes(yintercept = current_flow, color = Station), linetype = 3, size = 1) +
        theme_bw() +
        labs(color = "Current Reading", title = "Daily Stats: POR Percentiles and Current Flow.") +
        facet_wrap(~month_day, scales = "free")
} else {

  ggplot(percentiles, aes(Percentile, value))+
    geom_col(fill = "white", color = "black") +
    geom_hline(aes(yintercept = current_flow, color = Station), linetype = 3, size = 1) +
    theme_bw() +
    labs(color = "Current Reading", title = "Daily Stats: POR Percentiles and Current Flow.") +
    facet_wrap(~month_day, scales = "free")
}

  } else if (time == "month") {


    per <- c("p05_va","p10_va","p20_va","p25_va","p50_va","p75_va","p80_va","p90_va","p95_va")
    percentiles <- report %>%
      pivot_longer(cols = all_of(per), names_to = "Percentile") %>%
      mutate(Percentile = str_replace_all(Percentile, "_va|p", ""))

    if(length(unique(percentiles$Station))>1){
      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", aes(color = Station)) +
        geom_hline(aes(yintercept = current_flow, color = Station), linetype = 3, size = 1) +
        theme_bw() +
        labs(color = "Current Year Reading", title = "Monthly Stats: POR Percentiles and Current Year Stat.",
             y = "Monthly Flow Stats") +
        facet_wrap(~month, scales = "free")
    } else {

      ggplot(percentiles, aes(Percentile, value))+
        geom_col(fill = "white", color = "black") +
        geom_hline(aes(yintercept = current_flow, color = Station), linetype = 3, size = 1) +
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
#'
#' @return A ggplot. Doesn't work with plotly::ggplotly.
#' @export
#'
#'
#'
plot_USGSdrought <- function(procDV, site = NULL, rolln = 30) {
if(is.null(rolln)) stop("Need 'rolln' for moving average")
  if(rolln %in% c(0, Inf, -Inf))stop({"rolln needs to be positive."})
if(is.null(site) & missing(procDV)) stop("Need at least one argument")

  if(is.null(site)){

  laura_DeCicco_fun(procDV = {{ procDV }}, rolln = {{ rolln }})

  } else if (missing(procDV)){

    laura_DeCicco_fun(site = {{ site }}, rolln = {{ rolln }})
  }
}
