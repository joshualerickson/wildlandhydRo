#' Change colnames snotel
#'
#' @param df A data.frame
#' @description hijacked 'snotelr' package functions and manipulated
#' @return a function for changing colnames
#'
#'
snotel_wild_custom <- function(df) {

  # check if it's a dataframe
  df_check <- is.data.frame(df)

  if (!df_check) {
    stop("Not a valid (SNOTEL) data frame...")
  }

  # check the file, if less than 8 columns
  # are present this is not a standard file,
  # stop processing
  if (ncol(df) != 8) {
    stop("not a standard snotel file")
  }

  # define new column names
  snotel_columns <- c(
    "date",
    "snow_water_equivalent",
    "precipitation_cumulative",
    "temperature_max",
    "temperature_min",
    "temperature_mean",
    "precipitation",
    "snow_depth"
  )

  # rename columns
  colnames(df) <- snotel_columns


  # return data frame
  return(df)
}

#' Change colnames snotel report
#'
#' @param df A data frame
#' @description hijacked 'snotelr' package functions and manipulated
#' @return a function for changing colnames for report
#'
#'
snotel_report_custom <- function(df) {

  # check if it's a dataframe
  df_check <- is.data.frame(df)

  if (!df_check) {
    stop("Not a valid (SNOTEL) data frame...")
  }

  # check the file, if less than 6 columns
  # are present this is not a standard file,
  # stop processing
  if (ncol(df) != 6) {
    stop("not a standard snotel file")
  }

  # define new column names
  snotel_columns <- c(
    "date",
    "swe_percent_median",
    "swe_current",
    "swe_prev_year",
    "snow_current",
    "snow_prev_year"


  )

  # rename columns
  colnames(df) <- snotel_columns


  # return data frame
  return(df)
}

#' Process SNOTEL daily values
#'
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#' @description This function gets daily snotel data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. This
#' function is similar to \link[snotelr]{snotel_download} except it includes english units and adds snow depth as a variable.
#'
#' @return A \code{data.frame} with SNOTEL metrics that capture the following variables: \code{snow_water_equivalent},
#' \code{precipitation_cumulative},
#' \code{temperature_max},
#' \code{temperature_min},
#' \code{temperature_mean},
#' \code{precipitation},
#' \code{snow_depth}.
#'
#'
#' @importFrom httr GET write_disk http_error
#' @importFrom plyr rbind.fill
#' @export
#'

batch_SNOTELdv <- function(sites, parallel = FALSE, ...) {


  # download meta-data
  meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  # loop over selection, and download the data

                snotel_daily <- function(md) {
                           # download url (metric by default!)
                           base_url <- paste0(
                             "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/",
                             md$site_id, ":",
                             md$state, ":",
                             md$network,
                             "%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value,SNWD::value"
                           )

                           # try to download the data
                           error <- httr::GET(url = base_url,
                                              httr::write_disk(path = file.path(tempdir(),
                                                                                "snotel_tmp.csv"),
                                                               overwrite = TRUE))

                           # catch error and remove resulting zero byte files
                           if (httr::http_error(error)) {
                             warning(sprintf("Downloading site %s failed, removed empty file.",
                                             md$site_id))
                           }

                           # read in the snotel data
                           df <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                                                   header = TRUE,
                                                   sep = ",",
                                                   stringsAsFactors = FALSE)

                           # subsitute column names
                           df <- snotel_wild_custom(df) %>% mutate(site_id = md$site_id)

                           df <- left_join(md, df, by = 'site_id')
                }

                if(isTRUE(parallel)){

                  snotel_data <- meta_snotel %>%
                    split(.$site_id) %>%
                    furrr::future_map(safely(~snotel_daily(.))) %>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill()

                } else {

                  snotel_data <- meta_snotel %>%
                    split(.$site_id) %>%
                    purrr::map(safely(~snotel_daily(.)))%>%
                    purrr::keep(~length(.) != 0) %>%
                    purrr::map(~.x[['result']]) %>%
                    plyr::rbind.fill()

                }
  snotel_data <- snotel_data %>%
    dplyr::mutate(Date = lubridate::parse_date_time(date, orders = c("%y-%m-%d", "%y%m%d", "%y-%m-%d %H:%M")),
           year = lubridate::year(Date),
           month = lubridate::month(Date),
           month_abb = factor(month.abb[month], levels = month.abb),
           site_name = str_to_title(site_name),
           site_name = factor(site_name))

}

#' Water Year Stats (SNOTEL)
#' @description This function gets snotel data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. It generates
#' the maximum and mean of snow water equivalent and snow depth per water year.
#' @param procDV A previously created \link[wildlandhydRo]{batch_SNOTELdv} object. \code{recommended}
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}. \code{optional}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{data.frame} with \code{mean} and \code{maximum} snow water equivalent and snow depth.
#' @export

wySNOTEL <- function(procDV, sites = NULL,  parallel = FALSE, ...) {

  #error catching
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(!missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

snotel_yearly <- function(md){
      # download url (metric by default!)
      base_url <- paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/annual_water_year/start_of_period/",
        md$site_id, ":",
        md$state, ":",
        md$network,
        '%7Cid=""%7Cname/POR_BEGIN,POR_END/WTEQ::value:daily%20MEAN,WTEQ::value:daily%20MAX,SNWD::value:daily%20MEAN,SNWD::value:daily%20MAX'
      )

      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "snotel_tmp.csv"),
                                          overwrite = TRUE))

      # read in the snotel data
      df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),comment = "#", col_types = readr::cols())

      # subsitute column names
      # define new column names
      snotel_columns <- c(
        "wy",
        "swe_mean",
        "swe_max",
        "snow_mean",
        "snow_max"
      )

      # rename columns
      colnames(df) <- snotel_columns

      df <- df %>% dplyr::mutate(site_id = md$site_id)


      df <- dplyr::left_join(md, df, by = 'site_id')


}
if(isTRUE(parallel)){

  snotel_download_wy <- meta_snotel %>%
    split(.$site_id) %>%
    furrr::future_map(safely(~snotel_yearly(.))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    plyr::rbind.fill()

} else {

  snotel_download_wy <- meta_snotel %>%
    split(.$site_id) %>%
    purrr::map(safely(~snotel_yearly(.)))%>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    plyr::rbind.fill()

}


}

#' Water Year & Monthly Stats (SNOTEL)
#' @description This function gets snotel data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. It generates
#' the maximum and mean of snow water equivalent and snow depth per water year per month.
#' @param procDV A previously created \link[wildlandhydRo]{batch_SNOTELdv} object. \code{recommended}
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}. \code{optional}
#' @param parallel \code{logical} indicating whether to use future_map().
#' @param ... arguments to pass on to \link[furrr]{future_map}.
#'
#' @return A \code{data.frame} with \code{mean} and \code{maximum} snow water equivalent and snow depth.
#' @export
#' @importFrom stringr str_remove str_extract str_c
#' @importFrom readr parse_number read_csv cols
#' @importFrom httr GET http_error write_disk
#' @importFrom lubridate as_date
#'

wymSNOTEL <- function(procDV, sites = NULL, parallel = FALSE, ...) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}
  if(!missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  }

      snotel_wym <- function(md){
      # download url (metric by default!)
      base_url <- paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/monthly/start_of_period/",
        md$site_id, ":",
        md$state, ":",
        md$network,
        '%7Cid=""%7Cname/POR_BEGIN,POR_END/WTEQ::value:daily%20MEAN,WTEQ::value:daily%20MAX,SNWD::value:daily%20MEAN,SNWD::value:daily%20MAX'
      )

      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "snotel_tmp.csv"),
                                          overwrite = TRUE))

      # read in the snotel data
      df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),comment = "#", col_types = readr::cols())

      # subsitute column names
      # define new column names
      snotel_columns <- c(
        "date",
        "swe_mean",
        "swe_max",
        "snow_mean",
        "snow_max"
      )

      # rename columns
      colnames(df) <- snotel_columns

      df <- df %>% dplyr::mutate(site_id = md$site_id)

      df <- left_join(md, df, by = 'site_id')


      }

      if(isTRUE(parallel)){

        snotel_download_wym <- meta_snotel %>%
          split(.$site_id) %>%
          furrr::future_map(safely(~snotel_wym(.))) %>%
          purrr::keep(~length(.) != 0) %>%
          purrr::map(~.x[['result']]) %>%
          plyr::rbind.fill()

      } else {

        snotel_download_wym <- meta_snotel %>%
          split(.$site_id) %>%
          purrr::map(safely(~snotel_wym(.)))%>%
          purrr::keep(~length(.) != 0) %>%
          purrr::map(~.x[['result']]) %>%
          plyr::rbind.fill()

      }

    snotel_download_wym <- snotel_download_wym   %>%
      mutate(year = str_extract(date, "(\\d+)"),
             year = str_remove(year, " "),
             month = str_extract(date, "(\\D+)"),
             month = str_remove(month, " "),
             day = 1,
             Date = str_c(year,month,day, sep = "-"),
             Date = as_date(Date),
             wy = waterYear(Date,numeric = TRUE))


}

#' Month-Only Stats (SNOTEL)
#' @description This function uses a \link[wildlandhydRo]{batch_SNOTELdv} object to generate
#' month only statistics for snow water equivalent and snow depth.
#' @param procDV A previously created \link[wildlandhydRo]{batch_SNOTELdv} object
#'
#' @return A data.frame
#' @export
#' @importFrom dplyr group_by summarise mutate relocate
#'
#'

monthSNOTEL <- function(procDV) {

  snotel_raw_min_max_month <-
    procDV  %>%
    group_by(site_name, month_abb) %>%
    summarise(Maximum_swe = round(max(snow_water_equivalent, na.rm = TRUE),2),
              Minimum_swe = round(min(snow_water_equivalent, na.rm = TRUE),2),
              Mean_swe = round(mean(snow_water_equivalent, na.rm = TRUE),2),
              Median_swe = round(median(snow_water_equivalent, na.rm = TRUE),2),
              Maximum_snow = round(max(snow_depth, na.rm = TRUE),2),
              Minimum_snow = round(min(snow_depth, na.rm = TRUE),2),
              Mean_snow = round(mean(snow_depth, na.rm = TRUE),2),
              Median_snow = round(median(snow_depth, na.rm = TRUE),2)) %>%
    ungroup()

  return(snotel_raw_min_max_month)
}

#' Hourly SNOTEL
#' @description This function gets hourly SNOTEL data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website.

#' @param procDV A previously created \link[wildlandhydRo]{batch_SNOTELdv} object
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param days A \code{numeric} input of days, e.g. 1 = 24 hrs.
#' @importFrom httr GET write_disk http_error
#' @importFrom plyr rbind.fill
#' @importFrom stringr str_to_title
#' @importFrom lubridate as_date ymd_hm
#' @return A data.frame with hourly SNOTEL data
#' @export

hourlySNOTEL <- function(procDV, sites = NULL,  days = 7, parallel = FALSE, ...) {

  if(length(days) > 1){stop("only length 1 vector")}
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}
 if(!missing(procDV)) {

   meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


    }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  choice_days <- days
snotel_hourly <- function(md, choice_days){
      # download url (metric by default!)
      base_url <- paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/hourly/start_of_period/",
        md$site_id, ":",
        md$state, ":",
        md$network,
        "%7Cid=\"\"%7Cname/-",round(choice_days*24,0),",0/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value,SNWD::value"
      )

      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "snotel_tmp.csv"),
                                          overwrite = TRUE))

      # read in the snotel data
      df <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                              header = TRUE,
                              sep = ",",
                              stringsAsFactors = FALSE)

      # subsitute column names
      df <- snotel_wild_custom(df) %>% dplyr::mutate(site_id = md$site_id)

      df <- dplyr::left_join(md,df, by = 'site_id')
    }

if(isTRUE(parallel)){

  snotel_hourly_download <- meta_snotel %>%
    split(.$site_id) %>%
    furrr::future_map(safely(~snotel_hourly(., choice_days = choice_days))) %>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    plyr::rbind.fill()

} else {

  snotel_hourly_download <- meta_snotel %>%
    split(.$site_id) %>%
    purrr::map(safely(~snotel_hourly(., choice_days = choice_days)))%>%
    purrr::keep(~length(.) != 0) %>%
    purrr::map(~.x[['result']]) %>%
    plyr::rbind.fill()

}
}


#' Snotel Report Daily
#' @description This function gets daily SNOTEL report data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. This includes
#' percentage of median from 1981-2010. Also included is current years data and the previous years data as well.
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param procDV A previously created \link[wildlandhydRo]{batch_SNOTELdv} object
#' @param days A \code{numeric} input for days
#'
#' @return A \code{data.frame}
#' @export
#' @importFrom readr read_csv


reportSNOTELdv <- function(procDV, sites = NULL, days = 8) {

  if(length(days) > 1){stop("only length 1 vector")}
  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  choice_days <- days

  #create blank dataframe to store the information in
  snotel_report <- data.frame()

  #run for loop over api, pulling necessary data.

  for (i in 1:nrow(meta_snotel)){
    # loop over selection, and download the data

    tryCatch({

      # some feedback on the download progress
      message(sprintf("Downloading site: %s, with id: %s\n",
                      meta_snotel$site_id[i],
                      meta_snotel$site_name[i]))

      # download url (metric by default!)
      base_url <- paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/",
        meta_snotel$site_id[i], ":",
        meta_snotel$state[i], ":",
        meta_snotel$network[i],
        "%7Cid=%22%22%7Cname/-",choice_days,",0/WTEQ::pctOfMedian_1981,WTEQ::value,WTEQ::prevValue,SNWD::value,SNWD::prevValue"
      )

      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "snotel_tmp.csv"),
                                          overwrite = TRUE))

      # catch error and remove resulting zero byte files
      if (httr::http_error(error)) {
        warning(sprintf("Downloading site %s failed, removed empty file.",
                        meta_snotel$site_id[i]))
      }

      # read in the snotel data
      df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),comment = "#", col_types = cols())

      # substitute column names
      df <- snotel_report_custom(df)

      df <- cbind.data.frame(meta_snotel[i,c(3,4,11)], df, row.names = NULL)

      #combine df with blank dataframe (usgs_download_hourly)

      snotel_report <- plyr::rbind.fill(snotel_report, df)


    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

  return(snotel_report)

}

#' Snotel Report Monthly
#' @description This function gets monthly SNOTEL report data from \url{https://wcc.sc.egov.usda.gov/reportGenerator/} website. This includes
#' percentage of median from 1981-2010. Also included is current years data and the previous years data as well.
#' @param sites A vector of SNOTEL site locations, e.g. \code{c("311", "500")}
#' @param procDV A previously created \link[wildlandhydRo]{batch_SNOTELdv} object
#'
#' @return A \code{data.frame}
#' @export
#' @importFrom readr read_csv


reportSNOTELmv <- function(procDV, sites = NULL) {

  if(!is.null(sites) & !missing(procDV)){stop("Can't use both Sites and procDV")}
  if(is.null(sites) & missing(procDV)){stop("Need at least one argument!")}

  if(missing(procDV)) {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% sites),]


  } else {

    meta_snotel <- meta_snotel[which(meta_snotel$site_id %in% procDV$site_id),]

  }

  # check if the provided site index is valid
  if (nrow(meta_snotel) == 0){
    stop("no site found with the requested ID")
  }

  #create blank dataframe to store the information in
  snotel_report_month <- data.frame()

  #run for loop over api, pulling necessary data.

  for (i in 1:nrow(meta_snotel)){
    # loop over selection, and download the data

    tryCatch({

      # some feedback on the download progress
      message(sprintf("Downloading site: %s, with id: %s\n",
                      meta_snotel$site_id[i],
                      meta_snotel$site_name[i]))
      # download url (metric by default!)
      base_url <- paste0(
        "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/monthly/start_of_period/",
        meta_snotel$site_id[i], ":",
        meta_snotel$state[i], ":",
        meta_snotel$network[i],
        "%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::pctOfMedian_1981,WTEQ::value,WTEQ::prevValue,SNWD::value,SNWD::prevValue"
      )

      # try to download the data
      error <- httr::GET(url = base_url,
                         httr::write_disk(path = file.path(tempdir(),
                                                           "snotel_tmp.csv"),
                                          overwrite = TRUE))

      # catch error and remove resulting zero byte files
      if (httr::http_error(error)) {
        warning(sprintf("Downloading site %s failed, removed empty file.",
                        meta_snotel$site_id[i]))
      }

      # read in the snotel data
      df <- readr::read_csv(file.path(tempdir(),"snotel_tmp.csv"),skip = 62, col_types = cols())

      # subsitute column names
      df <- snotel_report_custom(df)

      df <- cbind.data.frame(meta_snotel[i,c(3,4,11)], df, row.names = NULL)

      #combine df with blank dataframe (usgs_download_hourly)

      snotel_report_month <- plyr::rbind.fill(snotel_report_month, df)


    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

  snotel_report_month <- snotel_report_month  %>%
    mutate(year = str_extract(date, "(\\d+)"),
           year = str_remove(year, " "),
           month = str_extract(date, "(\\D+)"),
           month = str_remove(month, " "),
           day = 1,
           Date = str_c(year,month,day, sep = "-"),
           Date = as_date(Date),
           wy = waterYear(Date,numeric = TRUE))

  return(snotel_report_month)

}
#' Plot SNOTEL Report
#'
#' @param report A previously created \link[wildlandhydRo]{batch_SNOTELdv} object
#' @param time A string ("month", "daily") indicating what time frame to plot.
#' @param type A string ("per", "now", "all") indicating what type of plot.
#' @return
#' @importFrom stringr str_replace_all
#' @export
#'

plot_reportSNOTEL <- function(report, time = "daily", type = "per") {

  if (time == "daily") {
  if(is.null(type))stop({"Need a type argument."})

  if(type == "per"){

    report %>%
      mutate(site_name = fct_reorder(site_name, swe_percent_median)) %>%
      ggplot(aes(site_name, swe_percent_median, color = site_name)) +
      geom_col(show.legend = FALSE, fill = "white") +
      geom_point(size = 3) +
      geom_hline(yintercept = 100, linetype = 2, size = 1.3) +
      coord_flip() +
      theme_bw() + theme(strip.text = element_text(size = 8.5)) +
      labs(x = "Percent of Median",color = "Site Name",
           title = "Daily Snow Water Equivalent (SWE) Percent of Median") +
      facet_wrap(~date, scales = "free")

  } else if (type == "now") {

    report %>%
      mutate(site_name = fct_reorder(site_name, swe_percent_median)) %>%
      pivot_longer(cols = c(swe_current, swe_prev_year, snow_current, snow_prev_year), names_to = "stat") %>%
      mutate(stat = str_replace_all(stat, "_", " ")) %>%
      ggplot() + geom_col(aes(stat, value, fill = site_name), position = "dodge") +
      coord_flip() +
      theme_bw() + theme(strip.text = element_text(size = 8.5)) +
      labs(x = "Statistic Value", title = "Summary Statistics per Site") +
      facet_wrap(~date, scales = "free")

  } else if (type == "all") {

    g1 <-  report %>%
      mutate(site_name = fct_reorder(site_name, swe_percent_median)) %>%
      ggplot(aes(site_name, swe_percent_median, color = site_name)) +
    geom_col(show.legend = FALSE, fill = "white") +
      geom_point(size = 3) +
    geom_hline(yintercept = 100, linetype = 2) +
    coord_flip() +
    theme_bw() + theme(strip.text = element_text(size = 8.5)) +
    labs(x = "Statistic Value", color = "Site Name",
         title = "Summary Statistics per Site") +
    facet_wrap(~date, scales = "free")

 g2 <- report %>%
   mutate(site_name = fct_reorder(site_name, swe_percent_median)) %>%
   pivot_longer(cols = c(swe_current, swe_prev_year, snow_current, snow_prev_year), names_to = "stat") %>%
   mutate(stat = str_replace_all(stat, "_", " ")) %>%
   ggplot() + geom_col(aes(stat, value, fill = site_name), position = "dodge") +
   coord_flip() +
   theme_bw() + theme(strip.text = element_text(size = 8.5)) +
   labs(x = "Statistic Value", title = "", fill = "Site Name") +
   facet_wrap(~date, scales = "free")

 ggpubr::ggarrange(g1, g2, common.legend = TRUE)

  }

  } else if (time == "month") {

    if(type == "per"){
      year_low <- max(report$wy)-8
      report %>% mutate(month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                                                "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
        filter(wy %in% year_low:max(report$wy)) %>%
        ggplot(aes(month, swe_percent_median)) +
        geom_col(aes(fill = site_name), position = "dodge")+
        geom_hline(yintercept = 100) +
        theme_bw() + theme(strip.text = element_text(size = 8.5)) +
        labs(x = "Month", y = "SWE Percent of Median", title = "Start of Month SWE Percent of Median", fill = "Site Name") +
        facet_wrap(~wy)

    } else if (type == "now") {

      report %>%
        mutate(month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                                                "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
        pivot_longer(cols = c(swe_current, swe_prev_year, snow_current, snow_prev_year), names_to = "stat") %>%
        filter(wy %in% max(report$wy)) %>%
        ggplot(aes(month, value, fill = stat, group = stat)) +
        theme_bw() + theme(strip.text = element_text(size = 8.5)) +
        labs(x = "Month", y = "SWE", title = "Start of Month SWE/Snow Current and Previous Value", fill = "Metric") +
        geom_col(position = 'dodge') + facet_wrap(~site_name, scales = "free")

    } else if (type == "all") {
      year_low <- max(report$wy)-8
      g1 <-  report %>%
        mutate(month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                                                "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
        filter(wy %in% year_low:max(report$wy)) %>%
        ggplot(aes(month, swe_percent_median)) +
        geom_col(aes(fill = site_name), position = "dodge")+
        geom_hline(yintercept = 100) +
        theme_bw() + theme(strip.text = element_text(size = 8.5)) +
        labs(x = "Month", y = "SWE Percent of Median", title = "Start of Month SWE Percent of Median", fill = "Site Name") +
        facet_wrap(~wy)

      g2 <- report %>%
        mutate(month = factor(month, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                                                "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>%
        pivot_longer(cols = c(swe_current, swe_prev_year, snow_current, snow_prev_year), names_to = "stat") %>%
        filter(wy %in% max(report$wy)) %>%
        ggplot(aes(month, value, fill = stat, group = stat)) +
        theme_bw() + theme(strip.text = element_text(size = 8.5)) +
        labs(x = "Month", y = "SWE", title = "Start of Month SWE/Snow Current and Previous Value", fill = "Metric") +
        geom_col(position = 'dodge') + facet_wrap(~site_name, scales = "free")


      ggpubr::ggarrange(g1, g2, legend = 'bottom')

    }

}

}

#' Get SNOTEL locations
#' @description A function that locates the nearest SNOTEL station and returns a sf object. This function only works with
#' a sf object and is really handy when using
#' \link[AOI]{AOI-package} functions. This function is hijacked from \link[nhdplusTools]{get_nwis}.
#' @param AOI A AOI object
#' @return A \code{data.frame}
#' @export

get_SNOTEL = function(AOI){

  AOI_type <- sf::st_geometry_type(AOI)

 meta_sf <- meta_snotel %>% mutate(row.id = row_number()) %>%
    sf::st_as_sf(coords = c("longitude", "latitude")) %>% sf::st_set_crs(4269)

  if(AOI_type == "POINT"){
    pt  <-  AOI
    AOI <-  sf::st_transform(AOI, 5070) %>%
      sf::st_buffer(20000) # meters due to 5070
  } else {

   bb <-  sf::st_transform(AOI, 4269)
  resp <- suppressMessages(as.data.frame(sf::st_intersects(meta_sf, bb)))

  }





  if(nrow(resp) == 0){
    if(AOI_type == "POINT"){
      stop("No SNOTEL sites within 20km of this location", call. = FALSE)
    } else {
      stop("No SNOTEL sites found in this AOI.", call. = FALSE)
    }
  } else {

    sites_sf <- meta_sf %>% filter(row.id %in% resp$row.id)

    if(AOI_type == "POINT"){
      sites_sf <- suppressMessages(
        meta_sf[sf::st_nearest_feature(pt, meta_sf),]
      )
    }

    return(sites_sf)
  }
}
