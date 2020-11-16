#' snotel custom functions
#'
#' Takes any data.frame
#' @param data.frame A data.frame
#' @return The daily precip in snotel
#' @export


snotel_metric_custom <- function(df) {

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

  # if the columns match those in the current data frame
  # return it as is (previously processed)
  if(length(which(colnames(df) == snotel_columns)) == 8){
    message("File is already english, returning original!")
    return(df)
  }

  # if the data are metric, just rename the columns
  # otherwise convert from imperial to metric units
  if ( length(grep("degC", colnames(df))) >= 1 ){

    # rename columns
    colnames(df) <- snotel_columns

  } else {

    # rename the columns
    colnames(df) <- snotel_columns

    # convert the imperial to metric units
    # precipitation (inches)
    df$precipitation_cumulative <- df$precipitation_cumulative * 25.4
    df$precipitation <- df$precipitation * 25.4

    # temperature (fahrenheit to celcius)
    df$temperature_max <- (df$temperature_max - 32) * 5/9
    df$temperature_min <- (df$temperature_min - 32) * 5/9
    df$temperature_mean <- (df$temperature_mean - 32) * 5/9
  }

  # return data frame
  return(df)
}


snotel_download_custom <- function(
  site_id,
  path = tempdir(),
  internal = FALSE
){

  # trap empty site parameter, if all, downloadd all data
  # if string of IDs subset the dataset.
  if (base::missing(site_id)){
    stop("no site specified")
  }

  # download meta-data
  meta_data <- snotelr::snotel_info()
  meta_data <- meta_data[which(meta_data$site_id %in% site_id),]

  # check if the provided site index is valid
  if (nrow(meta_data) == 0){
    stop("no site found with the requested ID")
  }

  # for more than one site create a common output file
  if (length(site_id) > 1){
    filename <- "snotel_data.csv"
  }else{
    # filename
    filename <- sprintf("%s_%s.csv",
                        "snotel",
                        meta_data$site_id)
  }

  # loop over selection, and download the data
  snotel_data <- do.call("rbind",
                         lapply(seq_len(nrow(meta_data)), function(i){

                           # some feedback on the download progress
                           message(sprintf("Downloading site: %s, with id: %s\n",
                                           meta_data$site_name[i],
                                           meta_data$site_id[i]))

                           # download url (metric by default!)
                           base_url <- paste0(
                             "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport,metric/daily/",
                             meta_data$site_id[i], ":",
                             meta_data$state[i], ":",
                             meta_data$network[i],
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
                                             meta_data$site_id[i]))
                           }

                           # read in the snotel data
                           df <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                                                   header = TRUE,
                                                   sep = ",",
                                                   stringsAsFactors = FALSE)

                           # subsitute column names
                           df <- snotel_metric_custom(df)

                           # combine with the corresponding meta-data
                           # (remove warning on non matching size)
                           return(suppressWarnings(data.frame(meta_data[i,],df)))
                         }))

  # cleanup temporary file (if it exists)
  if(file.exists(file.path(tempdir(),"snotel_tmp.csv"))){
    file.remove(file.path(tempdir(), "snotel_tmp.csv"))
  }

  # return value internally, or write to file
  if (internal){
    return(snotel_data)
  } else {
    # overwrite the original with the metric version if desired
    # merging in the meta-data
    utils::write.table(snotel_data, file.path(path, filename),
                       quote = FALSE,
                       col.names = TRUE,
                       row.names = FALSE,
                       sep = ",")
  }
}

##### hourly #####

snotel_metric_custom <- function(df) {

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

  # if the columns match those in the current data frame
  # return it as is (previously processed)
  if(length(which(colnames(df) == snotel_columns)) == 8){
    message("File is already english, returning original!")
    return(df)
  }

  # if the data are metric, just rename the columns
  # otherwise convert from imperial to metric units
  # if ( length(grep("degC", colnames(df))) >= 1 ){

    # rename columns
    colnames(df) <- snotel_columns

  # } else {
  #
  #   # rename the columns
  #   colnames(df) <- snotel_columns
  #
  #   # convert the imperial to metric units
  #   # precipitation (inches)
  #   df$precipitation_cumulative <- df$precipitation_cumulative * 25.4
  #   df$precipitation <- df$precipitation * 25.4
  #
  #   # temperature (fahrenheit to celcius)
  #   df$temperature_max <- (df$temperature_max - 32) * 5/9
  #   df$temperature_min <- (df$temperature_min - 32) * 5/9
  #   df$temperature_mean <- (df$temperature_mean - 32) * 5/9
  # }
  #
  # return data frame
  return(df)
}


snotel_download_custom <- function(
  site_id,
  path = tempdir(),
  internal = FALSE
){

  # trap empty site parameter, if all, downloadd all data
  # if string of IDs subset the dataset.
  if (base::missing(site_id)){
    stop("no site specified")
  }

  # download meta-data
  meta_data <- snotelr::snotel_info()
  meta_data <- meta_data[which(meta_data$site_id %in% site_id),]

  # check if the provided site index is valid
  if (nrow(meta_data) == 0){
    stop("no site found with the requested ID")
  }

  # for more than one site create a common output file
  if (length(site_id) > 1){
    filename <- "snotel_data.csv"
  }else{
    # filename
    filename <- sprintf("%s_%s.csv",
                        "snotel",
                        meta_data$site_id)
  }

  # loop over selection, and download the data
  snotel_data <- do.call("rbind",
                         lapply(seq_len(nrow(meta_data)), function(i){

                           # some feedback on the download progress
                           message(sprintf("Downloading site: %s, with id: %s\n",
                                           meta_data$site_name[i],
                                           meta_data$site_id[i]))

                           # download url (metric by default!)
                           base_url <- paste0(
                             "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/hourly/start_of_period/",
                             meta_data$site_id[i], ":",
                             meta_data$state[i], ":",
                             meta_data$network[i],
                             "%7Cid=\"\"%7Cname/-168,0/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value,SNWD::value"
                           )

                           # try to download the data
                           error <- httr::GET(url = base_url,
                                              httr::write_disk(path = file.path(tempdir(),
                                                                                "snotel_tmp.csv"),
                                                               overwrite = TRUE))

                           # catch error and remove resulting zero byte files
                           if (httr::http_error(error)) {
                             warning(sprintf("Downloading site %s failed, removed empty file.",
                                             meta_data$site_id[i]))
                           }

                           # read in the snotel data
                           df <- utils::read.table(file.path(tempdir(),"snotel_tmp.csv"),
                                                   header = TRUE,
                                                   sep = ",",
                                                   stringsAsFactors = FALSE)

                           # subsitute column names
                           df <- snotel_metric_hourly(df)

                           # combine with the corresponding meta-data
                           # (remove warning on non matching size)
                           return(suppressWarnings(data.frame(meta_data[i,],df)))
                         }))

  # cleanup temporary file (if it exists)
  if(file.exists(file.path(tempdir(),"snotel_tmp.csv"))){
    file.remove(file.path(tempdir(), "snotel_tmp.csv"))
  }

  # return value internally, or write to file
  if (internal){
    return(snotel_data)
  } else {
    # overwrite the original with the metric version if desired
    # merging in the meta-data
    utils::write.table(snotel_data, file.path(path, filename),
                       quote = FALSE,
                       col.names = TRUE,
                       row.names = FALSE,
                       sep = ",")
  }
}
