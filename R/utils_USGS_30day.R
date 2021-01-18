#' 30-day moving average
#'
#' @param procDV A previously created proc DV
#' @param site A USGS NWIS site
#' @param rolln Number of days in moving average
#'
#'
#' @return
#' @importFrom stats quantile loess predict
#' @importFrom dplyr summarize bind_rows
#' @importFrom zoo rollmean
#'
#' @examples
laura_DeCicco_fun <- function(procDV, site = NULL, rolln = 30, startDate = '2010-01-01', endDate = '2015-01-01') {

  if(missing(procDV) & is.null(site)) stop("Need at least one argument")

  if(missing(procDV)) {
    siteNumber<- {{ site }}
    parameterCd <- "00060" #Discharge
    dailyQ <- readNWISdv(siteNumber, parameterCd)
    dailyQ <- renameNWISColumns(dailyQ)
    stationInfo <- readNWISsite(siteNumber)

    dailyQ <- dailyQ %>% mutate(Station = stationInfo$station_nm,
                                drainage_area = stationInfo$drain_area_va)

    ma <- function(x,n={{ rolln }}){stats::filter(x,rep(1/n,n), sides=1)}

    dailyQ <- dailyQ %>%
      mutate(rollMean = as.numeric(ma(Flow)),
             day.of.year = as.numeric(strftime(Date,
                                               format = "%j")))

    } else {


  ma <- function(x,n= {{ rolln }}){stats::filter(x,rep(1/n,n), sides=1)}

  dailyQ <- {{ procDV }} %>%
    mutate(rollMean = as.numeric(ma(Flow)),
           day.of.year = as.numeric(strftime(Date,
                                             format = "%j")))

  }
  if(length(unique(dailyQ$Station)) > 1)stop("Only one station allowed, sorry.")


  summaryQ <- dailyQ %>%
    group_by(day.of.year) %>%
    summarize(p75 = quantile(rollMean, probs = .75, na.rm = TRUE),
              p25 = quantile(rollMean, probs = .25, na.rm = TRUE),
              p10 = quantile(rollMean, probs = 0.1, na.rm = TRUE),
              p05 = quantile(rollMean, probs = 0.05, na.rm = TRUE),
              p00 = quantile(rollMean, probs = 0, na.rm = TRUE))

#maybe make this more dynamic, e.g. allow for the user to select a date range.
  startDate = startDate %>% stringr::str_sub(end = 4) %>% as.numeric()
  endDate = endDate %>% stringr::str_sub(end = 4) %>% as.numeric()
  range = endDate-startDate
  summary_drought <- data.frame()
  for (i in 0:range){

    sum_date <- summaryQ %>%
      mutate(Date = as_date(day.of.year - 1,
                            origin = paste0(endDate-i,"-01-01")))

    summary_drought <- plyr::rbind.fill(sum_date, summary_drought)
  }


  latest.years <- dailyQ %>%
    filter(Date >= as_date(paste0(startDate,"-01-01")), Date <= as_date(paste0(endDate, '-01-01')))

  title.text <- paste0(dailyQ$Station,"\n",
                       "Provisional Data - Subject to change\n",
                       "Record Start = ", min(dailyQ$Date),
                       "  Number of years = ",
                       as.integer(as.numeric(difftime(time1 = max(dailyQ$Date),
                                                      time2 = min(dailyQ$Date),
                                                      units = "weeks"))/52.25),
                       "\nDate of plot = ",startDate,' to ',endDate,
                       "  Drainage Area = ",dailyQ$drainage_area, " mi^2")

  label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

  simple.plot <- ggplot(data = summary_drought, aes(x = Date)) +
    geom_ribbon(aes(ymin = p25, ymax = p75, fill = "Normal")) +
    geom_ribbon(aes(ymin = p10, ymax = p25, fill = "Drought Watch")) +
    geom_ribbon(aes(ymin = p05, ymax = p10, fill = "Drought Warning")) +
    geom_ribbon(aes(ymin = p00, ymax = p05, fill = "Drought Emergency")) +
    scale_y_log10() +
    geom_line(data = latest.years, aes(x=Date, y=rollMean, color = paste0(30,"-Day Mean")),size = .75)

  styled.plot <- sty.p(simple.plot, breaks = label.text, title = title.text)

  styled.plot


}


sty.p <- function(data, breaks, title) {

  data +
  annotation_logticks(sides="l") +
  theme_bw() +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title=title,
       y = paste(30,"-day moving ave", x="")) +
  scale_fill_manual(name="",breaks = breaks,
                    values = rev(c("red","orange","yellow","darkgreen"))) +
  scale_color_manual(name = "", values = "black") +
  theme(legend.position="bottom")
}
