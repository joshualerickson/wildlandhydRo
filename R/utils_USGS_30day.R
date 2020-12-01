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
#' @export
#'
#' @examples
laura_DeCicco_fun <- function(procDV, site = NULL, rolln = 30) {

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

  current.year <- as.numeric(strftime(Sys.Date(), format = "%Y"))

  summary.0 <- summaryQ %>%
    mutate(Date = as_date(day.of.year - 1,
                          origin = paste0(current.year-2,"-01-01")),
           day.of.year = day.of.year - 365)
  summary.1 <- summaryQ %>%
    mutate(Date = as_date(day.of.year - 1,
                          origin = paste0(current.year-1,"-01-01")))
  summary.2 <- summaryQ %>%
    mutate(Date = as_date(day.of.year - 1,
                          origin = paste0(current.year,"-01-01")),
           day.of.year = day.of.year + 365)

  summaryQ <- bind_rows(summary.0, summary.1, summary.2)

  smooth.span <- 0.3

  summaryQ$sm.75 <- predict(loess(p75~day.of.year, data = summaryQ, span = smooth.span))
  summaryQ$sm.25 <- predict(loess(p25~day.of.year, data = summaryQ, span = smooth.span))
  summaryQ$sm.10 <- predict(loess(p10~day.of.year, data = summaryQ, span = smooth.span))
  summaryQ$sm.05 <- predict(loess(p05~day.of.year, data = summaryQ, span = smooth.span))
  summaryQ$sm.00 <- predict(loess(p00~day.of.year, data = summaryQ, span = smooth.span))

  summaryQ <- select(summaryQ, Date, day.of.year,
                     sm.75, sm.25, sm.10, sm.05, sm.00) %>%
    filter(Date >= as_date(paste0(current.year-1,"-01-01")))

  latest.years <- dailyQ %>%
    filter(Date >= as_date(paste0(current.year-1,"-01-01"))) %>%
    mutate(day.of.year = 1:nrow(.))

  title.text <- paste0(dailyQ$Station,"\n",
                       "Provisional Data - Subject to change\n",
                       "Record Start = ", min(dailyQ$Date),
                       "  Number of years = ",
                       as.integer(as.numeric(difftime(time1 = max(dailyQ$Date),
                                                      time2 = min(dailyQ$Date),
                                                      units = "weeks"))/52.25),
                       "\nDate of plot = ",Sys.Date(),
                       "  Drainage Area = ",dailyQ$drainage_area, " mi^2")

  mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
  start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
  label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")

  summary.year1 <- data.frame(summaryQ[2:366,])
  summary.year2 <- data.frame(summaryQ[367:733,])


  simple.plot <- ggplot(data = summaryQ, aes(x = day.of.year)) +
    geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) +
    geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) +
    geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) +
    geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) +
    scale_y_log10() +
    geom_line(data = latest.years, aes(x=day.of.year, y=rollMean, color = paste0({{ rolln }},"-Day Mean")),size=2) +
    geom_vline(xintercept = 365)

  styled.plot <- simple.plot +
    scale_x_continuous(breaks = c(mid.month.days,365+mid.month.days),
                       labels = rep(month.letters,2),
                       expand = c(0, 0),
                       limits = c(0,730)) +
    annotation_logticks(sides="l") +
    expand_limits(x=0) +
    annotate(geom = "text",
             x = c(182,547),
             y = 1,
             label = c(current.year-1,current.year), size = 4) +
    theme_bw() +
    theme(axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title=title.text,
         y = paste({{ rolln }},"-day moving ave", x="")) +
    scale_fill_manual(name="",breaks = label.text,
                      values = rev(c("red","orange","yellow","darkgreen"))) +
    scale_color_manual(name = "", values = "black") +
    theme(legend.position="bottom")

  styled.plot

}
