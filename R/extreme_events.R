
#' Batched Frequencies
#' @description This function fits a variable (e.g., flow, snow, rain, etc) to a univariate distribution
#' function, e.g. Weibull,
#' GEV, Log-pearson type III, Pearson, Gumbel, Normal and Lognormal distributions.
#' It is up to the user to decide which distribution function to use and whether any pre-processing is necessary (outliers, mixed populations, etc).
#' @param data A \code{data.frame} with \code{numeric} variable and date
#' @param value A \code{numeric} vector.
#'
#' @return A \code{data.frame} with return intervals and associated value for Weibull,
#' GEV, Log-pearson type III, Pearson, Gumbel, Normal and Lognormal distributions.
#' @importFrom dplyr transmute
#' @importFrom stats na.omit
#' @importFrom extRemes fevd
#' @export
#'

batch_frequency <- function(data, value) {

 if(missing(data)){

   max.x <- data.frame(x = {{ value }}) %>% na.omit()

 }  else {

  max.x <- data %>% transmute(x = {{ value }}) %>% na.omit()
 }

  if (!nrow(max.x) >= 2) {stop("Warning: Need more data.")}

  mean.x <- mean(max.x$x, na.rm = TRUE) #mean of max Q

  sd.x <- sd(max.x$x, na.rm = TRUE) #standard deviation of max Q

  log_mean.x <- mean(log(max.x$x), na.rm = TRUE) #log mean of max Q

  log_sd.x <- sd(log(max.x$x), na.rm = TRUE) #log of of sd of max Q

  #equation from Maighty 2018, but doesnt' account for n; assumes n in infinite
  #reason to use the fevd() keep because someone might want it???

  # gum_scale.x <- sd(max.x$x, na.rm = TRUE)/1.2825 #scale for gumbel
  #
  # gum_loc.x <- mean(max.x$x, na.rm = TRUE) - sd(max.x$x, na.rm = TRUE)*0.4501 #loc for gumbel

  fevd_gum <- fevd(max.x$x, time.units = "years", type = "Gumbel")

  scale_gum <- fevd_gum$results$par[2] %>% unname()

  loc_gum <- fevd_gum$results$par[1] %>% unname()


  fevd_gev <- fevd(max.x$x, time.units = "years", type = "GEV")

  scale_gev <- fevd_gev$results$par[2] %>% unname()

  loc_gev <- fevd_gev$results$par[1] %>% unname()

  shape_gev <- fevd_gev$results$par[3] %>% unname()

  skew <- skewed(max.x$x,type = 3, na.rm = TRUE)

  skew_log <- skewed(log(max.x$x),type = 3, na.rm = TRUE)

  weib <- fitdist(max.x$x, distr = 'weibull')

  weib_shape <- weib$estimate[1]

  weib_scale <- weib$estimate[2]

  ReturnInterval <- c(1.0101,2,5,10,15,25,30,35,40,45,50,60,70,80,90,100,150,200)

  lpIII <- smwrBase::qlpearsonIII(1-(1/ReturnInterval), mean = log_mean.x, sd = log_sd.x,  skew = skew_log)

  lp <- smwrBase::qpearsonIII(1-(1/ReturnInterval), mean = mean.x, sd = sd.x, skew = skew)

  normal <- stats::qnorm(1-(1/ReturnInterval),mean = mean.x, sd = sd.x, lower.tail = TRUE)

  lognormal <- stats::qlnorm(1-(1/ReturnInterval),mean = log_mean.x, sd = log_sd.x)

  Gumbel <- evd::qgumbel(1-(1/ReturnInterval), scale = scale_gum, loc = loc_gum)

  GEV <- evd::qgev(1-(1/ReturnInterval), loc = loc_gev, scale = scale_gev, shape = shape_gev)

  weibull <- stats::qweibull(1-(1/ReturnInterval), scale = weib_scale, shape = weib_shape)


  Flood.Freq <- data.frame(ReturnInterval)

  Flood.Freq <- Flood.Freq %>%
    mutate(LogPearson = lpIII, Pearson = lp, Gumbel = Gumbel,
           GEV = GEV, Normal = normal, Lognormal = exp(lognormal),
           Weibull = weibull)

  Flood.Freq <- Flood.Freq %>%
    pivot_longer(cols = c("Gumbel", "LogPearson", "GEV", "Normal", "Lognormal", "Pearson", "Weibull"),
                 values_to = "Value",
                 names_to = "Distribution")

  return(as.data.frame(Flood.Freq))
}


#' Distribution Test
#' @description This function uses multiple statistics package \link[fitdistrplus]{fitdistrplus-package},
#' \link[evd]{evd-package}, \link[smwrBase]{smwrBase-package} and the \link[stats]{stats} to generate
#' multiple theoretical and empirical univariate distributions, e.g. Weibull,
#' GEV, Log-pearson type III, Pearson, Gumbel, Normal and Lognormal distributions.
#' @param data A \code{data.frame}
#' @param value A \code{numeric} vector
#' @importFrom purrr map transpose safely
#' @importFrom tidyr nest unnest
#' @importFrom dplyr mutate everything
#' @importFrom fitdistrplus fitdist
#' @importFrom stats dgamma pgamma qgamma rnorm rgamma
#' @importFrom stats dnorm qnorm pnorm dlnorm qlnorm plnorm dweibull qweibull pweibull
#' @importFrom extRemes fevd
#' @return A \code{list} with named distributions, e.g. Weibull,
#' GEV, Log-pearson type III, Pearson, Gumbel, Normal and Lognormal distributions.
#' @export
#'
batch_distribution <- function(data, value,...) {


  if(missing(data)){

    max.x <- data.frame(x = {{ value }}) %>% na.omit()

  }  else {

    max.x <- data %>% transmute(x = {{ value }}) %>% na.omit()
  }


  lot_outliers <- MGBT::MGBT(max.x$x)$LOThresh

  if(lot_outliers > 0) {max.x_lp <- subset(max.x, x > lot_outliers)} else {max.x_lp <- max.x}

  mean.x <- mean(max.x$x, na.rm = TRUE) #mean of max Q

  sd.x <- sd(max.x$x, na.rm = TRUE) #standard deviation of max Q

  mean.x_pearson <- mean(max.x_lp$x, na.rm = TRUE) #mean of max Q

  sd.x_pearson <- sd(max.x_lp$x, na.rm = TRUE) #standard deviation of max Q

  log_mean.x <- mean(log(max.x_lp$x), na.rm = TRUE) #log mean of max Q

  log_sd.x <- sd(log(max.x_lp$x), na.rm = TRUE) #log of of sd of max Q

  fevd_gum <- fevd(max.x$x, time.units = "years", type = "Gumbel")

  scale_gum <- fevd_gum$results$par[2] %>% unname()

  loc_gum <- fevd_gum$results$par[1] %>% unname()

  fevd_gev <- fevd(max.x$x, time.units = "years", type = "GEV")

  scale_gev <- fevd_gev$results$par[2] %>% unname()

  loc_gev <- fevd_gev$results$par[1] %>% unname()

  shape_gev <- fevd_gev$results$par[3] %>% unname()

  skew <- skewed(max.x$x,type = 3, na.rm = TRUE)

  skew.x_pearson <- skewed(max.x_lp$x, type = 3, na.rm = TRUE)

  skew_log <- skewed(log(max.x_lp$x),type = 3, na.rm = TRUE)

  weib <- fitdist(max.x$x, distr = 'weibull',...)
  lnorm <- fitdist(max.x$x, distr = 'lnorm',...)
  norm <- fitdist(max.x$x, distr = 'norm',...)
  lpearson <- fitdist(log(max.x_lp$x), distr = "lpearsonIII", start = list(meanlog = log_mean.x, sdlog = log_sd.x, skew = skew_log),...)
  pearson <- fitdist(max.x$x,  distr = 'pearsonIII', start = list(mean = mean.x_pearson, sd = sd.x_pearson, skew = skew.x_pearson),...)
  gumbel <- fitdist(max.x$x, distr = 'gumbel', start = list(loc = loc_gum, scale = scale_gum),...)
  gev <- suppressWarnings(fitdist(max.x$x, distr = 'gev', start = list(loc = loc_gev, scale = scale_gev, shape = shape_gev),...))


  dist_list <- list(weib = weib, lnorm = lnorm, norm = norm, lpearson = lpearson, pearson = pearson,
                    gumbel = gumbel, gev = gev)

}



#' Plot Density
#' @description This function plots a \link[wildlandhydRo]{batch_distribution} object using
#'  a probability density function.
#' @param procDist A previously created \link[wildlandhydRo]{batch_distribution} object
#' @param facet Ability to facet by distribution. default \code{TRUE}
#'
#' @return A ggplot object. Can use with \link[plotly]{ggplotly}
#' @export
#' @import ggplot2
#'
#'

plot_densDist <- function(procDist, facet = TRUE) {

  dens_gplot <- fitdistrplus::denscomp(procDist, plotstyle = 'ggplot')

  p <- dens_gplot[["layers"]][[1]][["data"]] %>% select(values, sfin)
  d <- dens_gplot[["layers"]][[2]][["data"]]

  if (facet == "TRUE") {
  ggplot() + geom_histogram(data = p, aes(sfin,..density..), color = 'black', fill = 'white', bins = 15) +
    geom_line(data = d, aes(sfin, values, color = ind), size = 1.3) +
    facet_wrap(~ind) +theme_bw() +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = "Values",
         title = "Histogram (empirical) and Theoretical Densities",
         y = "Density",
         color = "Distribution")

  } else {

    ggplot() + geom_histogram(data = p, aes(sfin,..density..), color = 'black', fill = 'white', bins = 15) +
      geom_line(data = d, aes(sfin, values, color = ind), size = 1.3) +
      theme_bw() +
      scale_x_continuous(labels = scales::comma_format()) +
      labs(x = "Values",
           title = "Histogram (empirical) and Theoretical Densities",
           y = "Density",
           color = "Distribution")
  }
}

#' Plot Q-Q
#' @description This function plots a \link[wildlandhydRo]{batch_distribution} object using
#'  quantile (empirical) to quantile (theoretical) methods for associated distributions.
#' @param procDist A previously created \link[wildlandhydRo]{batch_distribution} object
#' @param facet Ability to facet by distribution. default \code{TRUE}
#'
#' @return A ggplot object. Can use with \link[plotly]{ggplotly}
#' @export
#'
plot_qqDist <- function(procDist, facet = TRUE) {

  qq_gplot <- fitdistrplus::qqcomp(procDist, plotstyle = "ggplot")

  qq <- qq_gplot[["data"]]

  if (facet == "TRUE") {

    ggplot(data = qq) +
      geom_point(size = 1, aes(values, sdata, color = ind), show.legend = F)+
      geom_line(aes(values, sdata, color = ind), size = 1, show.legend = F) +
      scale_x_continuous(labels = scales::comma_format()) +
      geom_abline() +
      scale_y_continuous(labels = scales::comma_format()) +
      labs(x = "Theoretical Quantiles", title = "Q-Q Plot", y = "Empirical Quantiles")+
      theme_bw()  + facet_wrap(~ind) + theme(legend.position = 'none')

  } else {

    ggplot(data = qq) +
      geom_point(size = 1, aes(values, sdata, color = ind), show.legend = F)+
      geom_line(aes(values, sdata, color = ind), size = 1, show.legend = F) +
      scale_x_continuous(labels = scales::comma_format()) +
      geom_abline() +
      scale_y_continuous(labels = scales::comma_format()) +
      labs(x = "Theoretical Quantiles", title = "Q-Q Plot", y = "Empirical Quantiles")+
      theme_bw()

  }
}


#' Plot CDF
#'
#' @description This function plots a \link[wildlandhydRo]{batch_distribution} object using
#'  a cumulative density function.
#' @param procDist A previously created \link[wildlandhydRo]{batch_distribution} object
#' @param facet Ability to facet by distribution. default \code{TRUE}
#'
#' @return A ggplot object. Can use with \link[plotly]{ggplotly}
#' @export
#'
plot_cdfDist <- function(procDist, facet = TRUE) {

  cdf_gplot <- fitdistrplus::cdfcomp(procDist, plotstyle = "ggplot")

  p_cdf <- cdf_gplot[["layers"]][[1]][["data"]] %>% select(values, sfin)
  h_cdf <- cdf_gplot[["layers"]][[2]][["data"]] %>% select(-ind)
  l_cdf <- cdf_gplot[["layers"]][[5]][["data"]]


  if (facet == "TRUE") {
  ggplot() + geom_point(data = p_cdf, aes(sfin,values), color = 'black') +
    geom_segment(data = h_cdf, aes(x, y, xend = xend, yend = yend)) +
    geom_line(data = l_cdf, aes(sfin,values, color = ind), size = .75) +
    facet_wrap(~ind) +theme_bw() +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = "Values",
         title = "CDF Plot",
         y = "Probabilities",
         color = "Distributions")

  } else {

    ggplot() + geom_point(data = p_cdf, aes(sfin,values), color = 'black') +
      geom_segment(data = h_cdf, aes(x, y, xend = xend, yend = yend)) +
      geom_line(data = l_cdf, aes(sfin,values, color = ind), size = .75) +
      theme_bw() +
      scale_x_continuous(labels = scales::comma_format()) +
      scale_y_continuous(labels = scales::comma_format()) +
      labs(x = "Values",
           title = "CDF Plot",
           y = "Probabilities",
           color = "Distributions")
  }
}


#' Plot P-P
#'
#' @description This function plots a \link[wildlandhydRo]{batch_distribution} object using
#'  a probability (empirical) to probability (theoretical) methods for associated distributions.
#' @param procDist A previously created \link[wildlandhydRo]{batch_distribution} object
#' @param facet Ability to facet by distribution. default \code{TRUE}
#'
#' @return A ggplot object. Can use with \link[plotly]{ggplotly}
#' @export
#'
plot_ppDist <- function(procDist, facet = TRUE) {

  pp_gplot <- fitdistrplus::ppcomp(procDist, plotstyle = "ggplot")

  pp <- pp_gplot[["data"]]

  if (facet == "TRUE") {

    ggplot(data = pp) +
      geom_point(size = 1, aes(values, obsp, color = ind)) + geom_abline() +
      labs(x = "Theoretical Probabilites", title = "P-P Plot", y = "Empirical Probabilities")+
      theme_bw()  + facet_wrap(~ind) + theme(legend.position = 'none')


  } else {

    ggplot(data = pp) +
      geom_point(size = 1, aes(values, obsp, color = ind)) + geom_abline() +
      labs(x = "Theoretical Probabilites", title = "P-P Plot", y = "Empirical Probabilities",
           color = "Distributions")+
      theme_bw()


    }

}


#' Plot Distribution Plots Together
#'
#' @description This function plots all of the  \link[wildlandhydRo]{wildlandhydRo-package}
#' plot_*Dist functions. Mostly for reporting but also good for a quick EDA.
#' @param procDist A previously created \link[wildlandhydRo]{batch_distribution} object
#'
#' @return A ggplot object. Can't use with \link[plotly]{ggplotly}
#' @export
#'

plot_allDist <- function(procDist) {

  dens <- plot_densDist(procDist, facet = FALSE)
  cdf <- plot_cdfDist(procDist, facet = FALSE)
  pp <- plot_ppDist(procDist, facet = FALSE)
  qq <- plot_qqDist(procDist, facet = FALSE)

  ggpubr::ggarrange(dens, qq, pp, cdf, nrow = 2, ncol = 2, common.legend = TRUE)
}

#' Summary Statistic Distribution Report
#'
#' @description This function uses a \link[wildlandhydRo]{batch_distribution} to tidy a
#' \link[fitdistrplus]{gofstat} function. Good for choosing which distribution to use. For more details
#' see the \link[fitdistrplus]{fitdistrplus-package} on how to use the output.
#' @param procDist A previously created \link[wildlandhydRo]{batch_distribution} object
#'
#'
#' @return A \code{data.frame}
#' @importFrom fitdistrplus gofstat fitdist
#' @export
#'
#'
reportDist <- function(procDist) {

  gstat <- gofstat(procDist) %>% .[c("cvm", "ks", "ad", "aic", "bic")] %>%
    data.frame() %>% rename(`Cramer-von Mises` = "cvm", `Kolmogorov-Smirnov` = "ks",
                            `Anderson-Darling` = "ad", `Akaike's Information Criterion` = "aic",
                            `Bayesian Information Criterion` = "bic")

  v <- rownames(gstat)

  rownames(gstat) <- NULL

  gstat <- gstat %>% dplyr::mutate(Distribution = v)
print(gstat)
}

#' Plot Distribution Report
#'
#' @description This function plots \link[wildlandhydRo]{reportDist} object. it provides
#' the user graphs for each statistic.
#' @param report A previously created \link[wildlandhydRo]{reportDist} object
#'
#' @return A ggplot object. Can use with \link[plotly]{ggplotly}
#' @importFrom forcats fct_reorder
#' @export
#'

plot_reportDist <- function(report) {

  report %>% pivot_longer(cols = -Distribution, names_to = "stat") %>%
    mutate(Distribution = fct_reorder(Distribution, value),
           stat = factor(stat, levels = c("Cramer-von Mises", "Kolmogorov-Smirnov",
                                          "Anderson-Darling", "Akaike's Information Criterion",
                                          "Bayesian Information Criterion"))) %>%
    ggplot() + geom_point(aes(Distribution, value)) +
    coord_flip() +
    theme_bw() + theme(strip.text = element_text(size = 8.5)) +
    labs(x = "Statistic Value", title = "Summary Statistics per Distribution") +
    facet_wrap(~stat, scales = "free")
}

#' Get Exceedance Probability
#' @description Takes a numeric vector to find plotting position methods for exceedance probability.
#' @param x A numeric vector
#' @param a A numeric value for Gringoten formula
#' @return A data.frame with exceedance probabilities and return periods.
#' @note Methods used are: California, Hazen, Weibull, Chegodayev, Blom and Gringoten.
#'
#'

get_exceedance <- function(x, a = 0.375) {

  #code taken from JP Gannon
  #https://vt-hydroinformatics.github.io/floods.html#calculate-exceedance-probability-and-return-period
  ranks <- data.frame(m = rank(-x), x = x)

  N <- length(ranks$m)

  ex_prob <- ranks %>% mutate(gringoten = (m - a) / (N + 1 - (2*a)),
                              california = m/N,
                              hazen = (m-0.5)/N,
                              weibull = m/(N+1),
                              chegodayev = (m-0.3)/(N+0.4),
                              blom = (m-0.44)/(N+0.12)) %>%
    mutate(ri_gringoten = 1 / gringoten,
           ri_california = 1/california,
           ri_hazen = 1/hazen,
           ri_weibull = 1/weibull,
           ri_chegodayev = 1/chegodayev,
           ri_blom = 1/blom)

}


