#corrected May 7, 2007
#modified October ,2011 to use apply for mean and sd
#modified April, 2012 to return 3 estimates, depending upon type
#partly based upon e1071  skewness and kurtosis
#' Skew
#'
#' @param x numeric vector
#' @param na.rm remove NA's
#' @param type numeric
#'
#' @return Skew from the \link[psych]{psych-package}
#'
#' @examples
skewed <-
  function (x, na.rm = TRUE,type=3)
  {
    if (length(dim(x)) == 0) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      sdx <- sd(x,na.rm=na.rm)
      mx <- mean(x)
      n <- length(x[!is.na(x)])
      switch(type,
             {skewer <- sqrt(n) *( sum((x - mx)^3,  na.rm = na.rm)/( sum((x - mx)^2,na.rm = na.rm)^(3/2)))}, #case 1
             {skewer <- n *sqrt(n-1) *( sum((x - mx)^3,  na.rm = na.rm)/((n-2) * sum((x - mx)^2,na.rm = na.rm)^(3/2)))}, #case 2
             {skewer <- sum((x - mx)^3)/(n * sd(x)^3) })  #case 3
    } else {

      skewer <- rep(NA,dim(x)[2])
      if (is.matrix(x)) {mx <- colMeans(x,na.rm=na.rm)} else {mx <- apply(x,2,mean,na.rm=na.rm)}
      sdx <- apply(x,2,sd,na.rm=na.rm)
      for (i in 1:dim(x)[2]) {
        n <- length(x[!is.na(x[,i]),i])
        switch(type,
               {skewer[i] <-sqrt(n) *( sum((x[,i] - mx[i])^3,  na.rm = na.rm)/( sum((x[,i] - mx[i])^2,na.rm = na.rm)^(3/2)))}, #type 1
               {skewer[i] <- n *sqrt(n-1) *( sum((x[,i] - mx[i])^3,  na.rm = na.rm)/((n-2) * sum((x[,i] - mx[i])^2,na.rm = na.rm)^(3/2)))},#type 2
               {skewer[i] <- sum((x[,i] - mx[i])^3,  na.rm = na.rm)/(n * sdx[i]^3)} #type 3
        ) #end switch
      } #end loop
    }
    return(skewer)
  }
