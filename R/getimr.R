#----------------------------------------------------------------------
# getimr.R
# Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetEstimateViaMLT <- function(# Get IMR or U5MR from the other indicator using a model life table
  x.obs, ##<< U5MR or IMR values from which the other will be derived
  result = "IMR", ##<< Desired result ("IMR" or "U5MR")
  lifetable, ##<< Model life table (East, North, South, West, UN Chilean, UN Far East Asian, UN General, UN Latin, UN South Asian)
  allow.na = TRUE,
  lifetable.file = "input/data_lt.csv"
) {
  data.lt <- read.csv(file = lifetable.file, header = T, stringsAsFactors = F)
  if (result == "IMR") {
    y <- data.lt$imr[data.lt$type == lifetable]
    x <- data.lt$u5mr[data.lt$type == lifetable]
  } else {
    y <- data.lt$u5mr[data.lt$type == lifetable]
    x <- data.lt$imr[data.lt$type == lifetable]
  }
  x.obs <- x.obs/1000; y.int <- NULL
  for(i in 1:length(x.obs)) {
    if (is.na(x.obs[i])) {
      y.int[i] <- NA
    } else {
      compare <- x < x.obs[i]
      if (sum(compare == FALSE) == 0) {
        if (allow.na) {
          y.int[i] <- NA
        } else {
          y.int[i] <- max(y)
        }
      } else if (sum(compare == TRUE) == 0) {
        if (allow.na) {
          y.int[i] <- NA
        } else {
          y.int[i] <- min(y)
        }
      } else {
        for (j in 1:length(compare)) {
          if (compare[j] == FALSE & compare[j+1] == TRUE) 
            N <- j
        }
        d <- (x.obs[i]-x[N])/(x[N+1]-x[N])
        y.int[i] <- y[N]*(1-d) + y[N+1]*d
      }
    }
  }
  y.int <- y.int*1000
  ##value<<
  return(y.int = y.int) ##<< Vector of IMR or U5MR values derived using a model life table
}
#--------------------------------------------------
GetIMRFromSahelEquation <- function( # Get IMR from U5MR using the Sahel Equation
  u5mr, #<< U5MR values from which IMR will be derived
  sahelvalue #<< Country-specific variable for Sahel equations
) {
##details<<
## The Sahel equation is based on a multilevel model (Patrick Gerland, UNPD, 2018).
imr <- u5mr*(exp(0.24545062* log(u5mr/1000) +  (0.20760623* log(u5mr/1000)^2) -0.13575839
                   + sahelvalue))/(1+exp(0.24545062* log(u5mr/1000) +  (0.20760623* log(u5mr/1000)^2) -0.13575839
                                         +sahelvalue))

  
## The Sahel equation is based on a multilevel model (Patrick Gerland, UNPD, 2017).
# imr <- u5mr*(exp(0.2302357* log(u5mr/1000) +  (0.2044329 * log(u5mr/1000)^2) -0.1509166
#                  + sahelvalue))/(1+exp(0.2302357* log(u5mr/1000) +  (0.2044329 * log(u5mr/1000)^2) -0.1509166
#                                       + sahelvalue))


 
 ##details<<
 ## The Sahel equation is based on a multilevel model (Patrick Gerland, UNPD, 2016).
 # imr <- u5mr*(exp((-1.145686655*log(u5mr))
 #                 + (0.085341337*((log(u5mr))^2))
 #                 + 2.984221236
 #                 + sahelvalue))
    ##details<<
  ## The Sahel equation is based on a multilevel model (Patrick Gerland, UNPD, 2011).
  #imr <- u5mr*(exp((0.24050086*log(u5mr/1000))
  #                 + (0.15171803*((log(u5mr/1000))^2))
  #                 - 0.68271617
  #                + sahelvalue))
  ##value<<
  return(imr = imr) ##<< Vector of IMR values derived using the Sahel equation
}
