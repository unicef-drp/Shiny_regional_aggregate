#----------------------------------------------------------------------
# adjustments.R
# Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetHIVSubtractedSeries <- function( # Get HIV-subtracted series by subtracting UNAIDS estimates of HIV mortality from data observations
  year.i, ##<< Vector of observation years
  u.i, ##<< Vector of observations
  iso, ##<< ISO country code
  hiv.file ##<< File path to .csv with UNAIDS estimates 
  ## of HIV mortality which contains \code{country.hiv}, \code{countrycode.hiv}, 
  ## \code{year.hiv} and \code{unaids.hiv}
) {
  data.hiv <- read.csv(file = hiv.file)
  if (nrow(data.hiv) == 0)
    stop("No country HIV mortality data found in data.hiv.")
  ##details<< Linear interpolation of HIV mortality estimates is carried out for observation years
  ## which are not available in \code{hiv.file}. The value at the closest data extreme is returned for
  ## observation years outside the interval given in \code{hiv.file}
  unaids <- approx(x = data.hiv$year.hiv[data.hiv$countrycode.hiv == as.character(iso)], 
                   y = data.hiv$unaids.hiv[data.hiv$countrycode.hiv == as.character(iso)],
                   xout = roundoff(year.i, 1), method = "linear", rule = 2)
  unaids.i <- unaids$y
  year.hiv.range <- range(data.hiv$year.hiv[data.hiv$countrycode.hiv == as.character(iso)])
  # obtain HIV subtracted data series for HIV countries 
  #u.subtracted.i = u.i - ifelse(year.i < year.hiv.range[1] & year.i > year.hiv.range[2], 
  #                              0, unaids.i)
  u.subtracted.i = u.i - ifelse(year.i <= year.hiv.range[1] & year.i <= year.hiv.range[2], # Changed signs to remove adjustments before 1980 LH DJS 20180718
                                0, unaids.i)
  ##value<< 
  return(u.subtracted.i = u.subtracted.i) ##<< Vector of HIV-subtracted data observations
}
#--------------------------------------------------
GetCrisisSubtractedSeries <- function( # Get crisis-subtracted series by subtracting WHO estimates of crisis-related mortality from data observations
  year.i, ##<< Vector of observation years
  u.i, ##<< Vector of observations
  iso, ##<< ISO country code
  adj.file ##<< File path to .csv with post-adjustments/WHO estimates of child mortality from crises which contains \code{countrycode.adj}, \code{year.adj} and \code{add.adj}
) {
  data.adj <- read.csv(file = adj.file)
  if (nrow(data.adj) == 0)
    stop("No country crisis mortality data found in data.adj")
  ##details<< Linear interpolation of crisis mortality estimates is carried out for observation years
  ## which are outside the range of or in between the crisis years in \code{adjustment.file}. 
  ## For observation years that are not crisis years, the crisis mortality
  ## estimate is 0. Note that extreme values are first used for year.i that are not years.crisis, 
  # but set to 0 in a later step.
  years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso) & data.adj$add.adj != 0]
  if (length(years.crisis) > 1) {
    # extreme values used for year.t outside range of years.crisis, but set to 0 in a later step
    crisis <- approx(x = years.crisis, 
                     y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
                     xout = roundoff(year.i, 1), method = "linear", rule = 2)
    crisis.i <- crisis$y
    crisis.i[is.na(match(crisis$x,years.crisis))] <- 0 # edit DJS 2018-11-13 to handle two nonsequential crisis, e.g. Japan 2011.5 and 2015.5 
    u.subtracted.i <- u.i - crisis.i # edit DJS 2018-11-13 to handle two nonsequential crisis, e.g. Japan 2011.5 and 2015.5
    } else { # for the single year crisis 
      crisis.i <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(years.crisis)), data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)], 0)
      u.subtracted.i <- u.i - crisis.i
    } # if/else
  return(u.subtracted.i = u.subtracted.i) ##<< vector of crisis-subtracted data observations
}
#--------------------------------------------------
GetHIVAdjustedEstimates <- function( # Get HIV-adjusted estimates by adding UNAIDS estimates of HIV mortality
  year.t, ##<< Vector of estimate years
  u.t = NULL, ##<< Vector of estimates. If \code{NULL}, HIV adjustment is returned.
  iso, ##<< ISO country code.
  operation = "+", ##<< \code{"+"} to add HIV mortality, \code{"-"} to subtract it.
  hiv.file ##<< File path to .csv with UNAIDS estimates of HIV mortality which contains \code{countrycode.hiv}, \code{year.hiv} and \code{unaids.hiv}
) {
  data.hiv <- read.csv(file = hiv.file)
  if (nrow(data.hiv) == 0)
    stop("No country HIV mortality data found in data.hiv.")
  ##details<< Linear interpolation of HIV mortality estimates is carried out for observation years
  ## which are not available in \code{hiv.file}. The value at the closest data extreme is returned for
  ## observation years outside the interval given in \code{hiv.file}
  unaids <- approx(x = data.hiv$year.hiv[data.hiv$countrycode.hiv == as.character(iso)], 
                   y = data.hiv$unaids.hiv[data.hiv$countrycode.hiv == as.character(iso)],
                   xout = year.t, 
                   method = "linear", rule = 2)
  unaids.t <- unaids$y
  year.hiv.range <- range(data.hiv$year.hiv[data.hiv$countrycode.hiv == as.character(iso)])
  # add back UNAIDS estimates of HIV mortality
  if (is.null(u.t))
    u.t <- rep(0, length(year.t))
  u.unadjhiv.t <- u.t
  if (operation == "+") {
    #u.t <- u.unadjhiv.t + ifelse(year.t < year.hiv.range[1] & year.t > year.hiv.range[2], 
    #                             0, unaids.t)
    u.t <- u.unadjhiv.t + ifelse(year.t <= year.hiv.range[1] & year.t <= year.hiv.range[2], #Changed signs LH DJS 20180718
                                 0, unaids.t)

  } else if (operation == "-") {
    #u.t <- u.unadjhiv.t - ifelse(year.t < year.hiv.range[1] & year.t > year.hiv.range[2],
    #                             0, unaids.t)
    u.t <- u.unadjhiv.t - ifelse(year.t <= year.hiv.range[1] & year.t <= year.hiv.range[2],#Changed signs LH DJS 20180718
                                 0, unaids.t)

  } else {
    cat("Operation ", operation, " is not valid.\n")
    return()
  }
  # if vector of estimates not provided, proportion added to median estimate not calculated
  if (sum(u.unadjhiv.t, na.rm = TRUE) > 0) { 
    propadjhiv.t <- u.t/u.unadjhiv.t
  } else {
    propadjhiv.t <- NULL
  }
  ##value<< List with
  return(list(u.t = u.t, ##<< Vector of HIV-adjusted estimates
              propadjhiv.t = propadjhiv.t ##<< Vector of HIV adjustment factors
              )) 
}
#--------------------------------------------------
GetCrisisAdjustedEstimates <- function( # Get crisis-adjusted estimates by adding WHO estimates of crisis-related mortality
  year.t, ##<< Vector of estimate years
  u.t = NULL, ##<< Vector of estimates. If \code{NULL}, crisis adjustment is returned.
  operation = "+", ##<< \code{"+"} to add crisis mortality, \code{"-"} to subtract it.
  iso, ##<< ISO country code
  adj.file ##<< File path to .csv with post-adjustments/WHO estimates of child mortality from crises which contains \code{countrycode.adj}, \code{year.adj} and \code{add.adj}
) {
  data.adj <- read.csv(file = adj.file)
  if (nrow(data.adj) == 0)
    stop("No country crisis mortality data found in data.adj")
  ##details<< Linear interpolation of crisis mortality estimates is carried out for observation years
  ## which are outside the range of or in between the crisis years in \code{adjustment.file}. 
  ## For observation years that are not crisis years, the crisis mortality
  ## estimate is 0.
    years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso) & data.adj$add.adj != 0]
  if (length(years.crisis) > 1) {
    # extreme values used for year.t outside range of years.crisis, but set to 0 in a later step
    crisis <- approx(x = years.crisis,
                     y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
                     xout = roundoff(year.t, 1), method = "linear", rule = 2)
    crisis.t <- crisis$y
    crisis.t[is.na(match(crisis$x,years.crisis))] <- 0 # edit DJS 2018-11-13 to handle two nonsequential crisis, e.g. Japan 2011.5 and 2015.5 
  } else {
    crisis.t <- ifelse(as.character(roundoffmidpoint(year.t)) == as.character(roundoffmidpoint(years.crisis)),
                       data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
                       0)
  }

  # add WHO estimates of crisis-related mortality
  propadj.t <- NULL
  if (is.null(u.t))
    u.t <- rep(0, length(year.t))
  u.unadj.t <- u.t
  if (operation == "+") {
    u.t <- u.unadj.t + crisis.t
  } else if (operation == "-") {
    u.t <- u.unadj.t - crisis.t
  } else {
    cat("Operation ", operation, " is not valid.\n")
    return()
  }
  # if vector of estimates not provided, proportion added to median estimate not calculated
  if (sum(u.unadj.t, na.rm = TRUE) > 0) {
    propadj.t <- u.t/u.unadj.t
  } else {
    propadj.t <- NULL
  }
  ##value<< List with
  return(list(u.t = u.t, ##<< Vector of crisis-adjusted estimates
              propadj.t = propadj.t ##<< Vector of crisis adjustment factors
              ))
}