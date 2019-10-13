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
  u.subtracted.i = u.i - ifelse(year.i < year.hiv.range[1] & year.i > year.hiv.range[2], 
                                0, unaids.i)
  ##value<< 
  return(u.subtracted.i = u.subtracted.i) ##<< Vector of HIV-subtracted data observations
}
#--------------------------------------------------
# GetCrisisSubtractedSeries <- function( # Get crisis-subtracted series by subtracting WHO estimates of crisis-related mortality from data observations
#   year.i, ##<< Vector of observation years
#   u.i, ##<< Vector of observations
#   iso, ##<< ISO country code
#   adj.file ##<< File path to .csv with post-adjustments/WHO estimates of child mortality from crises which contains \code{countrycode.adj}, \code{year.adj} and \code{add.adj}
# ) {
#   data.adj <- read.csv(file = adj.file)
#   if (nrow(data.adj) == 0)
#     stop("No country crisis mortality data found in data.adj")
#   ##details<< Linear interpolation of crisis mortality estimates is carried out for observation years
#   ## which are not available but within the range of observation years in \code{adjustment.file}. 
#   ## For observation years outside the interval given in \code{adjustment.file}, the crisis mortality
#   ## estimate is 0. Note that extreme values are first used for year.t outside range of years.crisis, 
#   # but set to 0 in a later step.
#   years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso) & data.adj$add.adj != 0]
#   if (length(years.crisis) > 1) {
#     crisis <- approx(x = years.crisis, 
#                      y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
#                      xout = roundoff(year.i, 1), method = "linear", rule = 2)
#     crisis.i <- crisis$y
#     u.subtracted.i <- u.i - ifelse(year.i >= min(years.crisis) & year.i <= max(years.crisis), crisis.i, 0)
#   } else {
#     crisis.i <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(years.crisis)), 
#                        data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
#                        0)
#     u.subtracted.i <- u.i - crisis.i
#   }
#   ## deprecated as of 5 Jul 2013
#   ## subtract WHO estimates of crisis-related child mortality
#   #u.subtracted.i <- NULL
#   #for(i in 1:length(year.i)) {
#   #  u.subtracted.i[i] <- u.i[i] - ifelse(sum(data.adj$countrycode.adj == iso & 
#   #                                             data.adj$year.adj == year.i[i]) == 1, 
#   #                                       data.adj$add.adj[data.adj$countrycode.adj == iso & 
#   #                                                          data.adj$year.adj == year.i[i]], 0)
#   #}
#   ##value<< 
#   return(u.subtracted.i = u.subtracted.i) ##<< vector of crisis-subtracted data observations
# }
## GetCrisisSubtractedSeries with ad hoc edits for Japan -- needs crisis to be two seperate events DJS 2017-05-23
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
  ## which are not available but within the range of observation years in \code{adjustment.file}. 
  ## For observation years outside the interval given in \code{adjustment.file}, the crisis mortality
  ## estimate is 0. Note that extreme values are first used for year.t outside range of years.crisis, 
  # but set to 0 in a later step.
  years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso) & data.adj$add.adj != 0]
  if (length(years.crisis) > 1 & as.character(iso)!="JPN") { #& DJS edit as.character(iso)!="JPN" is ad hoc change so the 2 crisis in Japan are seperate events and not interpolated 
    # extreme values used for year.t outside range of years.crisis, but set to 0 in a later step
    crisis <- approx(x = years.crisis, 
                     y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
                     xout = roundoff(year.i, 1), method = "linear", rule = 2)
    crisis.i <- crisis$y
    u.subtracted.i <- u.i - ifelse(year.i >= min(years.crisis) & year.i <= max(years.crisis), crisis.i, 0)
  } else {
    if(as.character(iso)=="JPN"){ # DJS edit ad hoc change in code for 2 crisis in Japan
      crisis.i.2011 <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(min(years.crisis))), data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],0)
      crisis.i.2015 <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(max(years.crisis))), data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],0)
      u.subtracted.i <- u.i - crisis.i.2011 - crisis.i.2015 
    } else {
      crisis.i <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(years.crisis)), data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)], 0)
      u.subtracted.i <- u.i - crisis.i
    } # if/else
  } # if/else
  ## deprecated as of 5 Jul 2013
  ## subtract WHO estimates of crisis-related child mortality
  #u.subtracted.i <- NULL
  #for(i in 1:length(year.i)) {
  #  u.subtracted.i[i] <- u.i[i] - ifelse(sum(data.adj$countrycode.adj == iso & 
  #                                             data.adj$year.adj == year.i[i]) == 1, 
  #                                       data.adj$add.adj[data.adj$countrycode.adj == iso & 
  #                                                          data.adj$year.adj == year.i[i]], 0)
  #}
  ##value<< 
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
    u.t <- u.unadjhiv.t + ifelse(year.t < year.hiv.range[1] & year.t > year.hiv.range[2], 
                                 0, unaids.t)
  } else if (operation == "-") {
    u.t <- u.unadjhiv.t - ifelse(year.t < year.hiv.range[1] & year.t > year.hiv.range[2],
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
# GetCrisisAdjustedEstimates <- function( # Get crisis-adjusted estimates by adding WHO estimates of crisis-related mortality
#   year.t, ##<< Vector of estimate years
#   u.t = NULL, ##<< Vector of estimates. If \code{NULL}, crisis adjustment is returned.
#   operation = "+", ##<< \code{"+"} to add crisis mortality, \code{"-"} to subtract it.
#   iso, ##<< ISO country code
#   adj.file ##<< File path to .csv with post-adjustments/WHO estimates of child mortality from crises which contains \code{countrycode.adj}, \code{year.adj} and \code{add.adj}
# ) {
#   data.adj <- read.csv(file = adj.file)
#   if (nrow(data.adj) == 0)
#     stop("No country crisis mortality data found in data.adj")
#   ##details<< Linear interpolation of crisis mortality estimates is carried out for observation years
#   ## which are not available but within the range of observation years in \code{adjustment.file}. 
#   ## For observation years outside the interval given in \code{adjustment.file}, the crisis mortality
#   ## estimate is 0.
#   years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso) & data.adj$add.adj != 0]
#   if (length(years.crisis) > 1) {
#     # extreme values used for year.t outside range of years.crisis, but set to 0 in a later step
#     crisis <- approx(x = years.crisis, 
#                      y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
#                      xout = roundoff(year.t, 1), method = "linear", rule = 2)
#     crisis.t <- crisis$y
#   } else {
#     crisis.t <- ifelse(as.character(roundoffmidpoint(year.t)) == as.character(roundoffmidpoint(years.crisis)),
#                        data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
#                        0)
#   }
#   
#   # add WHO estimates of crisis-related mortality
#   propadj.t <- NULL
#   if (is.null(u.t)) 
#     u.t <- rep(0, length(year.t))  
#   u.unadj.t <- u.t
#   if (operation == "+") {
#     u.t <- u.unadj.t +
#       ifelse(year.t >= min(years.crisis) & year.t <= max(years.crisis), crisis.t, 0)
#   } else if (operation == "-") {
#     u.t <- u.unadj.t -
#       ifelse(year.t >= min(years.crisis) & year.t <= max(years.crisis), crisis.t, 0)
#   } else {
#     cat("Operation ", operation, " is not valid.\n")
#     return()
#   }
#   # deprecated as of 5 Jul 2013
#   ## add WHO estimates of crisis-related mortality
#   #propadj.t <- NULL
#   #if (is.null(u.t)) 
#   #  u.t <- rep(0, length(year.t))  
#   #u.unadj.t <- u.t
#   #for (t in 1:length(year.t)) {
#   #  if (operation == "+") {
#   #    u.t[t] <- u.unadj.t[t] +
#   #      ifelse(sum(data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]) == 1,
#   #             data.adj$add.adj[data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]], 0)
#   #  } else if (operation == "-") {
#   #    u.t[t] <- u.unadj.t[t] -
#   #      ifelse(sum(data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]) == 1,
#   #             data.adj$add.adj[data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]], 0)
#   #  } else {
#   #    cat("Operation ", operation, " is not valid.\n")
#   #    return()
#   #  }
#   #}
#   # if vector of estimates not provided, proportion added to median estimate not calculated
#   if (sum(u.unadj.t, na.rm = TRUE) > 0) {
#     propadj.t <- u.t/u.unadj.t
#   } else {
#     propadj.t <- NULL
#   }
#   ##value<< List with
#   return(list(u.t = u.t, ##<< Vector of crisis-adjusted estimates
#               propadj.t = propadj.t ##<< Vector of crisis adjustment factors
#               ))
# }
## GetCrisisAdjustedEstimates with ad hoc edits for Japan -- needs crisis to be two seperate events DJS 2017-05-23
GetCrisisAdjustedEstimates <- function( # Get crisis-adjusted estimates by adding WHO estimates of crisis-related mortality
  year.t, ##<< Vector of estimate years
  u.t = NULL, ##<< Vector of estimates. If \code{NULL}, crisis adjustment is returned.
  operation = "+", ##<< \code{"+"} to add crisis mortality, \code{"-"} to subtract it.
  iso, ##<< ISO country code
  adj.file ##<< File path to .csv with post-adjustments/WHO estimates of child mortality from crises which contains \code{countrycode.adj}, \code{year.adj} and \code{add.adj}
  # n.adj.traj=1 ## number of adjustment trajectories; if(NULL) will produce 8000, DJS edit added argument for producing trajectory-specific adjustments rather than a single adjustment applied to each trajectory, default =1 applies adjustment from first column of adjustment values in adj.file
) {
  # if(is.null(n.adj.traj)) n.adj.traj<-8000
  data.adj <- read.csv(file = adj.file)
  if (nrow(data.adj) == 0)
    stop("No country crisis mortality data found in data.adj")
  ##details<< Linear interpolation of crisis mortality estimates is carried out for observation years
  ## which are not available but within the range of observation years in \code{adjustment.file}. 
  ## For observation years outside the interval given in \code{adjustment.file}, the crisis mortality
  ## estimate is 0.
  years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso)] # edit DJS 2017-08-08 removed '& data.adj$add.adj != 0' since no add.adj = 0 and column names have changed since adding additional uncertainty to crisis years
  if (length(years.crisis) > 1) { 
    # extreme values used for year.t outside range of years.crisis, but set to 0 in a later step
    # for(i in 1:n.adj.traj){
    #   crisis.i <- approx(x = years.crisis,
    #                      y = data.adj[data.adj$countrycode.adj == as.character(iso),i+3],
    #                      xout = roundoff(year.t, 1), method = "linear", rule = 2)
    #   ifelse(i==1, crisis.t<-crisis.i$y, crisis.t<-rbind(crisis.t, crisis.i$y)) ## this should be a matrix of with n.adj.traj rows, columns=length(years)
    # }
    # if(n.adj.traj==1) crisis.t <- t(as.matrix(crisis.t))
    crisis <- approx(x = years.crisis,
                     y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
                     xout = roundoff(year.t, 1), method = "linear", rule = 2)
    crisis.t <- crisis$y
  } else {
    # for(i in 1:n.adj.traj){
    #   crisis.t <- ifelse(as.character(roundoffmidpoint(year.t)) == as.character(roundoffmidpoint(years.crisis)),
    #                      data.adj[data.adj$countrycode.adj == as.character(iso),i+3],0)
    # } # i loop
    # if(n.adj.traj==1) crisis.t <- t(as.matrix(crisis.t))
    crisis.t <- ifelse(as.character(roundoffmidpoint(year.t)) == as.character(roundoffmidpoint(years.crisis)),data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],0)
  }
  
  # crisis.temp <- rep(0, length(year.t))
  # crisis.temp[match(years.crisis, year.t)] <- crisis.t[match(years.crisis, year.t)]
  # crisis.t <- crisis.temp
  
  # add WHO estimates of crisis-related mortality
  propadj.t <- NULL
  if (is.null(u.t)) 
    u.t <- rep(0, length(year.t))
  u.unadj.t <- u.t
  if (operation == "+") {
    if(as.character(iso)!="JPN"){ # DJS edit 'if(as.character(iso)!="JPN")' is ad hoc of dealing with Japan's two seperate crisis without interpolation
      # for(traj in 1:n.adj.traj){
        # if(traj==1){
          u.t <- u.unadj.t +
            ifelse(year.t >= min(years.crisis) & year.t <= max(years.crisis), crisis.t, 0)
        # } else {
        #   u.t <- rbind(u.t, u.unadj.t +
        #     ifelse(year.t >= min(years.crisis) & year.t <= max(years.crisis), crisis.t[traj,], 0))
        # } # if/else traj==1
      # } # traj loop
    } else { # JPN
      # for(traj in 1:n.adj.traj){
        # if(traj==1){
          u.t <- u.unadj.t + ifelse(year.t==min(years.crisis), crisis.t, 0) + ifelse(year.t==max(years.crisis), crisis.t, 0)
        # } else {
          # u.t <- rbind(u.t, u.unadj.t + ifelse(year.t==min(years.crisis), crisis.t[traj,], 0) + ifelse(year.t==max(years.crisis), crisis.t[traj], 0))
        # } # if/else traj==1
      # } # traj loop
    } # if/else JPN
  } else if (operation == "-") {
    if(as.character(iso)!="JPN"){
      # for(traj in 1:n.adj.traj){
        # if(traj==1){
          u.t <- u.unadj.t -
            ifelse(year.t >= min(years.crisis) & year.t <= max(years.crisis), crisis.t[traj,], 0)
      #   } else {
      #     u.t <- rbind(u.t, u.unadj.t -
      #                    ifelse(year.t >= min(years.crisis) & year.t <= max(years.crisis), crisis.t[traj,], 0))
      #   } # if/else traj==1
      # } # traj loop
    } else { # JPN
      # for(traj in 1:n.adj.traj){
      #   if(traj==1){
          u.t <- u.unadj.t - ifelse(year.t==min(years.crisis), crisis.t[traj,], 0) - ifelse(year.t==max(years.crisis), crisis.t[traj], 0)
      #   } else {
      #     u.t <- rbind(u.t, u.unadj.t - ifelse(year.t==min(years.crisis), crisis.t[traj,], 0) - ifelse(year.t==max(years.crisis), crisis.t[traj], 0))
      #   } # if/else traj==1
      # } # traj loop
    } # if/else JPN
  } else {
    cat("Operation ", operation, " is not valid.\n")
    return()
  }
  # deprecated as of 5 Jul 2013
  ## add WHO estimates of crisis-related mortality
  #propadj.t <- NULL
  #if (is.null(u.t)) 
  #  u.t <- rep(0, length(year.t))  
  #u.unadj.t <- u.t
  #for (t in 1:length(year.t)) {
  #  if (operation == "+") {
  #    u.t[t] <- u.unadj.t[t] +
  #      ifelse(sum(data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]) == 1,
  #             data.adj$add.adj[data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]], 0)
  #  } else if (operation == "-") {
  #    u.t[t] <- u.unadj.t[t] -
  #      ifelse(sum(data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]) == 1,
  #             data.adj$add.adj[data.adj$countrycode.adj == iso & data.adj$year.adj == year.t[t]], 0)
  #  } else {
  #    cat("Operation ", operation, " is not valid.\n")
  #    return()
  #  }
  #}
  # if vector of estimates not provided, proportion added to median estimate not calculated
  if (sum(u.unadj.t, na.rm = TRUE) > 0) {
    # propadj.t <- t(t(u.t) / u.unadj.t)
    propadj.t <- u.t/u.unadj.t
  } else {
    propadj.t <- NULL
  }
  ##value<< List with
  return(list(u.t = u.t, ##<< Vector of crisis-adjusted estimates
              propadj.t = propadj.t ##<< Vector of crisis adjustment factors
  ))
}
