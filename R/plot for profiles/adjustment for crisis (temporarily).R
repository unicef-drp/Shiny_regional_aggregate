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
  if (length(years.crisis) > 1 ) {           #####when there are multiple crises in a country
    if(unique(diff(years.crisis))==1){                #####when the differences between all consecutive values all equal to 1 which means the values are continuous
    crisis <- approx(x = years.crisis,                #####linear interpolation
                     y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)],
                     xout = roundoff(year.i, 1), method = "linear", rule = 2)
    crisis.i <- crisis$y
    u.subtracted.i <- u.i - ifelse(year.i >= min(years.crisis) & year.i <= max(years.crisis), crisis.i, 0)
    } else {
      u.subtracted.i <- u.i                           #####when the differences between all consecutive values are not all equal to 1 which means at least some of the values are not continuous
      years.crisis. <- c(0, cumsum( diff(years.crisis)-1 ) )                 ######discriminate the continuity of crisis years
      continuouscrisis=unique(years.crisis.[duplicated(years.crisis.)])     ####get the location of continuous crises years 
      if(!identical(continuouscrisis, numeric(0))){           #######judge whether continuous crises years exist or not
      for(i in continuouscrisis){
        location=which(i==years.crisis.)
        years.crises=years.crisis[location]                   ####get the years which have continuous crises
        crisis <- approx(x = years.crises,  
                         y = data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)][location],
                         xout = roundoff(year.i, 1), method = "linear", rule = 2)               #####linear interpolation within these continuous crises years
        crisis.i <- crisis$y
        u.subtracted.i <- u.subtracted.i - ifelse(year.i >= min(years.crises) & year.i <= max(years.crises), crisis.i, 0)
      }
      }
      discretecrisis=setdiff(years.crisis.,continuouscrisis)         ####get the location of discrete crises years
      if(!identical(discretecrisis, numeric(0))){                    #######judge whether discrete crises years exist or not
       for(j in discretecrisis){
         singlecrisislocation = which(j==years.crisis.)
         years.singlecrisis = years.crisis[singlecrisislocation]        ####get the years which have discrete crises
         crisis.i <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(years.singlecrisis)), data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)][singlecrisislocation], 0)
         u.subtracted.i <- u.subtracted.i - crisis.i
       }
      }
    }
  } else {                 ######when there is only a single crisis in a country
      crisis.i <- ifelse(as.character(roundoffmidpoint(year.i)) == as.character(roundoffmidpoint(years.crisis)), data.adj$add.adj[data.adj$countrycode.adj == as.character(iso)], 0)
      u.subtracted.i <- u.i - crisis.i
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