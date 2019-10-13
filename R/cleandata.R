#----------------------------------------------------------------------
# cleanddata.R
#----------------------------------------------------------------------
CleanData <- function(
  data, ## Data frame.
  output.dir, ## Output directory for data summary.
  year.current,
  fit.B2.model = FALSE
) {
  # Define variables
  N <- length(data[, 1])
  # i is obs index
  country.i <- StandardiseCountryNames(data$Country.Name)
  name.c <- unique(country.i)
  C <- length(name.c) # 179
  countrycode.i <- data$Country.Code
  year.i <- data$Reference.Date
  u.i <- data$Estimates
  se.i <- data$Standard.Error.of.Estimates
  seriescategory.i <- data$Series.Category
  seriesyear.i <- data$Series.Year
  sourcedate.i <- data$End.date.of.Survey
  source.i <- data$Series.Name
  method.i <- ifelse(data$Series.Type == "Life Table", "Life Table",
                     ifelse(data$Data.Collection.Method == "Household Deaths", "Household Deaths",
                            ifelse(data$Series.Type == "Indirect", "Indirect", 
                                   "Direct"))) # Direct: Direct+VR
  period.i <- data$Age.Group.of.Women
  sourcename.i <- paste(country.i, source.i, seriesyear.i, sep = "_")
  sourceID.i <- paste(sourcename.i, method.i)
  interval.i <- data$Interval
  included.i <- data$Inclusion
  fixserieslevelbias.i <- ifelse(is.na(data$Fix.Series.Level.Bias), 0,
                                 data$Fix.Series.Level.Bias)
  if (is.null(data$Has.Bias)) # for back-compatability
    data$Has.Bias <- rep(NA, nrow(data))
  hasbias.i <- ifelse(is.na(data$Has.Bias), 0, 1)
  setasminimum.i <- ifelse(is.na(data$Set.As.Minimum), 0, data$Set.As.Minimum)
  minimumcompleteness.i <- data$Minimum.Level.of.Completeness
  if (is.null(data$Maximum.Level.of.Completeness)) # for back-compatability
    data$Maximum.Level.of.Completeness <- rep(NA, nrow(data))
  maximumcompleteness.i <- data$Maximum.Level.of.Completeness
  # extra columns
  seriestype.i <- data$Series.Type
  yearfrom.i <- data$Year.From
  yearto.i <- data$Year.To
  
  # categorise sources into types
  sourcetype.i <- ifelse(data$Series.Type == "VR", "VR",
                         ifelse(is.element(seriescategory.i, c("DHS")), "DHS",
                                ifelse(is.element(seriescategory.i, c("Interim DHS", "Special DHS", "AIS", "MIS", "NDHS", "WFS")), "Other DHS",
                                       ifelse(seriescategory.i == "MICS", "MICS",
                                              ifelse(seriescategory.i == "Census", "Census",
                                                     "Others"))))) # RHS, PAP, NMICS, Panel in Others!
  sourcetype.i <- ifelse(is.element(method.i, c("Household Deaths", "Life Table")), 
                         "Others", sourcetype.i)
  # change JR, 20140530: MICS Indirect AOM to be classified as Others Indirect
  sourcetype.i <- ifelse(seriescategory.i == "MICS" & method.i == "Indirect" &
                         !(!is.na(data$Indirect.method.based.on.TSFB) & data$Indirect.method.based.on.TSFB == 1),
                         "Others", sourcetype.i)

  # in B2 model, there is no bias adjustment, so all data is treated as VR
  if (fit.B2.model)
    sourcetype.i[] <- "VR" 
  
  with.se.i <- ifelse(is.na(se.i), "", "with SE")
  data.summary <- data.frame(sourcetype.i, method.i, with.se.i)
  data.summary.output <- ddply(data.summary, .(sourcetype.i, method.i, with.se.i), 
                               summarise, nobs = length(sourcetype.i))
  write.csv(data.summary.output, file = file.path(output.dir, "Overview of database.csv"), row.names = FALSE, fileEncoding = "latin1")
  cat(paste0("Database summary has been saved to ", output.dir, ".\n"))
  
  # for Life Table category, or Household Deaths category with series year = 0, 
  # input observation year as series year
  seriesyear.i <- ifelse(method.i == "Life Table" | 
                           (method.i == "Household Deaths" & seriesyear.i == 0), 
                         floor(year.i), seriesyear.i)
  
  # redo sourcename.i and sourceID.i to take into account change in seriesyear.i
  # so that life tables entries are recognised as distinct series
  #sourcename.i <- paste(country.i, data$Series.Name, seriesyear.i, sep = "_")#LH 2017 change to display as one
  #sourceID.i <- paste(sourcename.i, method.i)#LH 2017 change to display as one
  
  # standardise source names
  source.i[source.i == "Census Preliminary"] = "Census"
  source.i[grepl("Afghanistan Mortality Survey", source.i, ignore.case = TRUE)] = "Afghanistan Mortality Survey"
  
  # survey year
  # note: later we fix the source date if it turns out to be after the ref date using recall period
  surveyyear.i <- ifelse(sourcetype.i == "VR" & sourcedate.i == 0, year.i,
                         ifelse(is.element(method.i, c("Life Table", "Household Deaths"))
                                & sourcedate.i == 0, year.i,
                                sourcedate.i))
  
  # use assumption that for indirect series with missing survey date, 
  # the 25-29 AOM obs is 3.5 years before survey date
  #surveylabels = unique(sourceID.i[is.na(surveyyear.i) & method.i=="Indirect"])
  #for(i in 1:length(surveylabels)) {
  #  surveyyear.i[sourceID.i==surveylabels[i] & is.na(surveyyear.i)] = rep(year.i[sourceID.i==surveylabels[i] & period.i=="25-29"]+3.5, 
  #                                                                        each = length(surveyyear.i[sourceID.i==surveylabels[i] & is.na(surveyyear.i)]))
  #}
  
  # make sure no survey date ends up in the future!
  sum(surveyyear.i > year.current) # 0
  surveyyear.i <- ifelse(surveyyear.i > year.current, year.current, surveyyear.i)
  
  # fix some survey dates where survey date is before ref date
  (surveyyear.i - year.i)[surveyyear.i < year.i] # check that differences are not big
  data.frame(sourcename.i, sourcetype.i, method.i, # if big, they are VR or LT/HH deaths
             surveyyear.i, year.i)[surveyyear.i < year.i & 
                                     !(sourcetype.i == "VR" | 
                                         is.element(method.i, c("Life Table", "Household Deaths"))), ] 
  sourceIDs <- unique(sourcename.i[surveyyear.i < year.i])
  for (sourceID in sourceIDs) {
    select.obs <- seq(1,N)[sourcename.i == sourceID]
    surveyyear.i[select.obs] <- max(year.i[select.obs]) # change JR, 13 Mar
  }
  
  # and make sure survey dates are consistent for all obs from one non-VR & non-Others source
  surveyyear.raw.i <- surveyyear.i
  sourceIDs <- unique(sourcename.i[sourcetype.i != "VR" &
                                     !is.element(method.i, c("Life Table", "Household Deaths"))]) # change JR, 17 May
  for (sourceID in sourceIDs) {
    select.obs <- seq(1,N)[sourcename.i == sourceID]
    surveyyear.i[select.obs] <- max(surveyyear.i[select.obs], na.rm = T) 
  }
  sum(abs(surveyyear.i - surveyyear.raw.i) != 0) # should be 0
  # unique(sourcename.i[abs(surveyyear.i - surveyyear.raw.i) != 0])
  # unique(paste(sourcename.i, method.i, surveyyear.i, surveyyear.raw.i)[abs(surveyyear.i - surveyyear.raw.i) != 0])
  
  # change LA May 17, 2012: change all VR survey year to obs years (VR for Russia ends up with survey year of 2011) 
  sum(abs(surveyyear.i-year.i)[sourcetype.i == "VR"] != 0) # 0
  surveyyear.i[sourcetype.i=="VR"] <- year.i[sourcetype.i=="VR"]
  
  # recall period
  recallperiod.i <- surveyyear.i - year.i
  # sourceID.i[recallperiod.i < 0] # OK for HH deaths
  
  data.clean <- data.frame(country.i = country.i,
                           countrycode.i = countrycode.i,
                           year.i = year.i,
                           u.i = u.i,
                           se.i = se.i,
                           seriesyear.i = seriesyear.i,
                           surveyyear.i = surveyyear.i,
                           source.i = source.i,
                           sourceID.i = sourceID.i,
                           sourcetype.i = sourcetype.i,
                           seriescategory.i = seriescategory.i,
                           recallperiod.i = recallperiod.i,
                           method.i = method.i,
                           period.i = period.i,
                           interval.i = interval.i,
                           included.i = included.i,
                           sourcename.i = sourcename.i,
                           sourcedate.i = sourcedate.i,
                           setasminimum.i = setasminimum.i,
                           minimumcompleteness.i = minimumcompleteness.i,
                           maximumcompleteness.i = maximumcompleteness.i,
                           fixserieslevelbias.i = fixserieslevelbias.i,
                           hasbias.i = hasbias.i,
                           seriestype.i = seriestype.i,
                           yearfrom.i = yearfrom.i,
                           yearto.i = yearto.i)
  return(data.clean) ##<< Return processed data frame.
}
