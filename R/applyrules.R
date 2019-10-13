#----------------------------------------------------------------------
# applyrules.R
# Jin Rou New, 2013-2014
#----------------------------------------------------------------------
ApplyRules <- function(
  run.type = "global", ##<< "global" or "country" or "combined". No tweaks applied for global run.
  data.cmeinfo, ##<< Data file.
  country.B3info.file, ##<< File path to a .csv file containing country B3 information.
  country.info.file, ##<< File path to a .csv file containing country information.
  livebirths.file, ##<< File path to a .csv file containing WPP live birth information.
  year.lastestimatepublished, ##<< Year for which live birth data is used for rule 3.
  isos.to.exclude = NULL, ##<< Vector of 3-character ISO country codes of country data to exclude. Default: Exclude none.
  output.dir = NULL ##<< Output directory for \code{country.estimate.info}.
) {
  if (is.null(output.dir))
    dir.create(file.path(getwd(), "output"), showWarnings = FALSE) 
  # read in files
  info <- read.csv(country.B3info.file, header = T, stringsAsFactors = F, strip.white = T)
  C <- length(info$iso.c)
  data.lb <- read.csv(livebirths.file, header = T, stringsAsFactors = F)
  # Rules 2 and 4 do not apply to countries with special VR data (i.e. VR data with bias or completeness assumptions)
  iso.hasspecialvr <- unique(data.cmeinfo$Country.Code[data.cmeinfo$Series.Type == "VR" & 
    ( (!is.na(data.cmeinfo$Set.As.Minimum) & data.cmeinfo$Set.As.Minimum == 1) | 
    (!is.na(data.cmeinfo$Has.Bias) & data.cmeinfo$Has.Bias == 1) )]) # change JR, 20140603
  if (run.type == "global") # no non-VR bias for global run # change JR, 20140623
    data.cmeinfo$Has.Bias[data.cmeinfo$Series.Category != "VR"] <- NA
  #----------------------------------------------------------------------
  # rule 1: setLevelBiasAtPrior for indirect series with series year before 1975
  if (run.type == "global") {
    select.setLevelBiasAtPrior <- rep(FALSE, nrow(data.cmeinfo))
    data.cmeinfo$Fix.Series.Level.Bias[] <- NA
    # output edited data.cmeinfo
    write.csv(data.cmeinfo, file = file.path(output.dir, "data_CMEInfo.csv"), row.names = F, na = "", fileEncoding = "latin1")
    Series.With.Fixed.Level.Bias <- rep("", C)
    Rule.1 <- rep(0, nrow(info))
  } else {
    select.setLevelBiasAtPrior <- data.cmeinfo$Series.Type == "Indirect" & data.cmeinfo$End.date.of.Survey < 1975 &
      data.cmeinfo$Inclusion == 1 & !is.element(data.cmeinfo$Country.Code, isos.to.exclude) &
      is.na(data.cmeinfo$Set.As.Minimum)
    iso.rule1 <- unique(data.cmeinfo$Country.Code[select.setLevelBiasAtPrior])
    select.warning <- !is.na(data.cmeinfo$Fix.Series.Level.Bias) & 
      data.cmeinfo$Fix.Series.Level.Bias == 1 & !select.setLevelBiasAtPrior  
    if (any(select.warning)) {
      cat(paste0("Warning: The following series are not indirect series with series year before 1975 ",
                 "but have level bias set at prior!\n")) 
      print(unique(paste(data.cmeinfo$Country.Name, data.cmeinfo$Series.Name, 
                         data.cmeinfo$Series.Year)[select.warning]))
    }
    data.cmeinfo$Fix.Series.Level.Bias[select.setLevelBiasAtPrior] <- 1
    # output edited data.cmeinfo
    write.csv(data.cmeinfo, file = file.path(output.dir, "data_CMEInfo.csv"), row.names = F, na = "", fileEncoding = "latin1")
    Series.With.Fixed.Level.Bias <- rep("", C)
    for (c in 1:C) {
      Series.With.Fixed.Level.Bias[c] <- paste(unique(paste(data.cmeinfo$Series.Name, 
                                                            data.cmeinfo$Series.Year)[select.setLevelBiasAtPrior & 
                                                                                        data.cmeinfo$Country.Code == info$iso.c[c]]), 
                                               collapse = ", ")
    }
    Rule.1 <- ifelse(is.element(info$iso.c, iso.rule1), 1, 0)
  }
  #----------------------------------------------------------------------
  # rule 2: useWorldsigmau for countries with both VR and non-VR data
  if (run.type == "global") {
    Rule.2 <- rep(0, nrow(info))
  } else {
    check.vrandnonvrdata.by.country <- ddply(data.cmeinfo[data.cmeinfo$Inclusion == 1 & 
                                                            !is.element(data.cmeinfo$Country.Code, isos.to.exclude), ], 
                                             .(Country.Name, Country.Code), summarise, 
                                             hasVRandnonVRdata = ((sum(Series.Category == "VR") != 
                                                                     length(Series.Category)) &
                                                                    (sum(Series.Category == "VR") != 0)))
    iso.rule2 <- check.vrandnonvrdata.by.country$Country.Code[check.vrandnonvrdata.by.country$hasVRandnonVRdata == T]
    Rule.2 <- ifelse(is.element(info$iso.c, c(iso.rule2, "URY")) & # adhoc change JR, 20140530 
                       !is.element(info$iso.c, iso.hasspecialvr), # change JR, 24 Jun 
                     1, 0)
  }
  #----------------------------------------------------------------------
  # rule 3: useWorldsigmau for countries with live births < 10,000 in year.lastestimatepublished based on WPP 2012
  if (run.type == "global") {
    Rule.3 <- rep(0, nrow(info))
  } else {
    if (is.null(data.lb$iso3code)) {
      data.lb$iso3code <- GetCountryInfo(data.frame(Country.ISO = data.lb$uncode), 
                                         country.info.file = country.info.file)$Country.Code
      # check that only regions and countries not included in estimation do not have a matched ISO country code
      if (sum(is.na(data.lb$iso3code)) > 0)
        cat(paste0("No matched ISO country code for the following countries/regions: ",
                   paste(unique(data.lb$country[is.na(data.lb$iso3code)]), collapse = ", ")))
    }
    select.lb <- !is.na(data.lb$iso3code) & data.lb$year == year.lastestimatepublished & data.lb$lb < 10000
    iso.rule3 <- data.lb$iso3code[select.lb]
    Rule.3 <- ifelse(is.element(info$iso.c, iso.rule3), # change JR, 20140603
                     1, 0)
  }
  #----------------------------------------------------------------------
  # rule 4: useWorldsigmau for countries with gaps of > 5 years for VR data
  if (run.type == "global") {
    Rule.4 <- rep(0, nrow(info))
  } else {
    if (sum(data.cmeinfo$Inclusion == 1 & data.cmeinfo$Series.Category == "VR" & 
              !is.element(data.cmeinfo$Country.Code, isos.to.exclude)) > 0) {
      check.gapsinvrdata.by.series <- ddply(data.cmeinfo[data.cmeinfo$Inclusion == 1 & 
                                                           data.cmeinfo$Series.Category == "VR" &
                                                           !is.element(data.cmeinfo$Country.Code, isos.to.exclude), ], 
                                            .(Country.Name, Country.Code, Series.Name, Series.Year), summarise, 
                                            nyearsgapinvrdata = ifelse(length(Reference.Date) < 2, NA,
                                                                       max(diff(sort(Reference.Date)))))
      iso.rule4 <- check.gapsinvrdata.by.series$Country.Code[!is.na(check.gapsinvrdata.by.series$nyearsgapinvrdata) &
                                                               check.gapsinvrdata.by.series$nyearsgapinvrdata > 5]
    } else {
      iso.rule4 <- NULL
    }
    Rule.4 <- ifelse(is.element(info$iso.c, iso.rule4) & !is.element(info$iso.c, iso.hasspecialvr), # change JR, 24 Jun 
                     1, 0)
  }
  # output country.estimate.info
  country.estimate.info <- data.frame(Country.Name = info$name.c, ISO.Code = info$iso.c, 
                                      Rule.1 = Rule.1, Series.With.Fixed.Level.Bias = Series.With.Fixed.Level.Bias,
                                      Rule.2 = Rule.2, Rule.3 = Rule.3, Rule.4 = Rule.4)[!is.element(info$iso.c, 
                                                                                        isos.to.exclude), ]
  country.estimate.info <- country.estimate.info[country.estimate.info$ISO.Code %in% data.cmeinfo$Country.Code, ]
  write.csv(country.estimate.info, file = file.path(output.dir, "country.estimate.info.csv"), row.names = F, fileEncoding = "latin1")
  if (any(country.estimate.info$Rule.2 == 1 | country.estimate.info$Rule.3 == 1 |
            country.estimate.info$Rule.4 == 1)) {
    isos.for.global.smoothing <- as.character(country.estimate.info$ISO.Code[country.estimate.info$Rule.2 == 1 | 
                                                                               country.estimate.info$Rule.3 == 1 |
                                                                               country.estimate.info$Rule.4 == 1])
  } else {
    isos.for.global.smoothing <- NULL
  }
  ##value<<
  return(isos.for.global.smoothing = isos.for.global.smoothing)
}
