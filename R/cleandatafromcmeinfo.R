#----------------------------------------------------------------------
# cleandatafromcmeinfo.R
# Jin Rou New and Leontine Alkema, 2012-2013
#----------------------------------------------------------------------
CleanDataFromCMEInfo <- function(# Reads in and clean data file from CME Info.
  data.cmeinfo.file, ##<< Data from CME Info.
  country.B3info.file, ##<< File path of country B3 information from CME Info.
  hiv.file, ##<< File path to a .csv with UNAIDS HIV estimates
  adj.file, ##<< File path to a .csv with WHO crisis mortality adjustment data # change JR, 20150515
  runname = "test", ##<< Run name.
  iso.select = NULL, ##<< Vector of ISO country codes. If \code{NULL}, data for all countries.
  ## in database will be read in and cleaned. 
  output.dir = NULL, ##<< Directory where cleaned data file will be saved. if \code{NULL}, defaults to
  ## directory \code{output/runname}.
  indicator.type, ##<< "U5MR" or "IMR"?
  runname.U5MR, ##<< Character indicating runname of B3 U5MR run required for estimating IMR. # change JR, 28 May
  include.HIV.countries = TRUE, ##<< Logical value indicating if observations/series of the 17 HIV countries should be read in and cleaned.
  includeExcluded = FALSE, ##<< Logical value indicating whether to also read in excluded observations/series. # change JR, 31 May
  is.validation = FALSE, ##<< Logical value indicating whether this is a validation run. # change JR, 28 Jun
  fit.B2.model = FALSE, ##<< Logical value indicating whether or not to fit B2 model (without data model). # change JR, 5 Jul
  year.current = 2015.5 ##<< Current year. No series date can be after this date.
) {
  data.cmeinfo <- read.csv(file = data.cmeinfo.file, header = T, stringsAsFactors = F,encoding = "UTF-8")
  data.hiv <- read.csv(file = hiv.file, header = T, stringsAsFactors = F)
  data.adj <- read.csv(file = adj.file, header = T, stringsAsFactors = F) # change JR, 20150515
  # exclude Visible = 0 data
  data.cmeinfo <- data.cmeinfo[data.cmeinfo$Visible == 1, ]
  info <- read.csv(file = country.B3info.file,
                   header = T, as.is = T, stringsAsFactors = F, strip.white = T)
  if (is.null(output.dir)) {
    dir.create(file.path(getwd(), "output"), showWarnings = FALSE) 
    output.dir <- paste0(getwd(), "/output/", runname, "/")
  }
  dir.create(output.dir, showWarnings = FALSE) 
  iso.hiv <- info$iso.c[info$iso.c %in% data.hiv$countrycode.hiv[data.hiv$unaids.hiv != 0]]
  iso.crisis <- info$iso.c[info$iso.c %in% data.adj$countrycode.adj[data.adj$add.adj != 0]] # change JR, 20150515
  #----------------------------------------------------------------------
  if (indicator.type == "IMR") {
    load(file.path("output", runname.U5MR, "res.U5MR.rda"))
    colnames(res.U5MR$res.ct) <- res.U5MR$year.t
    results.U5MR.wide <- data.frame(Country.Code = res.U5MR$iso.c, res.U5MR$res.ct)
    results.U5MR <- melt(results.U5MR.wide)
    colnames(results.U5MR)[colnames(results.U5MR) == "variable"] <- "Year"
    colnames(results.U5MR)[colnames(results.U5MR) == "value"] <- "Estimate"
    results.U5MR$Year <- gsub("X", "", results.U5MR$Year)
    country.summary <- ddply(results.U5MR[!is.na(results.U5MR$Estimate), ], .(Country.Code),
                             summarise, Min.Observation.Year = min(Year))
    iso.c <- unique(data.cmeinfo$Country.Code)
    name.c <- unique(data.cmeinfo$Country.Name)
    n.exclude <- 0
    for (c in 1:length(iso.c)) {
      select.exclude <- data.cmeinfo$Inclusion[data.cmeinfo$Country.Code == iso.c[c]] == 1 &
        (data.cmeinfo$Reference.Date[data.cmeinfo$Country.Code == iso.c[c]] < 
           country.summary$Min.Observation.Year[country.summary$Country.Code == iso.c[c]])
      if (sum(select.exclude) > 0) {
        cat(paste0(name.c[c], " has minimum U5MR observation year ", 
                   country.summary$Min.Observation.Year[country.summary$Country.Code == iso.c[c]], ".\n"))
        cat(paste0("Reference dates excluded in database are: ", 
                   paste(data.cmeinfo$Reference.Date[data.cmeinfo$Country.Code ==
                                                       iso.c[c]][select.exclude], collapse = ", "), ".\n"))
        data.cmeinfo$Inclusion[data.cmeinfo$Country.Code == iso.c[c]][select.exclude] <- 0
        n.exclude <- n.exclude + sum(select.exclude)
      }
    }
    if (n.exclude > 0)
      cat(paste0(n.exclude, " observations with observation year earlier than the minimum U5MR observation year dropped.\n"))
  }
  #----------------------------------------------------------------------
  select.estimatezero <- select.olddata <- select.sps <- select.excluded <- 
    select.countrynotselect <- select.hiv <- select.crisis <- # change JR, 20150515
    rep(FALSE, nrow(data.cmeinfo))
  select.estimatezero <- data.cmeinfo$Estimates == 0 & data.cmeinfo$Inclusion == 1  
  if (any(select.estimatezero))
    cat(paste0(sum(select.estimatezero), " observations with Estimate = 0 and Inclusion = 1 dropped.\n"))
  if (any(select.estimatezero & data.cmeinfo$Inclusion == 1))
    cat(paste0("Warning: Of the above observations, ", sum(select.estimatezero & data.cmeinfo$Inclusion == 1), 
               " observations with Estimate = 0 and Inclusion = 1 were dropped.\n"))
  select.olddata <- !is.na(data.cmeinfo$Exclusion.Old.Data)
  if (any(select.olddata))
    cat(paste0(sum(select.olddata), " outdated observations (!is.na(Exclusion.Old.Data)) dropped.\n"))
  if (any(select.olddata & data.cmeinfo$Inclusion == 1))
    cat(paste0("Warning: Of the above observations, ", sum(select.olddata & data.cmeinfo$Inclusion == 1), 
               " observations with Estimate = 0 and Inclusion = 1 were dropped.\n"))
  select.sps <- data.cmeinfo$Country.Name %in% "Sudan pre secession"
  if (any(select.sps))
    cat(paste0(sum(select.sps), " observations from Sudan pre secession dropped.\n"))
  # change JR, 20150603: check all iso.crisis if iso.select is not specified
  if (is.null(iso.select)) {
    iso.crisis.to.check <- iso.crisis
  } else {
    iso.crisis.to.check <- intersect(iso.select, iso.crisis)
  }
  # exclude data in crisis years # change JR, 20150515
  # change DJS, 20170524 -- ad hoc fix for Japan to treat the crisis as two seperate events
  if (length(iso.crisis.to.check) > 0) {
    for (iso in iso.crisis.to.check) {
      crisis.years.range <- range(data.adj$year.adj[data.adj$countrycode.adj == iso & 
                                                      data.adj$add.adj != 0])
      
      years.crisis <- data.adj$year.adj[data.adj$countrycode.adj == as.character(iso) & data.adj$add.adj != 0]
      # if any observation year is within range of crisis years, select.crisis is TRUE
      
      select.crisis[data.cmeinfo$Country.Code == iso & is.element(floor(data.cmeinfo$Reference.Date), floor(years.crisis))] <- TRUE # edit DJS 2019-03-12 to handle non-contiguous crisis, eg Japan
      # if(as.character(iso)=="JPN"){
      #   select.crisis <- select.crisis |
      #     (data.cmeinfo$Country.Code == iso &
      #        data.cmeinfo$Reference.Date == 2011.5) | 
      #     (data.cmeinfo$Country.Code == iso &
      #        data.cmeinfo$Reference.Date == 2015.5)
      # } else {
      #   select.crisis <- select.crisis |
      #     (data.cmeinfo$Country.Code == iso &
      #        data.cmeinfo$Reference.Date >= crisis.years.range[1] & 
      #        data.cmeinfo$Reference.Date <= crisis.years.range[2])
      # } # if/else JPN
    }
  }
  if (any(select.crisis)) {
    crisis.countries.years <- unique(paste(data.cmeinfo$Country.Code,
                                           data.cmeinfo$Reference.Date)[select.crisis])
    cat(paste0(length(crisis.countries.years), 
               " observations in crisis country-years will have Inclusion changed to 0: ",
               paste(crisis.countries.years, collapse = ", "), "\n"))
    data.cmeinfo$Inclusion[select.crisis] <- 0 # change JR, 20150515
  }
  if (!includeExcluded) {
    select.excluded <- data.cmeinfo$Inclusion == 0
    if (any(select.excluded))
      cat(paste0(sum(select.excluded), " observations with Inclusion = 0 dropped.\n"))
  }
  if (!is.null(iso.select)) {
    select.countrynotselect <- !(data.cmeinfo$Country.Code %in% iso.select)
    if (any(select.countrynotselect))
      cat(paste0(sum(select.countrynotselect), " observations not from ",
                 paste(data.cmeinfo$Country.Name[!select.countrynotselect], collapse = ", "),
                 " excluded.\n"))
    if (!include.HIV.countries) {
      select.hiv <- data.cmeinfo$Country.Code %in% iso.hiv
      if (any(select.hiv))
        cat(paste0(sum(select.hiv), " observations for HIV countries dropped.\n"))
    }
  } 
  exclude <- #select.estimatezero | 
    select.olddata | select.sps | 
    select.excluded | select.countrynotselect | select.hiv
  # To include all VR series except where Exclusion.Old.Data == 2 # change JR, 25 Jan
  # exclude <- exclude | (data.cmeinfo$Inclusion == 0 & data.cmeinfo$Series.Category != "VR")
  data.cmeinfo2 <- data.cmeinfo[!exclude, ]
  #----------------------------------------------------------------------
  # If global validation run or B2 run, exclude set as minimum obs, 
  # incomplete VR observations, biased VR observations
  # and do not fix series level bias and do not add non-VR relative bias
  if ((is.null(iso.select) & is.validation) | fit.B2.model) { # change JR, 20140502
    select.vrincomplete <- data.cmeinfo2$Series.Type == "VR" &
      !is.na(data.cmeinfo2$Set.As.Minimum) & data.cmeinfo2$Set.As.Minimum == 1
    select.vrbias <- data.cmeinfo2$Series.Type == "VR" & 
      !is.na(data.cmeinfo2$Has.Bias) & data.cmeinfo2$Has.Bias == 1
    exclude.for.validation <- data.cmeinfo2$Inclusion == 1 & 
      (select.vrincomplete | select.vrbias) # change JR, 20140501
    data.cmeinfo2$Inclusion[exclude.for.validation] <- 0
    if (!includeExcluded) {
      data.cmeinfo2 <- data.cmeinfo2[data.cmeinfo2$Inclusion != 0, ]
      cat(paste0(sum(exclude.for.validation), " additional VR observations that are biased or incomplete ",
                 "excluded for global validation/B2 run.\n" ))
    }
    select.fixserieslevelbias <- !is.na(data.cmeinfo2$Fix.Series.Level.Bias) & 
      data.cmeinfo2$Fix.Series.Level.Bias == 1
    if (any(select.fixserieslevelbias)) {
      cat(paste0(sum(select.fixserieslevelbias), " observations have Fix.Series.Level.Bias changed from 1 to NA ",
                 "for global validation/B2 run.\n"))
      data.cmeinfo2$Fix.Series.Level.Bias <- rep(NA, nrow(data.cmeinfo2))
    }
    select.nonvrbias <- data.cmeinfo2$Series.Type != "VR" & 
      !is.na(data.cmeinfo2$Has.Bias) & data.cmeinfo2$Has.Bias == 1
    if (any(select.nonvrbias)) {
      cat(paste0(sum(select.fixserieslevelbias), " non-VR observations have Have.Bias changed from 1 to NA ",
                 "for global validation/B2 run.\n"))
      data.cmeinfo2$Fix.Series.Level.Bias <- rep(NA, nrow(data.cmeinfo2))
      data.cmeinfo2$Has.Bias <- rep(NA, nrow(data.cmeinfo2)) # change JR, 20140501
    }
  }
  #----------------------------------------------------------------------
  data.clean <- CleanData(data = data.cmeinfo2,
                          output.dir = output.dir,
                          year.current = year.current,
                          fit.B2.model = fit.B2.model)
  
  # write data set to csv to indicate imputed values
  res <- data.clean[order(data.clean$included.i, decreasing = TRUE), ]
  write.csv(res, file = file.path(output.dir,
                                  paste0("data", indicator.type, "_imputed", 
                                         ifelse(include.HIV.countries, "_inclHIV", ""),
                                         ifelse(includeExcluded, "_inclExcluded", ""), # change JR, 31 May
                                         ".csv")), row.names = FALSE)
  
  # get results for subsequent analysis
  res <- data.clean[order(data.clean$included.i, decreasing = TRUE), 
                    !is.element(colnames(data.clean), c("sourcename.i", "sourcedate.i"))]
  # incompletevr.i # change JR, 25 Jan
  namecsv <- paste0("data", indicator.type, "_clean",
                    ifelse(include.HIV.countries, "_inclHIV", ""), 
                    ifelse(includeExcluded, "_inclExcluded", ""), # change JR, 31 May
                    ".csv")
  write.csv(res, file = file.path(output.dir, namecsv), row.names = FALSE)
  cat(paste0("Database from CME Info has been read in, processed and saved to ", output.dir, ".\n"))
  ##value<< \code{NULL}. Cleaned data files are saved to \code{output.dir}.
  return(invisible())
}
