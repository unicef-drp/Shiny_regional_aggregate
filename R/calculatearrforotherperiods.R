#----------------------------------------------------------------------
# calculatearrforotherperiods.R
#----------------------------------------------------------------------
CalculateARRForOtherPeriods <- function( # Calculate and output ARR median/UIs for other periods
  output.dir, ##<< Output directory where previous results are saved and to save all results.
  year1.t = 1990.5, ##<< Vector of first year used for ARR calculation.
  year2.t = 2000.5, ##<< Vector of second year used for ARR calculation.
  regiontype.select = "UNICEF", ##<< Output regional aggregates for which region type? 
  ## Select from the options of "UNICEF", "MDG", "WHO", "WB", "UNPD", "OIC",  
  ## "Countdown", "ECAAfrica", "AU", "Fragile2013", "Fragile2014", "USAID"
  country.info.CME.file = NULL, ##<< If \code{NULL}, country info included in package is used. 
  quantity = "ARR", ##<< Quantity calculated (used for file name).
  percentiles = c(0.05, 0.5, 0.95), ##<< Vector of percentiles.
  ndigits = 1, ##<< Number of decimal places to use for analysis (e.g. to calcalate ARR, decline).
  test = FALSE ##<< Use a subset of 10 trajectories to test function.
) {
  source("R/chooseregion.R")
  output.dir.samplescombined <- file.path(output.dir, "samples_combined")
  if (is.null(country.info.CME.file))
    country.info.CME.file <- file.path("input", "country.info.CME.csv")
  
  # read in data
  country.info <- read.csv(file = country.info.CME.file, header = T, stringsAsFactors = F, 
                           strip.white = T)
  country.info <- country.info[, !grepl("pop", colnames(country.info))]
    
  load(file.path(output.dir.samplescombined, "info.rda"))
  cat(paste0("Information about the aggregates have been loaded from ", output.dir.samplescombined, ".\n"))
  iso.c <- info$iso.c
  est.years <- info$est.years
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, paste0(regiontype.select, "Region_u5mr.rtj.rda")))
  load(file.path(output.dir.samplescombined, "u5mr.wtj.rda"))
  cat(paste0("Country, regional and world trajectories have been loaded from ", 
             output.dir.samplescombined, ".\n"))
  # test
  if (test) {
    u5mr.ctj <- u5mr.ctj[, , 1:10]
    u5mr.rtj <- u5mr.rtj[, , 1:10]
    u5mr.wtj <- u5mr.wtj[, , 1:10]
  }
  
  nsim <- dim(u5mr.ctj)[3]
  # round off to 1 d.p. before calculation (for median only)
  if (nsim == 1) {
    u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
    u5mr.rtj <- roundoff(u5mr.rtj, digits = ndigits)
    u5mr.wtj <- roundoff(u5mr.wtj, digits = ndigits)
  }
  
  # reformat data (same country order)
  u5mr.ctj <- u5mr.ctj[match(country.info$ISO3Code, iso.c), , ]
  
  # calculate ARR
  country.RoDs.ui <- colnames.country.RoDs.ui <- NULL
  for (t in 1:length(year1.t)) {
    year1 <- year1.t[t]
    year2 <- year2.t[t]
    ARR.year1.year2.cj <- as.matrix(apply(u5mr.ctj, 1, CalculateARR, 
                                          years = est.years, year.start = year1, year.end = year2))
    if (nsim != 1)
      ARR.year1.year2.cj <- t(ARR.year1.year2.cj)
    # save trajectories
    eval(parse(text = paste0("ARR.", year1-0.5, ".", year2-0.5, ".cj", " <- ARR.year1.year2.cj")))
    eval(parse(text = paste0("save(ARR.", year1-0.5, ".", year2-0.5, ".cj, file = file.path(output.dir.samplescombined, \"ARR.", 
                             year1-0.5, ".", year2-0.5, ".cj\"))")))
    ARR.year1.year2.ui <- apply(ARR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
    country.RoDs.ui <- cbind(country.RoDs.ui, t(ARR.year1.year2.ui))
    colnames.country.RoDs.ui <- c(colnames.country.RoDs.ui,
                                  paste0("ARR ", year1-0.5, "-", year2-0.5, " lower bound"), 
                                  paste0("ARR ", year1-0.5, "-", year2-0.5, " median"), 
                                  paste0("ARR ", year1-0.5, "-", year2-0.5, " upper bound"))
  }
  # output to .csv
  colnames(country.RoDs.ui) <- colnames.country.RoDs.ui
  country.RoDs.ui <- cbind(country.info, country.RoDs.ui)
  if (nsim == 1)
    country.RoDs.ui <- country.RoDs.ui[, !grepl("bound", colnames(country.RoDs.ui))]
  write.csv(country.RoDs.ui, 
            file = file.path(output.dir, paste0("Rates of Decline ", quantity, "_Country Summary.csv")),
            row.names = FALSE, na = "")
  cat(paste0("Country results saved to ", output.dir, "\n"))
  #----------------------------------------------------------------------
  # calculate ARR for region
  filename <- paste0(regiontype.select, "Region")
  region.RoDs.ui <- global.RoDs.ui <- NULL
  for (t in 1:length(year1.t)) {
    year1 <- year1.t[t]
    year2 <- year2.t[t]
    ARR.year1.year2.rj <- as.matrix(apply(u5mr.rtj, 1, CalculateARR, 
                                          years = est.years, year.start = year1, year.end = year2))
    if (nsim != 1)
      ARR.year1.year2.rj <- t(ARR.year1.year2.rj)
    ARR.year1.year2.ui <- apply(ARR.year1.year2.rj, 1, quantile, probs = percentiles, na.rm = T)  
    region.RoDs.ui <- cbind(region.RoDs.ui, t(ARR.year1.year2.ui))
    # calculate ARR for world
    ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years, 
                                      year.start = year1, year.end = year2)
    ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
    global.RoDs.ui <- c(global.RoDs.ui, ARR.year1.year2.ui)
    # save trajectories
    ARR.year1.year2.rj.final <- rbind(ARR.year1.year2.rj, ARR.year1.year2.j)
    eval(parse(text = paste0("ARR.", year1-0.5, ".", year2-0.5, ".rj", " <- ARR.year1.year2.rj.final")))
    eval(parse(text = paste0("save(ARR.", year1-0.5, ".", year2-0.5, 
                             ".rj, file = file.path(output.dir.samplescombined, \"", filename, "_ARR.", 
                             year1-0.5, ".", year2-0.5, ".rj\"))")))
  }
  # combine ARR UIs
  region.RoDs.ui <- rbind(region.RoDs.ui, global.RoDs.ui)
  colnames(region.RoDs.ui) <- colnames.country.RoDs.ui
  if (regiontype.select == "UNICEF") {
    regiontypes <- UNICEFRegionAll
  } else if (regiontype.select == "MDG") {
    regiontypes <- MDGRegionAll
  } else if (regiontype.select == "WHO") {
    regiontypes = WHORegionAll
  } else if (regiontype.select == "WB") {
    regiontypes <- WBRegionAll
  } else if (regiontype.select == "UNPD") {
    regiontypes <- UNPDRegionAll
  } else if (regiontype.select == "OIC") {
    regiontypes <- OICRegionAll
  } else if (regiontype.select == "Countdown") {
    regiontypes <- CountdownAll
  } else if (regiontype.select == "ECAAfrica") {
    regiontypes <- ECAAfricaRegionAll
  } else if (regiontype.select == "AU") {
    regiontypes <- AURegionAll
  } else if (regiontype.select == "Fragile2013") {
    regiontypes <- Fragile2013All
  } else if (regiontype.select == "Fragile2014") {
    regiontypes <- Fragile2014All
  } else if (regiontype.select == "USAID") {
    regiontypes <- USAIDAll
  }
  region.RoDs <- data.frame(Region = c(regiontypes, "World"), region.RoDs.ui)
  if (nsim == 1)
    region.RoDs <- region.RoDs[, !grepl("bound", colnames(region.RoDs))]
  write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline ", quantity, "_", filename, ".csv")),
            row.names = F, na = "")
  cat(paste0("Output generated for ", filename, " and saved to ", output.dir, ".\n"))
}
