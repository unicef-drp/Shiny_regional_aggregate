#----------------------------------------------------------------------
# calculateposteriorprobabilities.R
#----------------------------------------------------------------------
CalculatePosteriorProbabilities <- function( # Calculate and output ARR posterior probabilities for various periods
  output.dir, ##<< Output directory where previous results are saved and to save all results.
  year1.t = 1990.5, ##<< Vector of first years used for ARR calculation.
  year2.t = 2000.5, ##<< Vector of second years used for ARR calculation.
  year4.t = 2012.5, ##<< Vector of third years used for ARR calculation.
  regiontype.select = "MDG", ##<< Output regional aggregates for which region type? 
  ## Select from the options of "UNICEF", "MDG", "WHO", "WB", "UNPD", "OIC",  
  ## "Countdown", "ECAAfrica", "AU", "Fragile2013", "Fragile2014", "USAID" 
  country.info.CME.file = NULL, ##<< If \code{NULL}, country info included in package is used. 
  quantity = "Posterior probabilities", ##<< Quantity calculated (used for file name).
  percentiles = c(0.05, 0.5, 0.95), ##<< Vector of percentiles.
  ndigits = 1 ##<< Number of decimal places to use for analysis (e.g. to calcalate ARR, decline).
) {
  source("R/chooseregion.R")
  output.dir.samplescombined <- file.path(output.dir, "samples_combined")
  filename <- paste0(regiontype.select, "Region")
  
  if (is.null(country.info.CME.file))
    country.info.CME.file <- file.path("input", "country.info.CME.csv")
  
  # read in data
  country.info <- read.csv(file = country.info.CME.file, header = T, stringsAsFactors = F, 
                           strip.white = T)
  country.info <- country.info[, !grepl("pop", colnames(country.info))]
  
  country.RoDs.probs <- colnames.country.RoDs.probs <- NULL
  for (t in 1:length(year1.t)) {
    year1 <- year1.t[t]
    year2 <- year2.t[t]
    year4 <- year4.t[t]
    eval(parse(text = paste0("load(file = file.path(output.dir.samplescombined, \"", "ARR.", 
                             year1-0.5, ".", year2-0.5, ".cj\"))")))
    eval(parse(text = paste0("ARR.year1.year2.cj <- ARR.", year1-0.5, ".", year2-0.5, ".cj")))
    eval(parse(text = paste0("load(file = file.path(output.dir.samplescombined, \"", "ARR.", 
                             year2-0.5, ".", year4-0.5, ".cj\"))")))
    eval(parse(text = paste0("ARR.year2.year4.cj <- ARR.", year2-0.5, ".", year4-0.5, ".cj")))
    changeinARR.cj <- ARR.year2.year4.cj - ARR.year1.year2.cj
    # calculate posterior probabilities
    if (t == 1) { # because the periods (usually) repeat
      prob.ARR.year1.year2.M0.c <- apply(ARR.year1.year2.cj, 1, CalculatePosteriorProbabilityM0)
      prob.ARR.year1.year2.MEq4.4.c <- apply(ARR.year1.year2.cj, 1, CalculatePosteriorProbabilityMEq4.4)
    }
    prob.ARR.year2.year4.M0.c <- apply(ARR.year2.year4.cj, 1, CalculatePosteriorProbabilityM0)
    prob.ARR.year2.year4.MEq4.4.c <- apply(ARR.year2.year4.cj, 1, CalculatePosteriorProbabilityMEq4.4)
    prob.changeinARR.M0.c <- apply(changeinARR.cj, 1, CalculatePosteriorProbabilityM0)
    changeinARR.cq <- t(apply(changeinARR.cj, 1, quantile, percentiles, na.rm = T))
    
    if (t == 1) {
      country.RoDs.probs <- cbind(country.RoDs.probs,
                                  prob.ARR.year1.year2.M0.c, prob.ARR.year1.year2.MEq4.4.c, 
                                  prob.ARR.year2.year4.M0.c, prob.ARR.year2.year4.MEq4.4.c,
                                  prob.changeinARR.M0.c, changeinARR.cq)
      colnames.country.RoDs.probs <- c(colnames.country.RoDs.probs,
                                       paste0("P.ARR ", year1-0.5, "-", year2-0.5, " M0"), 
                                       paste0("P.ARR ", year1-0.5, "-", year2-0.5, " MEq4.4"), 
                                       paste0("P.ARR ", year2-0.5, "-", year4-0.5, " M0"),      
                                       paste0("P.ARR ", year2-0.5, "-", year4-0.5, " MEq4.4"), 
                                       paste0("P.Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " M0"),
                                       paste0("Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " lower bound"),
                                       paste0("Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " median"),
                                       paste0("Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " upper bound"))
    } else {
      country.RoDs.probs <- cbind(country.RoDs.probs,
                                  prob.ARR.year2.year4.M0.c, prob.ARR.year2.year4.MEq4.4.c,
                                  prob.changeinARR.M0.c, changeinARR.cq)
      colnames.country.RoDs.probs <- c(colnames.country.RoDs.probs,
                                       paste0("P.ARR ", year2-0.5, "-", year4-0.5, " M0"),      
                                       paste0("P.ARR ", year2-0.5, "-", year4-0.5, " MEq4.4"), 
                                       paste0("P.Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " M0"),
                                       paste0("Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " lower bound"),
                                       paste0("Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " median"),
                                       paste0("Change in ARR ", year4-0.5, "-", year2-0.5, "-", year1-0.5, 
                                              " upper bound"))
    }
  }
  # output to .csv
  colnames(country.RoDs.probs) <- colnames.country.RoDs.probs
  country.RoDs.probs <- cbind(country.info, country.RoDs.probs)
  write.csv(country.RoDs.probs, 
            file = file.path(output.dir, paste0("Rates of Decline ", quantity, "_Country Summary.csv")),
            row.names = FALSE, na = "")
  cat(paste0("Country results saved to ", output.dir, "\n"))
  #----------------------------------------------------------------------
  # calculate ARR for region  
  region.RoDs.probs <- NULL
  for (t in 1:length(year1.t)) {
    year1 <- year1.t[t]
    year2 <- year2.t[t]
    year4 <- year4.t[t]
    eval(parse(text = paste0("load(file = file.path(output.dir.samplescombined, \"", filename, "_ARR.", 
                             year1-0.5, ".", year2-0.5, ".rj\"))")))
    eval(parse(text = paste0("ARR.year1.year2.rj <- ARR.", year1-0.5, ".", year2-0.5, ".rj")))
    eval(parse(text = paste0("load(file = file.path(output.dir.samplescombined, \"", filename, "_ARR.", 
                             year2-0.5, ".", year4-0.5, ".rj\"))")))
    eval(parse(text = paste0("ARR.year2.year4.rj <- ARR.", year2-0.5, ".", year4-0.5, ".rj")))
    changeinARR.rj <- ARR.year2.year4.rj - ARR.year1.year2.rj
    # calculate posterior probabilities
    if (t == 1) { # because the periods (usually) repeat
      prob.ARR.year1.year2.M0.r <- apply(ARR.year1.year2.rj, 1, CalculatePosteriorProbabilityM0)
      prob.ARR.year1.year2.MEq4.4.r <- apply(ARR.year1.year2.rj, 1, CalculatePosteriorProbabilityMEq4.4)
    }
    prob.ARR.year2.year4.M0.r <- apply(ARR.year2.year4.rj, 1, CalculatePosteriorProbabilityM0)
    prob.ARR.year2.year4.MEq4.4.r <- apply(ARR.year2.year4.rj, 1, CalculatePosteriorProbabilityMEq4.4)
    prob.changeinARR.M0.r <- apply(changeinARR.rj, 1, CalculatePosteriorProbabilityM0)
    changeinARR.rq <- t(apply(changeinARR.rj, 1, quantile, percentiles, na.rm = T))
    if (t == 1) {
      region.RoDs.probs <- cbind(region.RoDs.probs,
                                 prob.ARR.year1.year2.M0.r, prob.ARR.year1.year2.MEq4.4.r, 
                                 prob.ARR.year2.year4.M0.r, prob.ARR.year2.year4.MEq4.4.r,
                                 prob.changeinARR.M0.r, changeinARR.rq)
    } else {
      region.RoDs.probs <- cbind(region.RoDs.probs,
                                 prob.ARR.year2.year4.M0.r, prob.ARR.year2.year4.MEq4.4.r,
                                 prob.changeinARR.M0.r, changeinARR.rq)
    }
  }
  colnames(region.RoDs.probs) <- colnames.country.RoDs.probs
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
  region.RoDs <- data.frame(Region = c(regiontypes, "World"), region.RoDs.probs)
  write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline ", quantity, "_", filename, ".csv")),
            row.names = F, na = "")
  cat(paste0("Output generated for ", filename, " and saved to ", output.dir, ".\n"))
}

CalculatePosteriorProbabilityM0 <- function(x) {
  mean(x > 0)
}

CalculatePosteriorProbabilityMEq4.4 <- function(x) {
  mean(x >= 4.4)
}
