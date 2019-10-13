#--------------------------------------------------
# getq5estimates.R
# JRN, Jan 2013
#--------------------------------------------------
SummariseU5MREstimates <- function(# Summarise U5MR run and output list to be used for IMR and NMR runs.
  results.file = NULL, ##<< Specify 1) either \code{results.file}, so Results.csv is used as input instead of .rda files,
  runname.U5MR = NULL, ##<< or 2) \code{runname.U5MR} so the relevant .rda files can be read in.
  get.adjusted.estimates = TRUE, ##<< Get adjusted estimates without HIV/crisis adjustments?
  weight.alpha.select = NULL, ##<< Choice of weight.alpha for results to use. Required if runname.U5MR is not \code{NULL}.
  output.dir = NULL,
  hiv.file = NULL,
  adj.file = NULL
) {
  if (is.null(output.dir))
    output.dir <- paste0(getwd(), "/output/", runname.U5MR, "/")
  if (get.adjusted.estimates) {
    if (is.null(hiv.file))
      hiv.file <- file.path("input", "dataUNAIDS_U5MR.csv")
    hiv.data <- read.csv(hiv.file, header = T, stringsAsFactors = F)
    if (is.null(adj.file))
      adj.file <- file.path("input", "dataPostAdj_U5MR.csv")
    adj.data <- read.csv(adj.file, header = T, stringsAsFactors = F)
  }
  if (!is.null(results.file)) {
    results <- read.csv(results.file, header = T, stringsAsFactors = F)
    iso.c <- unique(results$ISO.Code)
    colnames.year <- colnames(results)[grepl("X", colnames(results))]
    results.processed <- results[results$Quantile == "Median", is.element(colnames(results), colnames.year)]
    colnames.year.select <- apply(results.processed, 2, function(x) sum(!is.na(x))) != 0
    year.t <- as.numeric(gsub("X", "", colnames.year[colnames.year.select]))
    res.ct <- results.processed[, colnames.year.select]
    if (get.adjusted.estimates) {
      # get HIV-free estimates
      res.hivremoved.ct <- res.ct
      for (c in 1:length(iso.c)) {
        if (is.element(iso.c[c], hiv.data$countrycode.hiv)) {
          res.hivremoved.ct[c, ] <-
            GetHIVSubtractedSeries(year.i = year.t,
                                   u.i = res.ct[c, ],
                                   iso = iso.c[c], hiv.file = hiv.file)
        }
      }
      # get crisis-free estimates
      res.crisisandhivremoved.ct <- res.hivremoved.ct
      for (c in 1:length(iso.c)) {
        if (is.element(iso.c[c], adj.data$countrycode.adj)) {
          res.crisisandhivremoved.ct[c, ] <-
            GetCrisisSubtractedSeries(year.i = year.t,
                                      u.i = res.hivremoved.ct[c, ],
                                      iso = iso.c[c], adj.file = adj.file)
        }
      }
      res.final.ct <- res.crisisandhivremoved.ct
    } else {
      res.final.ct <- res.ct
    }
  } else if (!is.null(runname.U5MR)) {
    # load files
    if (get.adjusted.estimates) {
      load(file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
    } else {
      load(file.path(output.dir, "res.cqt.Lw.rda"))
    }
    load(file.path(output.dir, "iso.c.rda"))
    load(file.path(output.dir, "year.t.rda"))
    if (get.adjusted.estimates) {
      # get crisis-free estimates
      res.crisisandhivremoved.cqt.Lw <- res.hivremoved.cqt.Lw 
      for (c in 1:length(iso.c)) {
        res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha.select)]][c, 2, ] <-
          GetCrisisSubtractedSeries(year.i = year.t,
                                    u.i = res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]][c, 2, ],
                                    iso = iso.c[c], adj.file = adj.file)
      }
      res.final.ct <- res.crisisandhivremoved.ct <- 
        res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha.select)]][, 2, ]
    } else {
      res.final.ct <- res.cqt.Lw[[paste0(weight.alpha.select)]][, 2, ]
    }
  }
  res.U5MR <- list(res.ct = res.final.ct,
                   iso.c = iso.c,
                   year.t = year.t)
  # res.U5MR.noadj is the final estimates, res.U5MR is the crisis-and-HIV-free estimates
  save(res.U5MR, file = file.path(output.dir, paste0("res.U5MR",
                                                     ifelse(get.adjusted.estimates, "", ".nonadj"), ".rda")))
  ##value<< \code{NULL}; Saves \code{res.U5MR} to \code{output.dir}. 
  return(invisible())
}

Getq5Estimates <- function( # Get q5 estimates from B3 global run for required years
  iso, ##<< ISO country code
  years, ##<< Vector of years
  get.adjusted.estimates = TRUE, ##<< Get adjusted estimates without HIV/crisis adjustments?
  runname.U5MR, ##<< Run name of B3 global model run for U5MR
  printWarnings = TRUE #<< Logical value indicating whether or not to print warning messages
) {
  # load U5MR run results
  load(file.path("output", runname.U5MR, paste0("res.U5MR",
                                                ifelse(get.adjusted.estimates, "", ".nonadj"), ".rda")))
  est.years <- res.U5MR$year.t[!is.na(res.U5MR$res.ct[res.U5MR$iso.c == iso, ])]
  # obtain estyears closest to years
  # if year is between two estyears, take the larger estyear
  approxyears <- sapply(years, function(year) { 
    approxyear <- max(est.years[abs(year-est.years) == min(abs(year-est.years))])
    if (printWarnings) {
      # print warning message if approxyear is too far off from input year
      if (min(abs(year-est.years)) > 1) {
        print(paste0("warning for ", iso, ": Input year ", year, 
                    " is ", min(abs(year-est.years)), " year(s) away from nearest U5MR estimate year."))
      }
    }
    return(approxyear)
  })
  q5ests <- NULL
  for (y in 1:length(approxyears)) {
    q5ests <- c(q5ests, unlist(res.U5MR$res.ct[res.U5MR$iso.c == iso, res.U5MR$year.t == approxyears[y]]))
  }
  ##value<<
  return(q5ests) ##<< Vector of q5 estimates for required years
}
