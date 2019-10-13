#----------------------------------------------------------------------
# outputaggregates.R
# Jin Rou New, 2012-2013
#----------------------------------------------------------------------
OutputAggregates <- function( # Calculate and output aggregated rates and numbers of deaths at the country,
  ## regional and global level.
  runname.U5MR = NULL, ##<< Either specify 1) \code{runname.U5MR}
  runname.IMR = NULL, ##<< and \code{runname.IMR}, 
  runname.NMR = NULL, ##<< and/or \code{runname.NMR}
  results.U5MR.file = NULL, ##<< 2) and \code{results.U5MR.file} (for median only)
  results.IMR.file = NULL, ##<< and \code{results.IMR.file} (for median only)
  results.NMR.file = NULL, ##<< and/or \code{results.NMR.file} (for median only)
  filename.U5MR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.U5MR} is specified.
  ## Default is \code{u5mrfinal.ctj.rda}.
  filename.IMR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.IMR} is specified.
  filename.NMR = NULL, ##<< Alternative file name of trajectory array, used if \code{runname.NMR} is specified.
  ## Default is \code{imrfinal.ctj.rda}.
  country.info.file = NULL, ##<< If \code{NULL}, country info included in package is used.
  population.file = NULL, ##<< File path to population data. If \code{NULL}, population data included in package is used.
  livebirths.file = NULL,
  data.a0.file = NULL, ##<< File path to a0 data. If \code{NULL}, a0 data included in package is used.
  ## package from World Population Prospects 2012 is used.
  run.on.server = TRUE, ##<< Running on server? Set \code{TRUE} to run in parallel.
  regiontypes.select = c("UNICEF", "NewUnicef", "MDG", "SDG", "WHO", "WB", "UNPD", "OIC",
                         "Countdown", "ECAAfrica", "AU",
                         "Fragile2013", "Fragile2014", "Fragile2015", "Fragile2017",
                         "USAID", "M49"), ##<< Output regional aggregates for which region types? 
  ## Input a character vector of more than one of the possible options if desired.
  ## If \code{NULL}, output will not be generated at the region level.
  ### ---- note 7-27-17: Add new UNICEF regions?
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year4 = 2015.5, ##<< Last year used for ARR calculation.
  year.target = 2016.5, ##<< MDG target year.
  est.years = seq(1950.5, 2016.5, 1), ##<< Years of estimation.
  factor.target = 1/3, ##<< MDG target factor (Reduce to one third).
  percentiles = c(0.05, 0.5, 0.95), ##<< Vector of percentiles.
  ndigits = 1, ##<< Number of decimal places to use for analysis (e.g. to calcalate ARR, decline).
  output.dir = NULL, ##<< Output directory to save all results.
  test = FALSE ##<< Use a subset of 10 trajectories to test function.
) {
  source("R/chooseregion.R")

  if (is.null(output.dir))
  output.dir <- "output_numberofdeaths"
  output.dir.samples <- file.path(output.dir, "samples")
  output.dir.samplescombined <- file.path(output.dir, "samples_combined")
  dir.create(file.path(getwd(), output.dir), showWarnings = FALSE)
  dir.create(file.path(getwd(), output.dir.samples), showWarnings = FALSE)
  dir.create(file.path(getwd(), output.dir.samplescombined), showWarnings = FALSE)

  ## Add live births to this input list? Will need for the BWC calculations
  if (is.null(country.info.file))
    country.info.file <- file.path("input", "country.info.CME.csv")
  if (is.null(population.file)) # same file used as country.info.CME.csv contains both country and population info
    population.file <- file.path("input", "country.info.CME.csv")
  if (is.null(data.a0.file))
    data.a0.file <- file.path("input", "a0.csv")
  if (is.null(livebirths.file)) ## added DJS 2017-07-27 for BWC method
    livebirths.file <- file.path("input", "data_livebirths.csv")
  
  # read in data
  country.info <- read.csv(file = country.info.file, header = T, stringsAsFactors = F,
                           strip.white = T)
  country.info <- country.info[, !grepl("pop", colnames(country.info))]
  data.pop <- read.csv(file = population.file, header = T, stringsAsFactors = F, strip.white = T)
  data.a0 <- read.csv(file = data.a0.file, header = T, stringsAsFactors = F, strip.white = T)
  data.livebirths <- read.csv(file = livebirths.file, header = T, stringsAsFactors = F, strip.white = T)

  # reformat data (same country order)
  data.pop <- join(data.frame(ISO3Code = country.info$ISO3Code), data.pop)
  data.a0 <- join(data.frame(iso = country.info$ISO3Code), data.a0)
  data.livebirths <- reshape(data=data.livebirths, idvar=c("country", "uncode", "sex"), timevar = "year", direction = "wide")
  data.livebirths <- data.livebirths[match(country.info$UNCode, data.livebirths$uncode),] 
  data.livebirths$iso <- country.info$ISO3Code[match(country.info$UNCode, data.livebirths$uncode)]
  
  if (sum(is.na(data.a0$a0)) > 0)
    cat(paste0("Warning: a0 is NA for ", paste(data.a0$iso[is.na(data.a0$a0)], collapse = ", "), ".\n"))

  # read in B3 U5MR and IMR trajectories
  if (!is.null(runname.U5MR) & !is.null(runname.IMR)) {
    cat("Reading in results from output/runname.U5MR and output/runname.IMR.\n")
  } else if (!is.null(results.U5MR.file) & !is.null(results.IMR.file)) {
    cat("Reading in results from results.U5MR.file and results.IMR.file.\n")
  } else {
    cat("Error: Either runname.U5MR and runname.IMR or results.U5MR.file and results.IMR.file must be specified.\n")
  }
  if (is.null(filename.U5MR))
    filename.U5MR <- "u5mrfinal.ctj.rda" ## check structure here to fit into BWC code
  if (is.null(filename.IMR))
    filename.IMR <- "imrfinal.ctj.rda" ## check structure here to fit into BWC code
  if (is.null(filename.NMR))
    filename.NMR <- "nmrfinal.ctj.rda" ## check structure here to fit into BWC code
  if (!is.null(results.U5MR.file) & !is.null(results.IMR.file)) { ## may need to add NMR file here if SummariseResults works the same for it
    SummariseResults(results.file = results.U5MR.file,
                     output.dir = file.path(output.dir, "U5MR"),
                     filename.output = gsub(".rda", "", filename.U5MR))
    SummariseResults(results.file = results.IMR.file,
                     output.dir = file.path(output.dir, "IMR"),
                     filename.output = gsub(".rda", "", filename.IMR))

    # load required U5MR files
    load(file = file.path(output.dir, "U5MR", filename.U5MR))
    eval(parse(text = paste0("u5mrfinal.ctj <- ", gsub(".rda", "", filename.U5MR))))
    load(file = file.path(output.dir, "U5MR", "iso.c.rda"))
    load(file = file.path(output.dir, "U5MR", "year.t.rda"))
    isoU5MR.c <- iso.c
    yearU5MR.t <- year.t

    # load required IMR files
    load(file = file.path(output.dir, "IMR", filename.IMR))
    eval(parse(text = paste0("imrfinal.ctj <- ", gsub(".rda", "", filename.IMR))))
    load(file = file.path(output.dir, "IMR", "iso.c.rda"))
    load(file = file.path(output.dir, "IMR", "year.t.rda"))
    isoIMR.c <- iso.c
    yearIMR.t <- year.t
    
    # nmr
    if(!is.null(results.NMR.file)) {
      SummariseResults(results.file = results.NMR.file,
                       output.dir = file.path(output.dir, "NMR"),
                       filename.output = gsub(".Rda", "", filename.NMR))
      
      load(file = file.path(output.dir, "NMR", gsub(".Rda", ".rda", filename.NMR)))
      eval(parse(text = paste0("nmrfinal.ctj <- ", gsub(".Rda", "", filename.NMR))))
      load(file = file.path(output.dir, "NMR", "iso.c.rda"))
      load(file = file.path(output.dir, "NMR", "year.t.rda"))
      isoNMR.c <- iso.c
      yearNMR.t <- year.t
    }
  } else {
    # load required U5MR files
    load(file = file.path("output", runname.U5MR, filename.U5MR))
    eval(parse(text = paste0("u5mrfinal.ctj <- ", gsub(".rda", "", filename.U5MR))))
    load(file = file.path("output", runname.U5MR, "iso.c.rda"))
    load(file = file.path("output", runname.U5MR, "year.t.rda"))
    isoU5MR.c <- iso.c
    yearU5MR.t <- year.t

    # load required IMR files
    load(file = file.path("output", runname.IMR, filename.IMR))
    eval(parse(text = paste0("imrfinal.ctj <- ", gsub(".rda", "", filename.IMR))))
    load(file = file.path("output", runname.IMR, "iso.c.rda"))
    load(file = file.path("output", runname.IMR, "year.t.rda"))
    isoIMR.c <- iso.c
    yearIMR.t <- year.t
    
    if(!is.null(runname.NMR)) {
      load(file = file.path("output", runname.NMR, filename.NMR))
      eval(parse(text = paste0("nmrfinal.ctj <- ", gsub("final","",gsub(".Rda", "", filename.NMR)))))
      nmrfinal.ctj <- aperm(nmrfinal.ctj, c(3,2,1))
      isoNMR.c <- dimnames(nmrfinal.ctj)[[1]]
      yearNMR.t <- as.numeric(dimnames(nmrfinal.ctj)[[2]])
    }
  }

  # get dimensions
  nyears <- length(est.years) ## may use this for years or length of some dimension of output files
  est.years.floor <- floor(est.years)
  iso.c <- country.info$ISO3Code
  C <- length(iso.c)

  # test
  if (test) {
    u5mrfinal.ctj <- u5mrfinal.ctj[, , 1:10]
    imrfinal.ctj <- imrfinal.ctj[, , 1:10]
    if(exists("nmrfinal.ctj")) nmrfinal.ctj <- nmrfinal.ctj[, , 1:10]
  }
  nsim <- dim(u5mrfinal.ctj)[3]

  if (sum(!file.exists(file.path(output.dir.samplescombined, "u5mr.ctj.rda"),
                       file.path(output.dir.samplescombined, "imr.ctj.rda"))) > 0) {
    u5mr.ctj <- imr.ctj <- array(NA, c(C, nyears, nsim))
    u5mr.ctj[match(isoU5MR.c, iso.c), is.element(est.years, yearU5MR.t), ] <-
      u5mrfinal.ctj[, is.element(yearU5MR.t, est.years), ]
    imr.ctj[match(isoIMR.c, iso.c), is.element(est.years, yearIMR.t), ] <-
      imrfinal.ctj[, is.element(yearIMR.t, est.years), ]


    # check that U5MR and IMR <= 1000
    u5mr.ctj[u5mr.ctj > 1000] <- 1000
    imr.ctj[imr.ctj > 1000] <- 1000

    # calculate q1to4.ctj
    q1to4.ctj <- 1-(1-u5mr.ctj/1000)/(1-imr.ctj/1000)
    # make 1-year rate for BWC method
    #q1to4.ctj <- (1-((1-(q1to4.ctj))^(1/4)))
    arr.ind.select <- which(is.na(q1to4.ctj), arr.ind = TRUE)
    # set u5mr.ctj and imr.ctj to NA wherever the other rate is NA in that country-year
    u5mr.ctj[arr.ind.select] <- NA
    imr.ctj[arr.ind.select] <- NA
    # check for countries with all NA values
    select.NA.c <- apply(u5mr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Warning: U5MR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    select.NA.c <- apply(imr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Warning: IMR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    # save
    save(u5mr.ctj, file = file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
    save(imr.ctj, file = file.path(output.dir.samplescombined, "imr.ctj.rda"))
    cat(paste0("Processed trajectories saved to ", output.dir.samplescombined, "\n"))
  } else {
    load(file = file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
    load(file = file.path(output.dir.samplescombined, "imr.ctj.rda"))
    cat(paste0("Processed trajectories loaded from ", output.dir.samplescombined, "\n"))
  }
  
  #NMR
  if (!file.exists(file.path(output.dir.samplescombined, "nmr.ctj.rda"))&exists("nmrfinal.ctj")) {
    nmr.ctj  <- array(NA, c(C, nyears, nsim))
    nmr.ctj[match(isoNMR.c, iso.c), is.element(est.years, yearNMR.t), ] <-
      nmrfinal.ctj[, is.element(yearNMR.t, est.years), ]
    
    # check that U5MR and IMR <= 1000
    nmr.ctj[nmr.ctj > 1000] <- 1000

    arr.ind.select <- which(is.na(q1to4.ctj), arr.ind = TRUE)
    # set nmr.ctj to NA wherever the u5mr or imr is NA in that country-year
    nmr.ctj[arr.ind.select] <- NA

    # check for countries with all NA values
    select.NA.c <- apply(nmr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Warning: NMR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    # save
    save(nmr.ctj, file = file.path(output.dir.samplescombined, "nmr.ctj.rda"))
    cat(paste0("Processed NMR trajectories saved to ", output.dir.samplescombined, "\n"))
  } else {
    if(!is.null(results.NMR.file)|!is.null(runname.NMR)){
      load(file = file.path(output.dir.samplescombined, "nmr.ctj.rda"))
      cat(paste0("Processed NMR trajectories loaded from ", output.dir.samplescombined, "\n"))
    } else {
      nmr.ctj <- NULL
    } #if/else
  } # if/else
  
  #-------------------------------------------------------------------------
  a0.c <- data.a0$a0
  a1to4.c <- rep(0.4, length(a0.c))
  pop0.orig.ct <- data.pop[, is.element(colnames(data.pop), paste0("pop0", est.years.floor))]
  pop1to4.orig.ct <- data.pop[, is.element(colnames(data.pop), paste0("pop1to4", est.years.floor))]
  # get live birth matrix like pop; added 2017-07-27 DJS for BWC method
  lb.ct <- data.livebirths[,which(names(data.livebirths)==paste0("lb.",est.years[1])):which(names(data.livebirths)==paste0("lb.",est.years[length(est.years)]))]
  #-------------------------------------------------------------------------
  # get country results
  if(!is.null(nmr.ctj)){
    files.country <- c("death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda", "deathnn.ctj.rda",
                       "dx.array.ctj.rda", "lx.array.ctj.rda", "dx.nn.array.ctj.rda", "lx.nn.array.ctj.rda",
                       "ARR.year1.year4.cj.rda", "ARR.year1.year2.cj.rda",
                       "ARR.year2.year4.cj.rda", "ARR.year1.year4.cj.rda",
                       "decline.year1.year4.cj.rda", "decline.year1.year2.cj.rda",
                       "decline.year2.year4.cj.rda")
  } else {
    files.country <- c("death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda",
                       "dx.array.ctj.rda", "lx.array.ctj.rda",
                       "ARR.year1.year4.cj.rda", "ARR.year1.year2.cj.rda",
                       "ARR.year2.year4.cj.rda", "ARR.year1.year4.cj.rda",
                       "decline.year1.year4.cj.rda", "decline.year1.year2.cj.rda",
                       "decline.year2.year4.cj.rda")
  }

  if (sum(!file.exists(file.path(output.dir.samplescombined, files.country))) > 0) {
    cat(paste("Generating country results...\n"))
    if (run.on.server) {
      registerDoMC()
      foreach (j = 1:nsim) %dopar% {
        CalculateCountryDeathsBWC(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                  livebirths.ct = lb.ct,
                                  a0.c = a0.c, a1to4.c = a1to4.c,
                                  pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                  iso.c = iso.c, est.years = est.years,
                                  year1 = year1, year2 = year2, year4 = year4,
                                  year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                  output.dir = output.dir.samples)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    } else {
      for (j in 1:nsim) {
       CalculateCountryDeathsBWC(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                 livebirths.ct = lb.ct,
                                 a0.c = a0.c, a1to4.c = a1to4.c,
                                 pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                 iso.c = iso.c, est.years = est.years,
                                 year1 = year1, year2 = year2, year4 = year4,
                                 year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                 output.dir = output.dir.samples)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
    cat(paste0("Combining and outputting country results...\n"))
    CombineAndOutputCountryResults(u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                   country.info = country.info,
                                   percentiles = percentiles, ndigits = ndigits,
                                   output.dir = output.dir,
                                   output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined)
  } else {
    sapply(files.country, LoadFile, output.dir = output.dir.samplescombined,
           envir = environment())
    cat(paste("Country results loaded from ", output.dir.samplescombined, "\n"))
  }
  #-------------------------------------------------------------------------
  # get world results
  if(is.null(nmr.ctj)){
    files.world <- c("res.world.rda", "global.RoDs.ui.rda", "u5mr.wtj.rda", "imr.wtj.rda",
                     "deathu5.all.wtj.rda", "death0.all.wtj.rda",
                     "pop0.wt.rda", "pop1to4.wt.rda",
                     "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                     "livebirths.wt.rda",
                     "coverage0.wt.rda", "coverageu5.wt.rda") # change JR, 26 Aug 2013
  } else {
    files.world <- c("res.world.rda", "global.RoDs.ui.rda", "u5mr.wtj.rda", "imr.wtj.rda", "nmr.wtj.rda",
                     "deathu5.all.wtj.rda", "death0.all.wtj.rda", "deathnn.all.wtj.rda",
                     "pop0.wt.rda", "pop1to4.wt.rda",
                     "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                     "livebirths.wt.rda",
                     "coverage0.wt.rda", "coverageu5.wt.rda")
  }

  if (sum(!file.exists(file.path(output.dir.samplescombined, files.world))) > 0) {
    cat(paste("Generating world results...\n"))
    # CalculateWorldDeaths(output.dir.samplescombined = output.dir.samplescombined, 
    #                      output.dir = output.dir,
    #                      percentiles = percentiles,
    #                      ndigits = ndigits)
    CalculateWorldDeathsBWC(output.dir.samplescombined = output.dir.samplescombined, 
                         output.dir.samples = output.dir.samples,
                         output.dir = output.dir,
                         percentiles = percentiles,
                         ndigits = ndigits)
    
    cat(paste("Output generated for world.\n"))
  } else {
    sapply(files.world, LoadFile, output.dir = output.dir.samplescombined,
           envir = environment())
    cat(paste("World results loaded from ", output.dir.samplescombined, "\n"))
  }
  #-------------------------------------------------------------------------
  # get regional results
  if (!is.null(regiontypes.select)) {    #PROBABLY DELETE THIS ONE
    cat(paste("Generating regional results...\n"))
    if (is.element("UNICEF", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = UNICEFRegionAll, ## func at end; think about new regions
                         regions = country.info[, grepl("UNICEF", colnames(country.info))],
                         filename = "UNICEFRegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("NewUnicef", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = NewUnicefRegionAll,
                            regions = country.info[, grepl("NewUnicef", colnames(country.info))],
                            filename = "NewUnicef",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits)
    if (is.element("MDG", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = MDGRegionAll,
                         regions = country.info[, grepl("MDG", colnames(country.info))],
                         filename = "MDGRegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("SDG", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = SDGRegionAll,
                            regions = country.info[, grepl("SDG", colnames(country.info))],
                            filename = "SDGRegion",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits)
    if (is.element("WHO", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WHORegionAll,
                         regions = country.info[, grepl("WHO", colnames(country.info))],
                         filename = "WHORegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("WB", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = WBRegionAll,
                         regions = country.info[, grepl("WB", colnames(country.info))],
                         filename = "WBRegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("UNPD", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = UNPDRegionAll,
                         regions = country.info[, grepl("UNPD", colnames(country.info))],
                         filename = "UNPDRegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("OIC", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = OICRegionAll,
                         regions = country.info[, grepl("OIC", colnames(country.info))],
                         filename = "OICRegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("Countdown", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = CountdownAll,
                         regions = country.info[, grepl("Countdown", colnames(country.info))],
                         filename = "CountdownCountries",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("ECAAfrica", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = ECAAfricaRegionAll,
                         regions = country.info[, grepl("ECAAfrica", colnames(country.info))],
                         filename = "ECAAfricaRegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("AU", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = AURegionAll,
                         regions = country.info[, grepl("AURegion2", colnames(country.info))],
                         filename = "AURegion",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("Fragile2013", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2013All,
                         regions = country.info[, grepl("FragileCountries2013", colnames(country.info))],
                         filename = "FragileCountries2013",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("Fragile2014", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2014All,
                         regions = country.info[, grepl("FragileCountries2014", colnames(country.info))],
                         filename = "FragileCountries2014",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("Fragile2015", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2015All,
                         regions = country.info[, grepl("FragileCountries2015", colnames(country.info))],
                         filename = "FragileCountries2015",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    
    if (is.element("Fragile2017", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = Fragile2017All,
                            regions = country.info[, grepl("FragileCountries2017", colnames(country.info))],
                            filename = "FragileCountries2017",
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits)
    if (is.element("USAID", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = USAIDAll,
                         regions = country.info[, grepl("USAID", colnames(country.info))],
                         filename = "USAIDCountries",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
    if (is.element("M49", regiontypes.select))
      GetRegionalResultsBWC(regiontypes = M49RegionAll,
                         regions = country.info[, grepl("M49", colnames(country.info))],
                         filename = "M49Region",
                         output.dir = output.dir, output.dir.samples = output.dir.samples,
                         output.dir.samplescombined = output.dir.samplescombined,
                         run.on.server = run.on.server,
                         percentiles = percentiles, ndigits = ndigits)
  }
}
#-------------------------------------------------------------------------
CalculateCountryDeathsBWC <- function(
  j, ##<< Index number of trajectory.
  u5mr.ctj,
  imr.ctj,
  nmr.ctj=NULL,
  a0.c,
  a1to4.c,
  pop0.orig.ct,
  pop1to4.orig.ct,
  livebirths.ct,
  iso.c,
  est.years,
  year1,
  year2,
  year4,
  year.target, ## final year of estimates
  factor.target,
  ndigits,
  output.dir
) {
  pop0.ct <- pop0.orig.ct
  pop1to4.ct <- pop1to4.orig.ct
  # set population to 0 if rate data not available
  arr.ind.select <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]), arr.ind = TRUE)
  arr.ind.select.nmr <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]) | is.na(nmr.ctj[, , 1]), arr.ind = TRUE)
  pop0.ct[arr.ind.select] <- 0
  pop1to4.ct[arr.ind.select] <- 0
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.ctj)[3] == 1) {
    u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
    imr.ctj <- roundoff(imr.ctj, digits = ndigits)
    nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  }
  u5mr.ct <- u5mr.ctj[, , j]
  imr.ct <- imr.ctj[, , j]
  nmr.ct <- nmr.ctj[, , j]
  
  C <- nrow(u5mr.ct)
  nyears <- ncol(u5mr.ct)
  death0.ct <- death1to4.ct <- deathu5.ct <- deathnn.ct <- matrix(NA, C, nyears)
  
  # make matrixes for calculation
  years <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  dx.array.by.c <- array(NA, dim=c(12,length(years)*52,nrow(u5mr.ct)))
  lx.array.by.c <- array(NA, dim=c(13,length(years)*52,nrow(u5mr.ct)))
  
  # wgt.nmr.mat
  weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4))
  wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  
  # wgt.pnmr.mat 
  weight.pnmr.j.1 <- c(((53-(5:52))/48), rep(0,4))
  wgt.pnmr.mat <- t(matrix(cbind(weight.pnmr.j.1, 1-weight.pnmr.j.1), ncol=2, nrow=52))
  
  wgt.u1.mat <- rbind(wgt.nmr.mat[1,], wgt.pnmr.mat[1,], wgt.nmr.mat[2,], wgt.pnmr.mat[2,])
  
  # wgt.cmr.mat
  weight.cmr.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.cmr.mat <- t(matrix(cbind(weight.cmr.j.1, 1-weight.cmr.j.1), ncol=8, nrow=52))
  
  # wgt.mat
  wgt.mat <- rbind(wgt.u1.mat, wgt.cmr.mat)
  
  # years.mat
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  years.mat <- rbind(years.mat[c(1,1,2,2),], years.mat[-c(1,2),])
  year1.est.u5 <- rep(NA, C)
  year1.est.u1 <- rep(NA, C)
  year1.est.nn <- rep(NA, C)
  
  
  # k loop for countries
  for(k in 1:nrow(u5mr.ct)){
    ## get just the years for which U5MR is available in the country
    years.k <- years[!is.na(as.numeric(u5mr.ct[k,]))] ## get years where we have u5MR for country k
    if(length(years.k)<1) next
    year1.est.u5[k] <- min(years.k)
    year1.est.u1[k] <- min(years[!is.na(as.numeric(imr.ct[k,]))])
    year1.est.nn[k] <- min(years[!is.na(as.numeric(nmr.ct[k,]))])
    
    ## get live births for years.k
    wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
    #if(nrow(wpp.livebirths.k)<1) next
    bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
    
    for(i in 1:length(years.k)){
      if(years.k[i]+5>max(years.k)){
        # nmr; has IMR for year[i] and year[i+1] 
        nmx.nn.i <- nmr.ct[k,match(years.k[i:length(years.k)],years)]
        # u1 mortality rates; has IMR for year[i] and year[i+1] 
        nmx.u1.i <- imr.ct[k,match(years.k[i:length(years.k)],years)]
        # u5 mortality rates; need same length as u1 to convert to 4q1
        nmx.u5.i <- u5mr.ct[k,match(years.k[i:length(years.k)],years)]
        # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
        nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
        nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
        nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
        # pnn mortality rates; has pnmr
        nmx.pnmr.i <- ((nmx.u1.i-nmx.nn.i)/(1000-nmx.nn.i))*1000
        # combine appropriate rates in mortality rate vector
        nmx.i <- c(nmx.nn.i[1], nmx.pnmr.i[1], nmx.nn.i[2], nmx.pnmr.i[2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
      } else {
        # nmr; has IMR for year[i] and year[i+1] 
        nmx.nn.i <- nmr.ct[k,match(years.k[i]:years.k[i+5],years)]
        # u1 mortality rates; has IMR for year[i] and year[i+1] 
        nmx.u1.i <- imr.ct[k,match(years.k[i]:(years.k[i]+5),years)]
        # u5 mortality rates; need same length as u1 to convert to 4q1
        nmx.u5.i <- u5mr.ct[k,match(years.k[i]:(years.k[i]+5),years)]
        # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
        nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
        nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
        nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
        # pnn mortality rates; has pnmr
        nmx.pnmr.i <- ((nmx.u1.i-nmx.nn.i)/(1000-nmx.nn.i))*1000
        # combine in mortality rate vector for lifetable function
        nmx.i <- c(nmx.nn.i[1], nmx.pnmr.i[1], nmx.nn.i[2], nmx.pnmr.i[2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
      } #if/else 
      
      ## turn nmx.i into matrix
      ifelse(i==1, nmx.mat.k <- matrix(nmx.i, nrow=12, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=12, ncol=52)))
    } # i loop for nmx matrix
    
    ## get infant and u5 deaths
    wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=12, ncol=length(years.k)*52)
    nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
    npx.mat.k <- 1-nqx.mat.k
    lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
    dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
    
    ## save items for getting regional and world rates
    cols.k <- ((dim(dx.array.by.c)[2]-(length(years.k)*52))+1):(length(years)*52)
    dx.array.by.c[,cols.k,k] <- dx.mat.k
    lx.array.by.c[,cols.k,k] <- lx.mat.k
    
    years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=12, ncol=length(years.k)*52)
    years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
    for(yrk in (year1.est.nn[k]+5):max(years.k)){
      deathu5.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T) 
    } # for loop for u5 deaths
    for(yrk in (year1.est.nn[k]+1):max(years.k)){
      death0.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:4,][floor(years.k.mat[1:4,])==yrk], na.rm=T)
    } # for loop for infant deaths 
    death1to4.ct[k,] <- deathu5.ct[k,]-death0.ct[k,]
    for(yrk in (year1.est.nn[k]+1):max(years.k)){  
      deathnn.ct[k,match(yrk,years)] <- sum(dx.mat.k[c(1,3),][floor(years.k.mat[c(1,3),])==yrk], na.rm=T)
    } ## for loop nn deaths
  } # k loop: countries
  
  # # set deaths to NA if rate data is not available, and for first 5 years for U5MR and 1 year for IMR and NMR
  # --- REPLACED in k loop with year1.est to calculate deaths only at year1.est.nn+1 for neoanatal and infant deaths and year1.est.nn+5 for U5 deaths; need t-5 years of cohorts surviving for complete under-5 deaths with BWC
  # death0.ct[arr.ind.select] <- NA
  # death1to4.ct[arr.ind.select] <- NA
  # deathu5.ct[arr.ind.select] <- NA
  
  # calculate country rates of decline
  ARR.year1.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  ARR.year1.year2.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  ARR.year2.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  required.ARR.c <- ifelse(year4 < year.target,
                           1/(year.target-year4)*
                             log(roundoff(u5mr.ct[, est.years == year1]*factor.target, digits = ndigits)/
                                   u5mr.ct[, est.years == year4])*-100, NA)
  changeinARR.c <- ARR.year2.year4.c - ARR.year1.year2.c
  decline.year1.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  decline.year1.year2.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  decline.year2.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  
  if (!file.exists(file.path(output.dir, "info.rda"))) {
    info <- list(iso.c = iso.c,
                 C = C,
                 est.years = est.years,
                 est.years.floor = est.years-0.5,
                 year1.est.nn = year1.est.nn,
                 year1.est.u1 = year1.est.u1,
                 year1.est.u5 = year1.est.u5,
                 nyears = nyears,
                 a0.c = a0.c,
                 a1to4.c = a1to4.c,
                 pop0.ct = pop0.ct,
                 pop1to4.ct = pop1to4.ct,
                 pop0.orig.ct = pop0.orig.ct,
                 pop1to4.orig.ct = pop1to4.orig.ct,
                 livebirths.ct = livebirths.ct,
                 year1 = year1,
                 year2 = year2,
                 year4 = year4,
                 year.target = year.target,
                 factor.target = factor.target)
    save(info, file = file.path(output.dir, "info.rda"))
    cat(paste0("Information about the aggregates have been saved to ", output.dir, ".\n"))
  }
  # save samples
  save(death0.ct, file = file.path(output.dir, paste0("death0.ct_", j, ".rda")))
  save(death1to4.ct, file = file.path(output.dir, paste0("death1to4.ct_", j, ".rda")))
  save(deathu5.ct, file = file.path(output.dir, paste0("deathu5.ct_", j, ".rda")))
  save(dx.array.by.c, file = file.path(output.dir, paste0("dx.array.ct_", j, ".rda")))
  save(lx.array.by.c, file = file.path(output.dir, paste0("lx.array.ct_", j, ".rda")))
  save(deathnn.ct, file = file.path(output.dir, paste0("deathnn.ct_", j, ".rda")))
  save(ARR.year1.year4.c, file = file.path(output.dir, paste0("ARR.year1.year4.c_", j, ".rda")))
  save(ARR.year1.year2.c, file = file.path(output.dir, paste0("ARR.year1.year2.c_", j, ".rda")))
  save(ARR.year2.year4.c, file = file.path(output.dir, paste0("ARR.year2.year4.c_", j, ".rda")))
  save(required.ARR.c, file = file.path(output.dir, paste0("required.ARR.c_", j, ".rda")))
  save(changeinARR.c, file = file.path(output.dir, paste0("changeinARR.c_", j, ".rda")))
  save(decline.year1.year4.c, file = file.path(output.dir, paste0("decline.year1.year4.c_", j, ".rda")))
  save(decline.year1.year2.c, file = file.path(output.dir, paste0("decline.year1.year2.c_", j, ".rda")))
  save(decline.year2.year4.c, file = file.path(output.dir, paste0("decline.year2.year4.c_", j, ".rda")))
}
#-------------------------------------------------------------------------
CombineAndOutputCountryResults <- function(
  u5mr.ctj,
  imr.ctj,
  nmr.ctj=NULL,
  country.info,
  percentiles,
  ndigits,
  output.dir,
  output.dir.samples,
  output.dir.samplescombined
) {
  load(file.path(output.dir.samples, "info.rda"))
  list2env(info, envir = environment())
  
  nsim <- dim(u5mr.ctj)[3]
  est.years.floor <- est.years-0.5
  
  # combine all the samples into their respective arrays
  if(is.null(nmr.ctj)){
    death0.ctj<-death1to4.ctj<-deathu5.ctj<-array(NA, c(C, nyears, nsim))
  } else {
    death0.ctj<-death1to4.ctj<-deathu5.ctj<-deathnn.ctj<-array(NA, c(C, nyears, nsim))
  }
  
  ARR.year1.year4.cj <- ARR.year1.year2.cj <- ARR.year2.year4.cj <- required.ARR.cj <- changeinARR.cj <-
    decline.year1.year4.cj <- decline.year1.year2.cj <- decline.year2.year4.cj <- array(NA, c(C, nsim))
  
  for (j in 1:nsim) {
    load(file.path(output.dir.samples, paste0("death0.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("death1to4.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("deathu5.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year1.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year1.year2.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("ARR.year2.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("required.ARR.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("changeinARR.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year1.year4.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year1.year2.c_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("decline.year2.year4.c_", j, ".rda")))
    death0.ctj[, , j] <- death0.ct
    death1to4.ctj[, , j] <- death1to4.ct
    deathu5.ctj[, , j] <- deathu5.ct
    ARR.year1.year4.cj[, j] <- ARR.year1.year4.c
    ARR.year1.year2.cj[, j] <- ARR.year1.year2.c
    ARR.year2.year4.cj[, j] <- ARR.year2.year4.c
    required.ARR.cj[, j] <- required.ARR.c
    changeinARR.cj[, j] <- changeinARR.c
    decline.year1.year4.cj[, j] <- decline.year1.year4.c
    decline.year1.year2.cj[, j] <- decline.year1.year2.c
    decline.year2.year4.cj[, j] <- decline.year2.year4.c
    if(!is.null(nmr.ctj)){
      load(file.path(output.dir.samples, paste0("deathnn.ct_", j, ".rda")))
      deathnn.ctj[, , j] <- deathnn.ct
    }
  }
  # save combined results
  save(death0.ctj, file = file.path(output.dir.samplescombined, "death0.ctj.rda"))
  save(death1to4.ctj, file = file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  save(deathu5.ctj, file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  save(ARR.year1.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year4.cj.rda"))
  save(ARR.year1.year2.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year2.cj.rda"))
  save(ARR.year2.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year2.year4.cj.rda"))
  save(required.ARR.cj, file = file.path(output.dir.samplescombined, "required.ARR.cj.rda"))
  save(changeinARR.cj, file = file.path(output.dir.samplescombined, "changeinARR.cj.rda"))
  save(decline.year1.year4.cj, file = file.path(output.dir.samplescombined, "decline.year1.year4.cj.rda"))
  save(decline.year1.year2.cj, file = file.path(output.dir.samplescombined, "decline.year1.year2.cj.rda"))
  save(decline.year2.year4.cj, file = file.path(output.dir.samplescombined, "decline.year2.year4.cj.rda"))
  save(info, file = file.path(output.dir.samplescombined, "info.rda"))
  
  if(!is.null(nmr.ctj)){
    save(deathnn.ctj, file = file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  }
  # delete samples
  unlink(file.path(output.dir.samples, paste0("death0.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("death1to4.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.ct_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year1.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year1.year2.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("ARR.year2.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("required.ARR.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("changeinARR.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year1.year4.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year1.year2.c_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("decline.year2.year4.c_", 1:nsim, ".rda")))
  
  if(!is.null(nmr.ctj)){
    unlink(file.path(output.dir.samples, paste0("deathnn.ct_", 1:nsim, ".rda")))
  }
  #----------------------------------------------------------------------
  # output country summaries
  u5mr.qct <- apply(u5mr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qct <- apply(imr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.qct <- apply(deathu5.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.qct <- apply(death0.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  u5mr.ui <- imr.ui <- deathu5.ui <- death0.ui <- NULL
  for (c in 1:C) {
    u5mr.ui <- rbind(u5mr.ui, u5mr.qct[, c, ])
    imr.ui <- rbind(imr.ui, imr.qct[, c, ])
    deathu5.ui <- rbind(deathu5.ui, deathu5.qct[, c, ])
    death0.ui <- rbind(death0.ui, death0.qct[, c, ])
  }
  
  if(!is.null(nmr.ctj)){
    nmr.qct <- apply(nmr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    deathnn.qct <- apply(deathnn.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    nmr.ui <- deathnn.ui <- NULL
    for (c in 1:C) {
      nmr.ui <- rbind(nmr.ui, nmr.qct[, c, ])
      deathnn.ui <- rbind(deathnn.ui, deathnn.qct[, c, ])
    }
    colnames(nmr.ui) <- paste0("NMR ", est.years.floor)
    colnames(deathnn.ui) <- paste0("Neonatal Deaths ", est.years.floor)
  }
  # output to .csv
  colnames(u5mr.ui) <- paste0("U5MR ", est.years.floor)
  colnames(imr.ui) <- paste0("IMR ", est.years.floor)
  colnames(deathu5.ui) <- paste0("Under-five Deaths ", est.years.floor)
  colnames(death0.ui) <- paste0("Infant Deaths ", est.years.floor)
  country.info.output <- matrix(rep(unlist(country.info), each = 3), C*3, ncol(country.info))
  colnames(country.info.output) <- colnames(country.info)
  if (nsim == 1) {
    select.rows <- seq(1, nrow(u5mr.ui), 3)+1
  } else {
    select.rows <- seq(1, nrow(u5mr.ui), 1)
  }
  
  if(is.null(nmr.ctj)){
    write.csv(cbind(country.info.output,
                    rep(c("Lower", "Median", "Upper"), C),
                    roundoff(u5mr.ui, digits = ndigits), roundoff(imr.ui, digits = ndigits),
                    roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0))[select.rows, ],
              file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
              row.names = F, na = "")
  } else {
    write.csv(cbind(country.info.output,
                    rep(c("Lower", "Median", "Upper"), C),
                    roundoff(u5mr.ui, digits = ndigits), roundoff(imr.ui, digits = ndigits), roundoff(nmr.ui, digits = ndigits),
                    roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0), roundoff(deathnn.ui, digits = 0))[select.rows, ],
              file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
              row.names = F, na = "")    
  }
  
  #----------------------------------------------------------------------
  # output country summaries - ARR
  ARR.year1.year4.ui <- apply(ARR.year1.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  ARR.year1.year2.ui <- apply(ARR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  ARR.year2.year4.ui <- apply(ARR.year2.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  required.ARR.ui <- apply(required.ARR.cj, 1, quantile, probs = percentiles, na.rm = T)
  changeinARR.ui <- apply(changeinARR.cj, 1, quantile, probs = percentiles, na.rm = T)
  decline.year1.year4.ui <- apply(decline.year1.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  decline.year1.year2.ui <- apply(decline.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  decline.year2.year4.ui <- apply(decline.year2.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  country.RoDs.ui <- cbind(t(ARR.year1.year4.ui), t(ARR.year1.year2.ui), t(ARR.year2.year4.ui),
                           t(required.ARR.ui), t(changeinARR.ui), t(decline.year1.year4.ui),
                           t(decline.year1.year2.ui), t(decline.year2.year4.ui))
  # output to .csv
  ui.colnames <- c(" lower bound", " median", " upper bound")
  colnames(country.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                 paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                 paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                 paste0("Required ARR", ui.colnames),
                                 paste0("Change in ARR", ui.colnames),
                                 paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                 paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                 paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  if (nsim == 1)
    country.RoDs.ui <- country.RoDs.ui[, !grepl("bound", colnames(country.RoDs.ui))]
  write.csv(cbind(country.info, country.RoDs.ui),
            file = file.path(output.dir, "Rates of Decline_Country Summary.csv"),
            row.names = FALSE, na = "")
}
#----------------------------------------------------------------------
CalculateWorldDeathsBWC <- function(
  output.dir.samplescombined,
  output.dir.samples,
  output.dir,
  percentiles,
  ndigits
) {
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  
  nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  
  nsim <- dim(deathu5.ctj)[3]
  ## load country mortality rates for later death calc
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  ## wgt and year matrixes for later death calc
  # wgt.nmr.mat
  weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4))
  wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  # wgt.pnmr.mat 
  weight.pnmr.j.1 <- c(((53-(5:52))/48), rep(0,4))
  wgt.pnmr.mat <- t(matrix(cbind(weight.pnmr.j.1, 1-weight.pnmr.j.1), ncol=2, nrow=52))
  wgt.u1.mat <- rbind(wgt.nmr.mat[1,], wgt.pnmr.mat[1,], wgt.nmr.mat[2,], wgt.pnmr.mat[2,])
  # wgt.cmr.mat
  weight.cmr.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.cmr.mat <- t(matrix(cbind(weight.cmr.j.1, 1-weight.cmr.j.1), ncol=8, nrow=52))
  # wgt.mat
  wgt.mat <- rbind(wgt.u1.mat, wgt.cmr.mat)
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  years.mat <- rbind(years.mat[c(1,1,2,2),], years.mat[-c(1,2),])
  
  # Note: w stands for w, and w = 1
  # death0.wtj <- death1to4.wtj <- deathu5.wtj <-
  #   death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <-
  #   M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <-
  #   u5mr.wtj <- imr.wtj <- array(data = NA, c(1, nyears, nsim))
  
  death0.wtj <- death1to4.wtj <- deathu5.wtj <- deathnn.wtj <- 
    death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <- deathnn.all.wtj <-
    M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <- qnn.wtj <- qpnn.wtj <- 
    u5mr.wtj <- imr.wtj <- nmr.wtj <- array(data = NA, c(1, nyears, nsim))
  
  imr.dx.bwc1 <- imr.lx.bwc1 <- cmr.dx.bwc1 <- cmr.lx.bwc1 <- 
    nmr.dx.bwc1 <- nmr.lx.bwc1 <- pnmr.dx.bwc1 <- pnmr.lx.bwc1 <- array(data = NA, c(1, nyears, nsim))
  
  nyears <- ncol(u5mr.ctj)
  death0.temp.ct <- deathu5.temp.ct <- deathnn.temp.ct <- matrix(NA, C, nyears)
  
  pop0.wt <- pop1to4.wt <- pop0.orig.wt <- pop1to4.orig.wt <- popu5.orig.wt <-
    coverage0.wt <- coverageu5.wt <- matrix(NA, 1, nyears)
  
  getBWC <- function(bwc=NULL, year){
    if(is.null(bwc)){
      bwc <- 1:52
    }
    return(((year-1950)*52)+bwc)
  }
  
  removeNA <- T
  
  for (j in 1:nsim) {
    ## infant and u5 deaths BWC method
    load(file.path(output.dir.samples, paste0("dx.array.ct_",j,".rda")))
    load(file.path(output.dir.samples, paste0("lx.array.ct_",j,".rda")))
    
    for (i in 1:nyears) {
      # Check that population coverage > 50%
      pop0.wt[, i] <- sum(pop0.ct[, i])
      pop1to4.wt[, i] <- sum(pop1to4.ct[, i])
      pop0.orig.wt[, i] <- sum(pop0.orig.ct[, i])
      pop1to4.orig.wt[, i] <- sum(pop1to4.orig.ct[, i])
      popu5.orig.wt[, i] <- pop0.orig.wt[, i] + pop1to4.orig.wt[, i]
      coverage0.wt[, i] <- pop0.wt[, i]/pop0.orig.wt[, i]
      coverageu5.wt[, i] <- (pop0.wt[, i] + pop1to4.wt[, i])/(popu5.orig.wt[, i])
      # calculate deaths: sum deaths across countries ignoring NA
      death0.wtj[, i, j] <- sum(death0.ctj[, i, j], na.rm = T) 
      death1to4.wtj[, i, j] <- sum(death1to4.ctj[, i, j], na.rm = T)
      deathu5.wtj[, i, j] <- sum(deathu5.ctj[, i, j], na.rm = T)
      deathnn.wtj[, i, j] <- sum(deathnn.ctj[, i, j], na.rm = T)
      
      # calculate rates
      nmr.dx.bwc1[,i,j] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
      nmr.lx.bwc1[,i,j] <- sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA) 
      
      pnmr.dx.bwc1[,i,j] <- sum(dx.array.by.c[2,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
      pnmr.lx.bwc1[,i,j] <- sum(lx.array.by.c[2,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA) 
      
      if(i>1){
        cmr.dx.bwc1[,i,j] <- sum(dx.array.by.c[5,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)
        cmr.lx.bwc1[,i,j] <- sum(lx.array.by.c[5,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA) 
      }
      
      qnn.wtj[, i, j] <- nmr.dx.bwc1[,i,j]/nmr.lx.bwc1[,i,j]
      qpnn.wtj[, i, j] <- pnmr.dx.bwc1[,i,j]/pnmr.lx.bwc1[,i,j]
      q0.wtj[, i, j] <- 1-(1-qnn.wtj[, i, j])*(1-qpnn.wtj[, i, j])
      q1to4.wtj[, i, j] <- 1-((1-(cmr.dx.bwc1[,i,j]/cmr.lx.bwc1[,i,j]))^4)
      q5.wtj[, i, j] <- 1-(1-q0.wtj[, i, j])*(1-q1to4.wtj[, i, j])
      
    } #i loop years
    
    ## do BWC method again for all countries replacing missing rates with world rates, then sum deaths at world level for deathXX.all.wtj
    for(k in 1:dim(u5mr.ctj)[1]){
      u5mr.temp.ct <- u5mr.ctj[,,j]
      imr.temp.ct <- imr.ctj[,,j]
      nmr.temp.ct <- nmr.ctj[,,j]
      
      ## get live births for years.k
      wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
      #if(nrow(wpp.livebirths.k)<1) next
      bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
      
      for(ik in 1:length(years.k)){
        if(years.k[ik]+5>max(years.k)){
          # nmr; has IMR for year[i] and year[i+1] 
          nmx.nn.i <- nmr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
          nmx.nn.i[is.na(nmx.nn.i)] <- qnn.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.nn.i)],j]*1000
          # u1 mortality rates; has IMR for year[i] and year[i+1] 
          nmx.u1.i <- imr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
          nmx.u1.i[is.na(nmx.u1.i)] <- q0.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.u1.i)],j]*1000
          # u5 mortality rates; need same length as u1 to convert to 4q1
          nmx.u5.i <- u5mr.temp.ct[k,match(years.k[ik:length(years.k)],years)]
          nmx.u5.i[is.na(nmx.u5.i)] <- q5.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmx.u5.i)],j]*1000
          # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
          nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
          nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
          nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
          # pnn mortality rates; has pnmr
          nmx.pnmr.i <- ((nmx.u1.i-nmx.nn.i)/(1000-nmx.nn.i))*1000
          # combine appropriate rates in mortality rate vector
          nmx.i <- c(nmx.nn.i[1], nmx.pnmr.i[1], nmx.nn.i[2], nmx.pnmr.i[2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } else {
          # nmr; has IMR for year[i] and year[i+1] 
          nmx.nn.i <- nmr.temp.ct[k,match(years.k[ik]:years.k[ik+5],years)]
          nmx.nn.i[is.na(nmx.nn.i)] <- qnn.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.nn.i)],j]*1000
          # u1 mortality rates; has IMR for year[i] and year[i+1] 
          nmx.u1.i <- imr.temp.ct[k,match(years.k[ik]:(years.k[ik]+5),years)]
          nmx.u1.i[is.na(nmx.u1.i)] <- q0.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u1.i)],j]*1000
          # u5 mortality rates; need same length as u1 to convert to 4q1
          nmx.u5.i <- u5mr.temp.ct[k,match(years.k[ik]:(years.k[ik]+5),years)]
          nmx.u5.i[is.na(nmx.u5.i)] <- q5.wtj[,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u5.i)],j]*1000
          # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
          nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
          nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
          nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
          # pnn mortality rates; has pnmr
          nmx.pnmr.i <- ((nmx.u1.i-nmx.nn.i)/(1000-nmx.nn.i))*1000
          # combine in mortality rate vector for lifetable function
          nmx.i <- c(nmx.nn.i[1], nmx.pnmr.i[1], nmx.nn.i[2], nmx.pnmr.i[2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } #if/else 
        
        ## turn nmx.i into matrix
        ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=12, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=12, ncol=52)))
      } # i loop for nmx matrix
      
      ## get infant and u5 deaths
      wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=12, ncol=length(years.k)*52)
      nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
      npx.mat.k <- 1-nqx.mat.k
      lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
      dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
      
      years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=12, ncol=length(years.k)*52)
      years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
      
      for(yrk in min(years.k):max(years.k)){
        deathu5.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
        death0.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:4,][floor(years.k.mat[1:4,])==yrk], na.rm=T)
        deathnn.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[c(1,3),][floor(years.k.mat[c(1,3),])==yrk], na.rm=T)
      } # for loop for summing deaths
    } # k loop
    
    death0.all.wtj[,,j] <- apply(death0.temp.ct, 2, sum, na.rm=F)
    deathu5.all.wtj[,,j] <- apply(deathu5.temp.ct, 2, sum, na.rm=F)
    deathnn.all.wtj[,,j] <- apply(deathnn.temp.ct, 2, sum, na.rm=F)
  } # j loop nsim
  
  ## NA for columns (years) where BWC method doesn't have full count yet
  death0.all.wtj[,1:3,] <- NA 
  deathnn.all.wtj[,1:3,] <- NA
  deathu5.all.wtj[,1:8,] <- NA # first year of complete nmx schedule for deaths + 5
  
  u5mr.wtj <- q5.wtj*1000
  imr.wtj <- q0.wtj*1000
  nmr.wtj <- qnn.wtj*1000
  
  # world summary
  u5mr.qwt <- apply(u5mr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qwt <- apply(imr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  nmr.qwt <- apply(nmr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qwt <- apply(deathu5.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qwt <- apply(death0.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathnn.all.qwt <- apply(deathnn.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T) 
  
  # NA if coverage < 0.5 -- maybe use this to drop early years
  for (q in 1:length(percentiles)) {
    u5mr.qwt[q, , ][coverageu5.wt < 0.5] <- NA
    imr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    nmr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    deathu5.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1):(max(which(coverageu5.wt < 0.5))+5))] <- NA
    death0.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1))] <- NA
    deathnn.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1))] <- NA
  }
  
  # world summary
  res.world <- cbind(est.years.floor,
                     roundoff(t(popu5.orig.wt), digits = 0),
                     roundoff(t(pop0.orig.wt), digits = 0),
                     roundoff(t(coverageu5.wt)*100, digits = 1),
                     roundoff(t(coverage0.wt)*100, digits = 1),
                     roundoff(t(u5mr.qwt[, 1, ]), digits = ndigits),
                     roundoff(t(imr.qwt[, 1, ]), digits = ndigits),
                     roundoff(t(nmr.qwt[, 1, ]), digits = ndigits),
                     roundoff(t(deathu5.all.qwt[, 1, ]), digits = 0),
                     roundoff(t(death0.all.qwt[, 1, ]), digits = 0),
                     roundoff(t(deathnn.all.qwt[, 1, ]), digits = 0))
  ui.colnames <- c(" lower bound", " median", " upper bound")
  colnames(res.world) <- c("Year", "Under-five population", "Infant population",
                           "Population coverage (under 5)",
                           "Population coverage (age 0)",
                           paste0("U5MR", ui.colnames),
                           paste0("IMR", ui.colnames),
                           paste0("NMR", ui.colnames),
                           paste0("Under-five deaths", ui.colnames),
                           paste0("Infant deaths", ui.colnames),
                           paste0("Neonatal deaths", ui.colnames))
  
  save(res.world, file = file.path(output.dir.samplescombined, "res.world.rda"))
  if (nsim == 1) {
    res.world <- res.world[, !grepl("bound", colnames(res.world))]
  }
  write.csv(res.world, file = file.path(output.dir, "Rates & Deaths_World.csv"),
            row.names = F, na = "")
  # save all quantities # change JR, 26 Aug 2013
  save(u5mr.wtj, file = file.path(output.dir.samplescombined, "u5mr.wtj.rda"))
  save(imr.wtj, file = file.path(output.dir.samplescombined, "imr.wtj.rda"))
  save(nmr.wtj, file = file.path(output.dir.samplescombined, "nmr.wtj.rda"))
  save(deathnn.all.wtj, file = file.path(output.dir.samplescombined, "deathnn.all.wtj.rda"))
  save(deathu5.all.wtj, file = file.path(output.dir.samplescombined, "deathu5.all.wtj.rda"))
  save(death0.all.wtj, file = file.path(output.dir.samplescombined, "death0.all.wtj.rda"))
  save(pop0.wt, file = file.path(output.dir.samplescombined, "pop0.wt.rda"))
  save(pop1to4.wt, file = file.path(output.dir.samplescombined, "pop1to4.wt.rda"))
  save(pop0.orig.wt, file = file.path(output.dir.samplescombined, "pop0.orig.wt.rda"))
  save(pop1to4.orig.wt, file = file.path(output.dir.samplescombined, "pop1to4.orig.wt.rda"))
  save(popu5.orig.wt, file = file.path(output.dir.samplescombined, "popu5.orig.wt.rda"))
  save(coverage0.wt, file = file.path(output.dir.samplescombined, "coverage0.wt.rda"))
  save(coverageu5.wt, file = file.path(output.dir.samplescombined, "coverageu5.wt.rda"))
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.wtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.wtj <- roundoff(u5mr.wtj, digits = ndigits)
    imr.wtj <- roundoff(imr.wtj, digits = ndigits)
  }
  
  # world summary - rates of decline
  ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
                                    year.start = year1, year.end = year4)
  ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
                                    year.start = year1, year.end = year2)
  ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
                                    year.start = year2, year.end = year4)
  required.ARR.j <- ifelse(year4 < year.target,
                           1/(year.target-year4)*
                             log(roundoff(u5mr.wtj[1, est.years == year1, ]*factor.target, digits = ndigits)/
                                   u5mr.wtj[1, est.years == year4, ])*-100, NA)
  changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
  decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
                                            year.start = year1, year.end = year4)
  decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
                                            year.start = year1, year.end = year2)
  decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
                                            year.start = year2, year.end = year4)
  ARR.year1.year4.ui <- quantile(ARR.year1.year4.j, probs = percentiles)
  ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
  ARR.year2.year4.ui <- quantile(ARR.year2.year4.j, probs = percentiles)
  # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
  # indicating that year4 = year.target
  required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
                              na.rm = all(is.na(required.ARR.j)))
  changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
  decline.year1.year4.ui <- quantile(decline.year1.year4.j, probs = percentiles)
  decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
  decline.year2.year4.ui <- quantile(decline.year2.year4.j, probs = percentiles)
  global.RoDs.ui <- rbind(c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
                            required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
                            decline.year1.year2.ui, decline.year2.year4.ui))
  colnames(global.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                paste0("Required ARR", ui.colnames),
                                paste0("Change in ARR", ui.colnames),
                                paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  save(global.RoDs.ui, file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  if (nsim == 1) {
    global.RoDs.ui <- global.RoDs.ui[, !grepl("bound", colnames(global.RoDs.ui))]
    global.RoDs.ui.output <- rbind(colnames(global.RoDs.ui), global.RoDs.ui)
  } else {
    global.RoDs.ui.output <- cbind(data.frame(Region = "World"), global.RoDs.ui)
  }
  write.csv(global.RoDs.ui.output,
            file = file.path(output.dir, "Rates of Decline_World.csv"), row.names = F, na = "")
}
#----------------------------------------------------------------------
GetRegionalResultsBWC <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regions, regiontypes,
  filename,
  run.on.server,
  percentiles,
  ndigits
) {
  cat(paste0("Generating output for ", filename, "...\n"))
  nregs <- length(regiontypes)
  regions[is.na(regions)] <- 0 # to remove NAs
  
  load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  nsim <- dim(deathu5.ctj)[3]
  
  if (run.on.server) {
    # calculate once to get population arrays, because multiple chains will be running at once for parallel computing
    CalculateRegionalDeathsBWC(j = 1, output.dir.samples = output.dir.samples,
                               output.dir.samplescombined = output.dir.samplescombined,
                               regions = regions, regiontypes = regiontypes, filename = filename)
    cat(paste0("Output generated for trajectory ", 1, " out of ", nsim,
               ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    if (nsim > 1) {
      registerDoMC()
      foreach (j=2:nsim) %dopar% {
        CalculateRegionalDeathsBWC(j = j, output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined,
                                   regions = regions, regiontypes = regiontypes, filename = filename)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
  } else {
    for (j in 1:nsim) {
      CalculateRegionalDeathsBWC(j = j, output.dir.samples = output.dir.samples,
                                 output.dir.samplescombined = output.dir.samplescombined,
                                 regions = regions, regiontypes = regiontypes, filename = filename)
      cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                 ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    }
  }
  cat(paste0("Combining and outputting regional results...\n"))
  CombineAndOutputRegionalResults(output.dir = output.dir,
                                  output.dir.samples = output.dir.samples,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  regiontypes = regiontypes,
                                  filename = filename,
                                  percentiles = percentiles,
                                  ndigits = ndigits)
}
#----------------------------------------------------------------------
CalculateRegionalDeathsBWC <- function(
  j,
  output.dir.samples,
  output.dir.samplescombined,
  regions,
  regiontypes,
  filename
) {
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  
  nregs <- length(regiontypes)
  
  ## infant and u5 deaths BWC method
  load(file.path(output.dir.samples, paste0("dx.array.ct_",j,".rda")))
  load(file.path(output.dir.samples, paste0("lx.array.ct_",j,".rda")))
  
  ## load country mortality rates for later death calc
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  ## wgt and year matrixes for later death calc
  # wgt.nmr.mat
  weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4))
  wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  # wgt.pnmr.mat 
  weight.pnmr.j.1 <- c(((53-(5:52))/48), rep(0,4))
  wgt.pnmr.mat <- t(matrix(cbind(weight.pnmr.j.1, 1-weight.pnmr.j.1), ncol=2, nrow=52))
  wgt.u1.mat <- rbind(wgt.nmr.mat[1,], wgt.pnmr.mat[1,], wgt.nmr.mat[2,], wgt.pnmr.mat[2,])
  # wgt.cmr.mat
  weight.cmr.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.cmr.mat <- t(matrix(cbind(weight.cmr.j.1, 1-weight.cmr.j.1), ncol=8, nrow=52))
  # wgt.mat
  wgt.mat <- rbind(wgt.u1.mat, wgt.cmr.mat)
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  years.mat <- rbind(years.mat[c(1,1,2,2),], years.mat[-c(1,2),])
  
  getBWC <- function(bwc=NULL, year){
    if(is.null(bwc)){
      bwc <- 1:52
    }
    return(((year-1950)*52)+bwc)
  }
  
  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  dir.create(file.path(getwd(), output.dir.samples.region), showWarnings = FALSE)
  
  if (j == 1) { # calculate once
    pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
      coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  } else {
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
    load(file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
  }
  M0.rt <- M1to4.rt <- q0.rt <- q1to4.rt <- q5.rt <- qnn.rt <- qpnn.rt <- 
    death0.rt <- death1to4.rt <- deathu5.rt <- deathnn.rt <- death0.all.rt <- death1to4.all.rt <- deathu5.all.rt <- deathnn.all.rt <- matrix(NA, nregs, nyears)
  
  removeNA <- T
  
  for (r in 1:nregs) {
    if (filename %in% c("UNICEFRegion", "NewUnicef", "MDGRegion", "SDGRegion", "WBRegion", "UNPDRegion", "OICRegion", "M49Region")) {
      reg.num <- ChooseRegion(region = regiontypes[r], regiontype = filename)
      select.reg <- (1:nrow(regions))[regions[, is.element(colnames(regions),
                                                           paste0(filename, reg.num))] == regiontypes[r]]
    } else if (filename %in% c("WHORegion", "CountdownCountries", "ECAAfricaRegion",
                               "AURegion", "FragileCountries2013", "FragileCountries2014", "FragileCountries2015",
                               "USAIDCountries")) {
      select.reg <- (1:length(regions))[regions == regiontypes[r]]
    }
    if (j == 1) { # calculate the first time
      for (i in 1:nyears) {
        # check that population coverage > 50% per region
        pop0.rt[r, i] <- sum(pop0.ct[select.reg,i])
        pop1to4.rt[r, i] <- sum(pop1to4.ct[select.reg,i])
        pop0.orig.rt[r, i] <- sum(pop0.orig.ct[select.reg,i])
        pop1to4.orig.rt[r, i] <- sum(pop1to4.orig.ct[select.reg,i])
        popu5.orig.rt[r, i] <- pop0.orig.rt[r, i] + pop1to4.orig.rt[r, i]
        coverage0.rt[r, i] <- pop0.rt[r, i]/pop0.orig.rt[r, i]
        coverageu5.rt[r, i] <- (pop0.rt[r, i] + pop1to4.rt[r, i])/(popu5.orig.rt[r, i])
      }
      # save the first time
      save(pop0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.rt.rda")))
      save(pop1to4.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.rt.rda")))
      save(popu5.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
      save(pop0.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
      save(pop1to4.orig.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_pop1to4.orig.rt.rda")))
      save(coverageu5.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
      save(coverage0.rt, file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
    }
    for (i in 1:nyears) {
      # calculate deaths
      death0.rt[r, i] <- sum(death0.ctj[select.reg, i, j], na.rm = T)
      death1to4.rt[r, i] <- sum(death1to4.ctj[select.reg, i, j], na.rm = T)
      deathu5.rt[r, i] <- sum(deathu5.ctj[select.reg, i, j], na.rm = T)
      deathnn.rt[r, i] <- sum(deathnn.ctj[select.reg, i, j], na.rm = T)
      
      # calculate rates
      qnn.rt[r, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA) 
      
      qpnn.rt[r, i] <- sum(dx.array.by.c[2,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[2,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA) 
      
      q0.rt[r, i] <- 1-(1-qnn.rt[r, i])*(1-qpnn.rt[r, i])
      
      if(i>1){
        q1to4.rt[r, i] <- 1-((1-(sum(dx.array.by.c[5,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[5,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)))^4) 
      }
      
      q5.rt[r, i] <- 1-(1-q0.rt[r, i])*(1-q1to4.rt[r, i])
    }
    
    ## do BWC method again for all countries in region replacing missing rates with regional rates, then sum deaths at world level for deathXX.all.wtj
    u5mr.temp.rt <- u5mr.ctj[select.reg,,j]
    imr.temp.rt <- imr.ctj[select.reg,,j]
    nmr.temp.rt <- nmr.ctj[select.reg,,j]
    livebirths.rt <- livebirths.ct[select.reg,]
    
    deathu5.temp.rt <- death0.temp.rt <- deathnn.temp.rt <- matrix(NA, nrow(u5mr.temp.rt), nyears)
    
    for(k in 1:dim(u5mr.temp.rt)[1]){
      ## get live births for years.k
      wpp.livebirths.k <- as.numeric(livebirths.rt[k,match(years.k,years)])
      #if(nrow(wpp.livebirths.k)<1) next
      bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
      
      for(ik in 1:length(years.k)){
        if(years.k[ik]+5>max(years.k)){
          # nmr; has IMR for year[i] and year[i+1] 
          nmx.nn.i <- nmr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
          nmx.nn.i[is.na(nmx.nn.i)] <- qnn.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.nn.i)]]*1000
          # u1 mortality rates; has IMR for year[i] and year[i+1] 
          nmx.u1.i <- imr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
          nmx.u1.i[is.na(nmx.u1.i)] <- q0.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.u1.i)]]*1000
          # u5 mortality rates; need same length as u1 to convert to 4q1
          nmx.u5.i <- u5mr.temp.rt[k,match(years.k[ik:length(years.k)],years)]
          nmx.u5.i[is.na(nmx.u5.i)] <- q5.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmx.u5.i)]]*1000
          # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
          nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
          nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
          nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
          # pnn mortality rates; has pnmr
          nmx.pnmr.i <- ((nmx.u1.i-nmx.nn.i)/(1000-nmx.nn.i))*1000
          # combine appropriate rates in mortality rate vector
          nmx.i <- c(nmx.nn.i[1], nmx.pnmr.i[1], nmx.nn.i[2], nmx.pnmr.i[2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } else {
          # nmr; has IMR for year[i] and year[i+1] 
          nmx.nn.i <- nmr.temp.rt[k,match(years.k[ik]:years.k[ik+5],years)]
          nmx.nn.i[is.na(nmx.nn.i)] <- qnn.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.nn.i)]]*1000
          # u1 mortality rates; has IMR for year[i] and year[i+1] 
          nmx.u1.i <- imr.temp.rt[k,match(years.k[ik]:(years.k[ik]+5),years)]
          nmx.u1.i[is.na(nmx.u1.i)] <- q0.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u1.i)]]*1000
          # u5 mortality rates; need same length as u1 to convert to 4q1
          nmx.u5.i <- u5mr.temp.rt[k,match(years.k[ik]:(years.k[ik]+5),years)]
          nmx.u5.i[is.na(nmx.u5.i)] <- q5.rt[r,match(years.k[ik]:years.k[ik+5],years)[is.na(nmx.u5.i)]]*1000
          # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
          nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
          nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
          nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
          # pnn mortality rates; has pnmr
          nmx.pnmr.i <- ((nmx.u1.i-nmx.nn.i)/(1000-nmx.nn.i))*1000
          # combine in mortality rate vector for lifetable function
          nmx.i <- c(nmx.nn.i[1], nmx.pnmr.i[1], nmx.nn.i[2], nmx.pnmr.i[2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } #if/else 
        
        ## turn nmx.i into matrix
        ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=12, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=12, ncol=52)))
      } # ik loop for nmx matrix
      
      ## get infant and u5 deaths
      wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=12, ncol=length(years.k)*52)
      nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
      npx.mat.k <- 1-nqx.mat.k
      lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
      dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
      
      years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=12, ncol=length(years.k)*52)
      years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
      
      for(yrk in min(years.k):max(years.k)){
        deathu5.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
        death0.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[1:4,][floor(years.k.mat[1:4,])==yrk], na.rm=T)
        deathnn.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[c(1,3),][floor(years.k.mat[c(1,3),])==yrk], na.rm=T)
      } # for loop for summing deaths
    } # k loop for countries in the region
    
    death0.all.rt[r,] <- apply(death0.temp.rt,2,sum,na.rm=T)
    deathu5.all.rt[r,] <- apply(deathu5.temp.rt,2,sum,na.rm=T)
    deathnn.all.rt[r,] <- apply(deathnn.temp.rt,2,sum,na.rm=T)
    
    save(q0.rt, file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    save(q1to4.rt, file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    save(q5.rt, file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    save(death0.all.rt, file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    # save(death1to4.all.rt, file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    save(deathu5.all.rt, file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    save(qnn.rt, file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    save(deathnn.all.rt, file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
  }
}
#----------------------------------------------------------------------
CombineAndOutputRegionalResults <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regiontypes,
  filename,
  percentiles,
  ndigits
) {
  # load one file first to get dimensions
  load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  nsim <- dim(deathu5.ctj)[3]
  nregs <- length(regiontypes)
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  
  # load world results
  load(file = file.path(output.dir.samplescombined, "res.world.rda"))
  load(file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  
  # create separate directory for each region type
  output.dir.samples.region <- file.path(output.dir.samples, filename)
  
  death0.rtj <- death1to4.rtj <- deathu5.rtj <- deathnn.rtj <- death0.all.rtj <- death1to4.all.rtj <- deathu5.all.rtj <- deathnn.all.rtj <- 
    M0.rtj <- M1to4.rtj <- q0.rtj <- q1to4.rtj <- q5.rtj <- qnn.rtj <- u5mr.rtj <- imr.rtj <- nmr.rtj <- array(NA, c(nregs, nyears, nsim))
  
  pop0.rt <- pop1to4.rt <- pop0.orig.rt <- pop1to4.orig.rt <- popu5.orig.rt <-
    coverage0.rt <- coverageu5.rt <- matrix(NA, nregs, nyears)
  
  for (j in 1:nsim) {
    load(file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    #load(file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
    
    
    q0.rtj[ , , j] <- q0.rt
    q1to4.rtj[ , , j] <- q1to4.rt
    q5.rtj[ , , j] <- q5.rt
    qnn.rtj[ , , j] <- qnn.rt
    death0.all.rtj[ , , j] <- death0.all.rt
    #death1to4.all.rtj[ , , j] <- death1to4.all.rt
    deathu5.all.rtj[ , , j] <- deathu5.all.rt
    deathnn.all.rtj[ , , j] <- deathnn.all.rt
  }
  u5mr.rtj <- q5.rtj*1000
  imr.rtj <- q0.rtj*1000
  nmr.rtj <- qnn.rtj*1000
  
  # save the samples
  save(u5mr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_u5mr.rtj.rda")))
  save(imr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_imr.rtj.rda")))
  save(deathu5.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathu5.all.rtj.rda")))
  save(death0.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_death0.all.rtj.rda")))
  save(nmr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_nmr.rtj.rda")))
  save(deathnn.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathnn.all.rtj.rda")))
  
  
  # delete samples
  unlink(file.path(output.dir.samples.region, paste0("q0.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q1to4.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("q5.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("death0.all.rt_", 1:nsim, ".rda")))
  #unlink(file.path(output.dir.samples.region, paste0("death1to4.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("deathu5.all.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("qnn.rt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples.region, paste0("deathnn.all.rt_", 1:nsim, ".rda")))
  
  # load population and coverage info
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda"))  )
  
  # regional summaries
  u5mr.qrt <- apply(u5mr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qrt <- apply(imr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qrt <- apply(deathu5.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qrt <- apply(death0.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  nmr.qrt <- apply(nmr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathnn.all.qrt <- apply(deathnn.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  
  
  # NA if coverage < 0.5
  for (q in 1:length(percentiles)) {
    u5mr.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    imr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    nmr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    deathu5.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1):(max(which(coverageu5.rt < 0.5))+5))] <- NA
    death0.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    deathnn.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
  }
  
  # regional summary
  res.year <- NULL
  for (i in 1:nyears) {
    res.year <- rbind(res.year,
                      rbind(
                        cbind(est.years.floor[i],
                              roundoff(popu5.orig.rt[,i], digits = 0),
                              roundoff(pop0.orig.rt[,i], digits = 0),
                              roundoff(coverageu5.rt[,i]*100, digits = 2),
                              roundoff(coverage0.rt[,i]*100, digits = 2),
                              roundoff(t(u5mr.qrt[,,i]), digits = ndigits),
                              roundoff(t(imr.qrt[,,i]), digits = ndigits),
                              roundoff(t(nmr.qrt[,,i]), digits = ndigits),
                              roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                              roundoff(t(death0.all.qrt[,,i]), digits = 0),
                              roundoff(t(deathnn.all.qrt[,,i]), digits = 0)),
                        res.world[res.world[, 1] == est.years.floor[i], ]
                      )) 
  }
  res.region <- cbind(rep(c(regiontypes, "World"), nyears), res.year)
  # output to .csv
  ui.colnames <- c(" lower bound", " median", " upper bound")
  colnames(res.region) <- c("Region", "Year",
                            "Under-five population", "Infant population",
                            "Population coverage (under 5)", "Population coverage (age 0)",
                            paste0("U5MR", ui.colnames),
                            paste0("IMR", ui.colnames),
                            paste0("NMR", ui.colnames),
                            paste0("Under-five deaths", ui.colnames),
                            paste0("Infant deaths", ui.colnames),
                            paste0("Neonatal deaths", ui.colnames))
  
  if (nsim == 1) res.region <- res.region[, !grepl("bound", colnames(res.region))]
  write.csv(res.region, file = file.path(output.dir, paste0("Rates & Deaths_", filename, ".csv")),
            row.names = F, na = "")
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.rtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.rtj <- roundoff(u5mr.rtj, digits = ndigits)
    imr.rtj <- roundoff(imr.rtj, digits = ndigits)
  }
  # regional summary - rates of decline
  region.RoDs.ui <- NULL
  for (r in 1:nregs) {
    ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                      year.start = year1, year.end = year4)
    ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                      year.start = year1, year.end = year2)
    ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
                                      year.start = year2, year.end = year4)
    required.ARR.j <- ifelse(year4 < year.target,
                             1/(year.target-year4)*
                               log(roundoff(u5mr.rtj[r, est.years == year1, ]*factor.target, digits = ndigits)/
                                     u5mr.rtj[1, est.years == year4, ])*-100, NA)
    changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
    decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                              year.start = year1, year.end = year4)
    decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                              year.start = year1, year.end = year2)
    decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
                                              year.start = year2, year.end = year4)
    ARR.year1.year4.ui <- quantile(ARR.year1.year4.j, probs = percentiles)
    ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
    ARR.year2.year4.ui <- quantile(ARR.year2.year4.j, probs = percentiles)
    # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
    # indicating that year4 = year.target
    required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
                                na.rm = all(is.na(required.ARR.j)))
    changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
    decline.year1.year4.ui <- quantile(decline.year1.year4.j, probs = percentiles)
    decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
    decline.year2.year4.ui <- quantile(decline.year2.year4.j, probs = percentiles)
    region.RoDs.ui <- rbind(region.RoDs.ui,
                            c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
                              required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
                              decline.year1.year2.ui, decline.year2.year4.ui))
    
  }
  colnames(region.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
                                paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
                                paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
                                paste0("Required ARR", ui.colnames),
                                paste0("Change in ARR", ui.colnames),
                                paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
                                paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
                                paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  region.RoDs <- data.frame(Region = c(regiontypes, "World"), rbind(region.RoDs.ui, global.RoDs.ui))
  if (nsim == 1)
    region.RoDs <- region.RoDs[, !grepl("bound", colnames(region.RoDs))]
  write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline_", filename, ".csv")),
            row.names = F, na = "")
  cat(paste0("Output generated for ", filename, ".\n"))
}