
# outputaggregates.R
# Jin Rou New, 2012-2013
# David Sharrow, 2019

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
  regiontypes.select = c("UNICEFProg", "UNICEFReport", "MDG", "SDGSimple", "WHO", "WB", "UNPD", "OIC",
                         "Countdown", "ECAAfrica", "AU",
                         "Fragile2013", "Fragile2014", "Fragile2015", "Fragile2017", "Fragile2018",
                         "USAID", "M49", "Wealthdata", "Wealthall","GlobalStrategy"), ##<< Output regional aggregates for which region types? 
  ## Input a character vector of more than one of the possible options if desired.
  ## If \code{NULL}, output will not be generated at the region level.
  ### ---- note 7-27-17: Add new UNICEF regions?
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year4 = 2015.5, ##<< Last year used for ARR calculation.
  year.target = 2018.5, ##<< MDG target year.
  est.years = seq(1950.5, 2018.5, 1), ##<< Years of estimation.
  factor.target = 1/3, ##<< MDG target factor (Reduce to one third).
  percentiles = c(0.05, 0.5, 0.95), ##<< Vector of percentiles.
  ndigits = 1, ##<< Number of decimal places to use for analysis (e.g. to calcalate ARR, decline).
  output.dir = NULL, ##<< Output directory to save all results.
  get.world.results = TRUE, ## Should the world results be calculated? If running to get regional aggregate for replace, world results can be silenced 
  round.output = FALSE,
  replace.rates.reg="M49Region", # Regional Aggregate to use for replacing -- must be one of regiontypes.select
  replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]),  # regiontypes from aggregate (e.g. M49Region1) -- must be vector with 1 regional type for each country and types must be from replace.rates.reg, this argument is necessary for creating the country trajectories with missing rates replaced with regional, not used if these country trajectories already exist
  test = FALSE ##<< Use a subset of first 5 trajectories to test function; must be using trajectory files not just Results.csv
) {

  if (is.null(output.dir)) output.dir <- "output_numberofdeaths"
  output.dir.samples <- file.path(output.dir, "samples")
  output.dir.samplescombined <- file.path(output.dir, "samples_combined")
  dir.create(file.path(output.dir), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output.dir.samples), showWarnings = FALSE,recursive = TRUE)
  dir.create(file.path(output.dir.samplescombined), showWarnings = FALSE,recursive = TRUE)
  
  # DJS edit 2018-07-23 for using historical regional constant for aggregation 
  if(!is.null(replace.rates.reg)){
    if(file.exists(file.path(output.dir.samplescombined, paste0(replace.rates.reg, "_u5mr.rtj.rda")))){
      cat(paste0("World and regional aggregate results will be calculated with country deaths calculated with missing historical rates replaced with ", replace.rates.reg,"...\n" ))
    } else 
      cat(paste0("Aggregate results for ",replace.rates.reg, " do not exist yet. \n Must generate aggregate results for ",replace.rates.reg, " with replace.rates.reg=NULL first to get regional aggregate to use as replacement rates...\n" ))
  }

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
  country.info <- read.csv(file = country.info.file, header = T, stringsAsFactors = FALSE,
                           strip.white = T)
  country.info <- country.info[, !grepl("pop", colnames(country.info))]
  data.pop <- read.csv(file = population.file, header = T, stringsAsFactors = F, strip.white = T)
  data.a0 <- read.csv(file = data.a0.file, header = T, stringsAsFactors = F, strip.white = T)
  data.livebirths <- read.csv(file = livebirths.file, header = T, stringsAsFactors = F, strip.white = T)

  # reformat data (same country order)
  suppressWarnings({
    data.pop <- dplyr::inner_join(data.frame(ISO3Code = country.info$ISO3Code), data.pop, by = "ISO3Code")
    data.a0 <- dplyr::inner_join(data.frame(iso = country.info$ISO3Code), data.a0, by = "iso")
  })
  data.livebirths <- reshape(data=data.livebirths, idvar=c("country", "uncode", "sex"), timevar = "year", direction = "wide")
  data.livebirths <- data.livebirths[match(country.info$UNCode, data.livebirths$uncode),] 
  data.livebirths$iso <- country.info$ISO3Code[match(country.info$UNCode, data.livebirths$uncode)]
  
  if (any(is.na(data.a0$a0)))
    cat(paste0("Note that a0 is NA for ", paste(data.a0$iso[is.na(data.a0$a0)], collapse = ", "), ".\n"))

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
    filename.NMR <- "finalresults.jtc.Rda" ## check structure here to fit into BWC code
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
    rand.select <- sample(1:dim(u5mrfinal.ctj)[3], 5)
    u5mrfinal.ctj <- u5mrfinal.ctj[, , rand.select]
    imrfinal.ctj <- imrfinal.ctj[, , rand.select]
    if(exists("nmrfinal.ctj")) nmrfinal.ctj <- nmrfinal.ctj[, , rand.select]
  }
  nsim <- dim(u5mrfinal.ctj)[3]

  if (sum(!file.exists(file.path(output.dir.samplescombined, "u5mr.ctj.rda"),
                       file.path(output.dir.samplescombined, "imr.ctj.rda"))) > 0) { # change to > 1 for conditional that both U5MR and IMR files are present
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
    cat(paste0("Note that U5MR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    select.NA.c <- apply(imr.ctj[, , 1], 1, function(x) sum(!is.na(x)) == 0)
    cat(paste0("Note that IMR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    
    rownames(u5mr.ctj) <- rownames(imr.ctj) <- iso.c
    colnames(u5mr.ctj) <- colnames(imr.ctj) <- est.years.floor

    
    # save
    if(!dir.exists(output.dir.samplescombined)) dir.create(output.dir.samplescombined, recursive = TRUE)
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
    cat(paste0("Note that NMR estimates are NA for all years of estimation for ",
               paste(iso.c[select.NA.c], collapse = ", "), ".\n"))
    
    rownames(nmr.ctj) <- iso.c
    colnames(nmr.ctj) <- est.years.floor
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
  # get country results ------------------------------------------------------
  # 
  if(!is.null(nmr.ctj)){
    files.country <- c("death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda", "deathnn.ctj.rda",
                       # "dx.array.ctj.rda", "lx.array.ctj.rda", "dx.nn.array.ctj.rda", "lx.nn.array.ctj.rda",
                       "ARR.year1.year4.cj.rda", "ARR.year1.year2.cj.rda",
                       "ARR.year2.year4.cj.rda",
                       "decline.year1.year4.cj.rda", "decline.year1.year2.cj.rda",
                       "decline.year2.year4.cj.rda")
  } else {
    files.country <- c("death0.ctj.rda", "death1to4.ctj.rda", "deathu5.ctj.rda",
                       # "dx.array.ctj.rda", "lx.array.ctj.rda",
                       "ARR.year1.year4.cj.rda", "ARR.year1.year2.cj.rda",
                       "ARR.year2.year4.cj.rda",
                       "decline.year1.year4.cj.rda", "decline.year1.year2.cj.rda",
                       "decline.year2.year4.cj.rda")
  }

  if (sum(!file.exists(file.path(output.dir.samplescombined, files.country))) > 0) {
    cat(paste("Generating country results...\n"))
    if (run.on.server) {
      registerDoMC(cores=detectCores())
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
    CombineAndOutputCountryResultsBWC(u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                   country.info = country.info,
                                   percentiles = percentiles, ndigits = ndigits,
                                   output.dir = output.dir,
                                   output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined,
                                   round.output = round.output)
  } else {
    if(is.null(replace.rates.reg)){
    sapply(files.country, LoadFile, output.dir = output.dir.samplescombined,
           envir = environment())
    cat(paste("Country results loaded from ", output.dir.samplescombined, "\n"))
    } else {
      cat(paste("Country results without regional replacement not loaded. Calculating country deaths with regional replacement instead.\n")) 
    }
  }
  # get country results replacing missing rates with regional aggregate--------
  # 
  if(!is.null(nmr.ctj)){
    files.country.replace <- c(paste0("death0.ctj.", replace.rates.reg, "-replace.rda"),
                       paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda"),
                       paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda"),
                       paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")
                       # ,paste0("dx.array.ctj.", replace.rates.reg, "-replace.rda")
                       # ,paste0("lx.array.ctj.", replace.rates.reg, "-replace.rda")
                       # ,paste0("dx.nn.array.ctj.", replace.rates.reg, "-replace.rda")
                       # ,paste0("lx.nn.array.ctj.", replace.rates.reg, "-replace.rda")
                       )
  } else {
    files.country.replace <- c(paste0("death0.ctj.", replace.rates.reg, "-replace.rda"),
                       paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda"),
                       paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")
                       # ,paste0("dx.array.ctj.", replace.rates.reg, "-replace.rda")
                       # ,paste0("lx.array.ctj.", replace.rates.reg, "-replace.rda")
    )
  }
  file.check.agg.replace <- file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda"))
  if(!is.null(replace.rates.reg)&!file.exists(file.check.agg.replace)){
  # if (sum(!file.exists(file.path(output.dir.samplescombined, files.coun0try))) > 0) {
    cat(paste0("Generating country results (missing rates replaced with ", replace.rates.reg, ")...\n"))
    if (run.on.server) {
      registerDoMC(cores=detectCores())
      foreach (j = 1:nsim) %dopar% {
        CalculateCountryDeathsBWC.replacemissingrates(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                  livebirths.ct = lb.ct,
                                  a0.c = a0.c, a1to4.c = a1to4.c,
                                  pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                  iso.c = iso.c, est.years = est.years,
                                  year1 = year1, year2 = year2, year4 = year4,
                                  year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                  output.dir = output.dir.samples,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  replace.rates.reg=replace.rates.reg,
                                  replace.rates.cat=replace.rates.cat)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    } else {
      for (j in 1:nsim) {
        CalculateCountryDeathsBWC.replacemissingrates(j = j, u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                  livebirths.ct = lb.ct,
                                  a0.c = a0.c, a1to4.c = a1to4.c,
                                  pop0.orig.ct = pop0.orig.ct, pop1to4.orig.ct = pop1to4.orig.ct,
                                  iso.c = iso.c, est.years = est.years,
                                  year1 = year1, year2 = year2, year4 = year4,
                                  year.target = year.target, factor.target = factor.target, ndigits = ndigits,
                                  output.dir = output.dir.samples,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  replace.rates.reg=replace.rates.reg,
                                  replace.rates.cat=replace.rates.cat)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
    cat(paste0("Combining and outputting country results (missing rates replaced with ", replace.rates.reg, ")...\n"))
    CombineAndOutputCountryResultsBWC.replacemissingrates(u5mr.ctj = u5mr.ctj, imr.ctj = imr.ctj, nmr.ctj = nmr.ctj,
                                   country.info = country.info,
                                   percentiles = percentiles, ndigits = ndigits,
                                   output.dir = output.dir,
                                   output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined,
                                   replace.rates.reg=replace.rates.reg)
  } else {
    if(is.null(replace.rates.reg)){
      cat(paste0("Did not generate country results with missing rates replaced...\n"))
    } else {
    sapply(files.country.replace, LoadFile, output.dir = output.dir.samplescombined,
           envir = environment())
    cat(paste("Country results (with missing rates replaced with regional) loaded from ", output.dir.samplescombined, "\n"))
    }
  }
  # get world results ------------------------------------------------------
  # 
  if(get.world.results){
  if(is.null(nmr.ctj)){
    files.world <- c("res.world.rda", "global.RoDs.ui.rda", "u5mr.wtj.rda", "imr.wtj.rda",
                     "deathu5.all.wtj.rda", "death0.all.wtj.rda",
                     "pop0.wt.rda", "pop1to4.wt.rda",
                     "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                     #"livebirths.wt.rda",
                     "coverage0.wt.rda", "coverageu5.wt.rda") # change JR, 26 Aug 2013
  } else {
    files.world <- c("res.world.rda", "global.RoDs.ui.rda", "u5mr.wtj.rda", "imr.wtj.rda", "nmr.wtj.rda",
                     "deathu5.all.wtj.rda", "death0.all.wtj.rda", "deathnn.all.wtj.rda",
                     "pop0.wt.rda", "pop1to4.wt.rda",
                     "pop0.orig.wt.rda", "pop1to4.orig.wt.rda", "popu5.orig.wt.rda",
                     #"livebirths.wt.rda",
                     "coverage0.wt.rda", "coverageu5.wt.rda")
  }

  if (sum(!file.exists(file.path(output.dir.samplescombined, files.world))) > 0) {
    if(is.null(replace.rates.reg)){
    cat(paste("Generating world results...\n"))
    } else {
      cat(paste0("Generating world results with country deaths calculated with missing historical rates replaced with ",replace.rates.reg,"...\n"))  
    }
    # CalculateWorldDeaths(output.dir.samplescombined = output.dir.samplescombined, 
    #                      output.dir = output.dir,
    #                      percentiles = percentiles,
    #                      ndigits = ndigits)
    CalculateWorldDeathsBWC(output.dir.samplescombined = output.dir.samplescombined, 
                         output.dir.samples = output.dir.samples,
                         output.dir = output.dir,
                         percentiles = percentiles,
                         ndigits = ndigits,
                         run.on.server = run.on.server,
                         replace.rates.reg = replace.rates.reg,
                         round.output = round.output)
    
    cat(paste("Output generated for world.\n"))
  } else {
    sapply(files.world, LoadFile, output.dir = output.dir.samplescombined,
           envir = environment())
    cat(paste("World results loaded from ", output.dir.samplescombined, "\n"))
  }
} # if(get.world.results)
  # get regional results ---------------------------------------------------------
  # 
  if (!is.null(regiontypes.select)) {#PROBABLY DELETE THIS ONE
    cat(paste("Generating regional results...\n"))
    #'@param region_code0, a code to point to vector of regions (pre-defined in
    #'  `chooseregion.R`) It is also used as the string pattern in grepl() to
    #'  extract all related columns from country.info
    wrap.GetRegionalResultsBWC <- function(region_code0){
     my_list = list(
        "UNICEFProg" = UNICEFProgRegionAll,
        "UNICEFReport" = UNICEFReportRegionAll,
        "Wealthall" = WealthallRegionAll,
        "WealthallGlobal" = WealthallGlobalAll,
        "Wealthdata" = WealthdataRegionAll,
        "WealthdataGlobal" = WealthdataGlobalAll,
        "MDG" = MDGRegionAll,
        "SDG" = SDGRegionAll,
        "SDGSimple" = SDGSimpleRegionAll,
        "WHO" = WHORegionAll,
        "WB" = WBRegionAll,
        "UNPD" = UNPDRegionAll,
        "OIC" = OICRegionAll,
        "Countdown" = CountdownAll,
        "ECAAfrica" = ECAAfricaRegionAll,
        "AU" = AURegionAll,
        "Fragile2018OECD1" = Fragile2018OECD1All,
        "Fragile2018OECD2" = Fragile2018OECD2All,
        "ECA" = ECAAll,
        "GlobalStrategy" = GlobalStrategyAll,
        "M49" = M49RegionAll,
        "AfricanEconomicCommunity" = AfricanEconomicCommunityAll
         )
      region_types0 <- if (is.null(my_list[[region_code0]])) region_code0 else my_list[[region_code0]] 
      if(is.null(my_list[[region_code0]])) message("region_code0 is ", region_code0, "\n")
      # Default: if code = "M49", 
      # regions = country.info[, grepl("M49", colnames(country.info))],
      # filename = "M49Region"
      col_pattern <- region_code0
      file_name0 <- paste0(region_code0, "Region")
      
      # with exceptions:
      if (region_code0 == "AU") col_pattern <- "AURegion2"
      # e.g., GAVI, grepl("GAVICountries"), filename = "GAVICountries"
      if (region_code0 %in% c("ECA", "GAVI", "USAID", "Adhoc")) {
        col_pattern <- file_name0 <-  paste0(region_code0, "Countries")
        if (region_code0 == "USAID")  col_pattern = "USAIDcountry" # .... 
      }
      
      # for Fragile2012 to Fragile2018
      if(region_code0%in%paste0("Fragile", 2012:2018)){
        region_types0 <- c("Fragile", "Non-fragile")
        # e.g. FragileCountries2018
        col_pattern <- file_name0 <- paste0("FragileCountries", gsub("Fragile", "",  x))
      }
        
      if (!any(grepl(col_pattern, colnames(country.info), ignore.case = TRUE))){ 
        stop("Column pattern", col_pattern, "was not found in Country.CME.\n")}

      GetRegionalResultsBWC(regiontypes = region_types0, ## func at end; think about new regions
                            regions = country.info[, grepl(col_pattern, colnames(country.info), ignore.case = TRUE)],
                            filename = file_name0,
                            output.dir = output.dir, output.dir.samples = output.dir.samples,
                            output.dir.samplescombined = output.dir.samplescombined,
                            run.on.server = run.on.server,
                            percentiles = percentiles, ndigits = ndigits,
                            replace.rates.reg = replace.rates.reg,
                            round.output = round.output)
    } # end function: wrap.GetRegionalResultsBWC
    # run `GetRegionalResultsBWC` for every region in the `regiontypes.select`
    invisible(sapply(regiontypes.select, wrap.GetRegionalResultsBWC))
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
  # arr.ind.select.nmr <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]) | is.na(nmr.ctj[, , 1]), arr.ind = TRUE)
  pop0.ct[arr.ind.select] <- 0
  pop1to4.ct[arr.ind.select] <- 0
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # round off to 1 d.p. before calculation (for median only)
  # if (dim(u5mr.ctj)[3] == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(!is.null(nmr.ctj)) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  u5mr.ct <- u5mr.ctj[, , j]
  imr.ct <- imr.ctj[, , j]
  if(!is.null(nmr.ctj)){
    nmr.ct <- nmr.ctj[, , j]
  }
  # end DJS edit 2018-03-09
  
  C <- nrow(u5mr.ct)
  nyears <- ncol(u5mr.ct)
  ifelse(is.null(nmr.ctj), death0.ct <- death1to4.ct <- deathu5.ct <- matrix(NA, C, nyears), death0.ct <- death1to4.ct <- deathu5.ct <- deathnn.ct <- matrix(NA, C, nyears))
  
  
  # make matrixes for calculation
  years <- seq(1950,1950+ncol(u5mr.ctj)-1,1) ## put this elsewhere so can change 1950?
  dx.array.by.c <- array(NA, dim=c(3,length(years)*52,nrow(u5mr.ct)))
  lx.array.by.c <- array(NA, dim=c(3,length(years)*52,nrow(u5mr.ct)))
  if(!is.null(nmr.ctj)){
    dx.nn.array.by.c <- array(NA, dim=c(2,length(years)*52,nrow(u5mr.ct)))
    lx.nn.array.by.c <- array(NA, dim=c(2,length(years)*52,nrow(u5mr.ct)))
  }
  
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(!is.null(nmr.ctj)){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  
  # years.mat
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
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
    if(!is.null(nmr.ctj)) year1.est.nn[k] <- min(years[!is.na(as.numeric(nmr.ct[k,]))])
    
    ## get live births for years.k
    wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
    #if(nrow(wpp.livebirths.k)<1) next
    bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
    
    for(i in 1:length(years.k)){
      if(years.k[i]+5>max(years.k)){
        # u1 mortality rates; has IMR for year[i] and year[i+1] 
        nmx.u1.i <- imr.ct[k,match(years.k[i:length(years.k)],years)]
        # u5 mortality rates; need same length as u1 to convert to 4q1
        nmx.u5.i <- u5mr.ct[k,match(years.k[i:length(years.k)],years)]
        # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
        nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
        nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
        nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
        # combine appropriate rates in mortality rate vector
        nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
      } else {
        # u1 mortality rates; has IMR for year[i] and year[i+1] 
        nmx.u1.i <- imr.ct[k,match(years.k[i]:(years.k[i]+5),years)]
        # u5 mortality rates; need same length as u1 to convert to 4q1
        nmx.u5.i <- u5mr.ct[k,match(years.k[i]:(years.k[i]+5),years)]
        # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
        nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
        nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
        nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
        # combine in mortality rate vector for lifetable function
        nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
      } #if/else 
      
      ## turn nmx.i into matrix
      ifelse(i==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
      
      # nmr
      if(!is.null(nmr.ctj)){
        if(years.k[i]+1>max(years.k)){
          nmr.i <- c(nmr.ct[k,match(years.k[i],years)], NA)
        } else {
          nmr.i <- nmr.ct[k,match(years.k[i]:years.k[i+1],years)]
        } # if/else
        
        ## turn nmr.i into matrix
        ifelse(i==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
      }
    } # i loop for nmx matrix
    
    ## get infant and u5 deaths
    wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
    nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
    npx.mat.k <- 1-nqx.mat.k
    lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
    dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
    
    ## get nn deaths
    if(!is.null(nmr.ctj)){
      wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
      nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
      npx.nn.mat.k <- 1-nqx.nn.mat.k
      lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
      dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
    }
    
    ## save items for getting regional and world rates
    cols.k <- ((dim(dx.array.by.c)[2]-(length(years.k)*52))+1):(length(years)*52)
    dx.array.by.c[,cols.k,k] <- dx.mat.k[1:3,]
    lx.array.by.c[,cols.k,k] <- lx.mat.k[1:3,]
    if(!is.null(nmr.ctj)){
      dx.nn.array.by.c[,cols.k,k] <- dx.nn.mat.k[1:2,]
      lx.nn.array.by.c[,cols.k,k] <- lx.nn.mat.k[1:2,]
    }
    
    ## sum deaths by year
    years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
    years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
    for(yrk in (year1.est.u5[k]+5):max(years.k)){
      deathu5.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T) 
    } # for loop for u5 deaths
    for(yrk in (year1.est.u1[k]+1):max(years.k)){
      death0.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
    } # for loop for infant deaths 
    death1to4.ct[k,] <- deathu5.ct[k,]-death0.ct[k,]
    if(!is.null(nmr.ctj)){
      for(yrk in (year1.est.nn[k]+1):max(years.k)){  
        deathnn.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
      } ## for loop nn deaths
    } # if 
  } # k loop: countries
  
  # # set deaths to NA if rate data is not available, and for first 5 years for U5MR and 1 year for IMR and NMR
  # --- REPLACED in k loop with year1.est.XX to calculate deaths only at year1.est.nn+1 for neoanatal and infant deaths and year1.est.nn+5 for U5 deaths; need t-5 years of cohorts surviving for complete under-5 deaths with BWC
  # death0.ct[arr.ind.select] <- NA
  # death1to4.ct[arr.ind.select] <- NA
  # deathu5.ct[arr.ind.select] <- NA
  
  # calculate country rates of decline
  # ARR.year1.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  # ARR.year1.year2.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  # ARR.year2.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  # required.ARR.c <- ifelse(year4 < year.target,
  #                          1/(year.target-year4)*
  #                            log(roundoff(u5mr.ct[, est.years == year1]*factor.target, digits = ndigits)/
  #                                  u5mr.ct[, est.years == year4])*-100, NA)
  # changeinARR.c <- ARR.year2.year4.c - ARR.year1.year2.c
  # decline.year1.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  # decline.year1.year2.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  # decline.year2.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  # 
  if (!file.exists(file.path(output.dir, "info.rda"))) {
    info <- list(iso.c = iso.c,
                 C = C,
                 est.years = est.years,
                 est.years.floor = est.years-0.5,
                 year1.est.nn = year1.est.nn, ## will be NA if no nmr
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
    if(!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)
    save(info, file = file.path(output.dir, "info.rda"))
    cat(paste0("Information about the aggregates have been saved to ", output.dir, ".\n"))
  }
  # save samples
  save(death0.ct, file = file.path(output.dir, paste0("death0.ct_", j, ".rda")))
  save(death1to4.ct, file = file.path(output.dir, paste0("death1to4.ct_", j, ".rda")))
  save(deathu5.ct, file = file.path(output.dir, paste0("deathu5.ct_", j, ".rda")))
  save(dx.array.by.c, file = file.path(output.dir, paste0("dx.array.ct_", j, ".rda")))
  save(lx.array.by.c, file = file.path(output.dir, paste0("lx.array.ct_", j, ".rda")))
  if(!is.null(nmr.ctj)){
    save(deathnn.ct, file = file.path(output.dir, paste0("deathnn.ct_", j, ".rda")))
    save(dx.nn.array.by.c, file = file.path(output.dir, paste0("dx.nn.array.ct_", j, ".rda")))
    save(lx.nn.array.by.c, file = file.path(output.dir, paste0("lx.nn.array.ct_", j, ".rda")))
  }
  # save(ARR.year1.year4.c, file = file.path(output.dir, paste0("ARR.year1.year4.c_", j, ".rda")))
  # save(ARR.year1.year2.c, file = file.path(output.dir, paste0("ARR.year1.year2.c_", j, ".rda")))
  # save(ARR.year2.year4.c, file = file.path(output.dir, paste0("ARR.year2.year4.c_", j, ".rda")))
  # save(required.ARR.c, file = file.path(output.dir, paste0("required.ARR.c_", j, ".rda")))
  # save(changeinARR.c, file = file.path(output.dir, paste0("changeinARR.c_", j, ".rda")))
  # save(decline.year1.year4.c, file = file.path(output.dir, paste0("decline.year1.year4.c_", j, ".rda")))
  # save(decline.year1.year2.c, file = file.path(output.dir, paste0("decline.year1.year2.c_", j, ".rda")))
  # save(decline.year2.year4.c, file = file.path(output.dir, paste0("decline.year2.year4.c_", j, ".rda")))
}
#-------------------------------------------------------------------------
CalculateCountryDeathsBWC.replacemissingrates <- function( # DJS add 2018-07-24 for using defined historical regional rates to replace missing country rates -- this function requires existing regional aggregate output for replacement
  j, ##<< Index number of trajectory.
  u5mr.ctj,
  imr.ctj,
  nmr.ctj=NULL, # if NULL will only calculate U5 and U1 deaths; supplying NMR wil give nn deaths from seperate BWC calculation for NMR
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
  output.dir,
  output.dir.samplescombined,
  replace.rates.reg, # Regional Aggregate to use for replacing -- must be one of regiontypes.select
  replace.rates.cat  # Regional categories from aggregate (e.g. M49Region1) -- must be vector with 1 regional category for each country and categories must be from replace.rates.reg
) {
  pop0.ct <- pop0.orig.ct
  pop1to4.ct <- pop1to4.orig.ct
  # set population to 0 if rate data not available
  arr.ind.select <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]), arr.ind = TRUE)
  # arr.ind.select.nmr <- which(is.na(imr.ctj[, , 1]) | is.na(u5mr.ctj[, , 1]) | is.na(nmr.ctj[, , 1]), arr.ind = TRUE)
  pop0.ct[arr.ind.select] <- 0
  pop1to4.ct[arr.ind.select] <- 0
  
  nn.exists <- !is.null(nmr.ctj)
  
  regions.constant <- replace.rates.cat
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # round off to 1 d.p. before calculation (for median only)
  # if (dim(u5mr.ctj)[3] == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(!is.null(nmr.ctj)) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  u5mr.ct <- u5mr.ctj[, , j]
  imr.ct <- imr.ctj[, , j]
  if(nn.exists){
    nmr.ct <- nmr.ctj[, , j]
  }
  # end DJS edit 2018-03-09
  
  # get deaths for calculating replace.rates.reg
  load(file.path(output.dir.samplescombined, paste0(replace.rates.reg,"_u5mr.rtj.rda")))
  u5mr.replace.rt <- u5mr.rtj[,,j] # will have same columns as u5mr.ct
  rm(u5mr.rtj)
  rownames(u5mr.replace.rt) <- M49RegionAll
  load(file.path(output.dir.samplescombined, paste0(replace.rates.reg,"_imr.rtj.rda")))
  imr.replace.rt <- imr.rtj[,,j]
  rm(imr.rtj)
  if(nn.exists){
    load(file.path(output.dir.samplescombined, paste0(replace.rates.reg,"_nmr.rtj.rda")))
    nmr.replace.rt <- nmr.rtj[,,j]
    rm(nmr.rtj)
  }
  

  C <- nrow(u5mr.ct)
  nyears <- ncol(u5mr.ct)
  ifelse(is.null(nmr.ctj), death0.ct <- death1to4.ct <- deathu5.ct <- matrix(NA, C, nyears), death0.ct <- death1to4.ct <- deathu5.ct <- deathnn.ct <- matrix(NA, C, nyears))
  
  
  # make matrixes for calculation
  years <- seq(1950,1950+ncol(u5mr.ctj)-1,1) ## put this elsewhere so can change 1950?
  dx.array.by.c <- array(NA, dim=c(3,length(years)*52,nrow(u5mr.ct)))
  lx.array.by.c <- array(NA, dim=c(3,length(years)*52,nrow(u5mr.ct)))
  if(!is.null(nmr.ctj)){
    dx.nn.array.by.c <- array(NA, dim=c(2,length(years)*52,nrow(u5mr.ct)))
    lx.nn.array.by.c <- array(NA, dim=c(2,length(years)*52,nrow(u5mr.ct)))
  }
  
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(!is.null(nmr.ctj)){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  
  # years.mat
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
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
    if(!is.null(nmr.ctj)) year1.est.nn[k] <- min(years[!is.na(as.numeric(nmr.ct[k,]))])
  
    
    ## get live births for years.k
    wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years,years)])
    #if(nrow(wpp.livebirths.k)<1) next
    bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years,52))]
    years.k <- years
    for(i in 1:length(years.k)){
      # print(i)
      if(years.k[i]+5>max(years.k)){
        # u1 mortality rates; has IMR for year[i] and year[i+1] 
        nmx.u1.i <- imr.ct[k,match(years.k[i:length(years.k)],years)]
        nmx.u1.i[is.na(nmx.u1.i)] <- imr.replace.rt[which(M49RegionAll==regions.constant[k]),is.na(nmx.u1.i)]
        # u5 mortality rates; need same length as u1 to convert to 4q1
        nmx.u5.i <- u5mr.ct[k,match(years.k[i:length(years.k)],years)]
        nmx.u5.i[is.na(nmx.u5.i)] <- u5mr.replace.rt[which(M49RegionAll==regions.constant[k]),is.na(nmx.u5.i)]
        # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
        nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
        nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
        nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
        # combine appropriate rates in mortality rate vector
        nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
      } else {
        # u1 mortality rates; has IMR for year[i] and year[i+1] 
        nmx.u1.i <- imr.ct[k,match(years.k[i]:(years.k[i]+5),years)]
        nmx.u1.i[is.na(nmx.u1.i)] <- imr.replace.rt[which(M49RegionAll==regions.constant[k]),match(years.k[i]:(years.k[i]+5),years)[is.na(nmx.u1.i)]]
        # u5 mortality rates; need same length as u1 to convert to 4q1
        nmx.u5.i <- u5mr.ct[k,match(years.k[i]:(years.k[i]+5),years)]
        nmx.u5.i[is.na(nmx.u5.i)] <- u5mr.replace.rt[which(M49RegionAll==regions.constant[k]),match(years.k[i]:(years.k[i]+5),years)[is.na(nmx.u5.i)]]
        # 1 to 4 mortality rates; has 1 to 4 mortality rate for year[i+1] onward
        nmx.1to4.i <- ((nmx.u5.i-nmx.u1.i)/(1000-nmx.u1.i))*1000
        nmx.1to4.i.alt <- (1-((1-(nmx.1to4.i/1000))^(1/4)))*1000
        nmx.1to4.i.alt <- nmx.1to4.i.alt[-1]
        # combine in mortality rate vector for lifetable function
        nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
      } #if/else 
      
      ## turn nmx.i into matrix
      ifelse(i==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
      
      # nmr
      if(!is.null(nmr.ctj)){
        if(years.k[i]+1>max(years.k)){
          nmr.i <- c(nmr.ct[k,match(years.k[i],years)], NA)
          # if(is.na(nmr.i[1])){
          # nmr.i[1] <- nmr.replace.rt[which(M49RegionAll==regions.constant[k]),match(years.k[i],years)]
          # }
        } else {
          nmr.i <- nmr.ct[k,match(years.k[i]:years.k[i+1],years)]
          nmr.i[is.na(nmr.i)] <- nmr.replace.rt[which(M49RegionAll==regions.constant[k]),match(years.k[i]:years.k[i+1],years)[is.na(nmr.i)]]
        } # if/else
        
        ## turn nmr.i into matrix
        ifelse(i==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
      }
    } # i loop for nmx matrix
    
    ## get infant and u5 deaths
    wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
    nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
    npx.mat.k <- 1-nqx.mat.k
    lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
    dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
    
    ## get nn deaths
    if(!is.null(nmr.ctj)){
      wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
      nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
      npx.nn.mat.k <- 1-nqx.nn.mat.k
      lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
      dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
    }
    
    ## save items for getting regional and world rates
    cols.k <- ((dim(dx.array.by.c)[2]-(length(years.k)*52))+1):(length(years)*52)
    dx.array.by.c[,cols.k,k] <- dx.mat.k[1:3,]
    lx.array.by.c[,cols.k,k] <- lx.mat.k[1:3,]
    if(!is.null(nmr.ctj)){
      dx.nn.array.by.c[,cols.k,k] <- dx.nn.mat.k[1:2,]
      lx.nn.array.by.c[,cols.k,k] <- lx.nn.mat.k[1:2,]
    }
    
    ## sum deaths by year
    years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
    years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
    for(yrk in (years.k[1]+5):max(years.k)){
      deathu5.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T) 
    } # for loop for u5 deaths
    for(yrk in (years.k[1]+1):max(years.k)){
      death0.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
    } # for loop for infant deaths 
    death1to4.ct[k,] <- deathu5.ct[k,]-death0.ct[k,]
    if(!is.null(nmr.ctj)){
      for(yrk in (years.k[1]++1):max(years.k)){  
        deathnn.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
      } ## for loop nn deaths
    } # if 
  } # k loop: countries
  
  # # set deaths to NA if rate data is not available, and for first 5 years for U5MR and 1 year for IMR and NMR
  # --- REPLACED in k loop with year1.est.XX to calculate deaths only at year1.est.nn+1 for neoanatal and infant deaths and year1.est.nn+5 for U5 deaths; need t-5 years of cohorts surviving for complete under-5 deaths with BWC
  # death0.ct[arr.ind.select] <- NA
  # death1to4.ct[arr.ind.select] <- NA
  # deathu5.ct[arr.ind.select] <- NA
  
  # calculate country rates of decline
  # ARR.year1.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  # ARR.year1.year2.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  # ARR.year2.year4.c <- CalculateARR(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  # required.ARR.c <- ifelse(year4 < year.target,
  #                          1/(year.target-year4)*
  #                            log(roundoff(u5mr.ct[, est.years == year1]*factor.target, digits = ndigits)/
  #                                  u5mr.ct[, est.years == year4])*-100, NA)
  # changeinARR.c <- ARR.year2.year4.c - ARR.year1.year2.c
  # decline.year1.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year4)
  # decline.year1.year2.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year1, year.end = year2)
  # decline.year2.year4.c <- CalculateDecline(u5mr = u5mr.ct, years = est.years, year.start = year2, year.end = year4)
  
  # if (!file.exists(file.path(output.dir, "info.rda"))) {
  #   info <- list(iso.c = iso.c,
  #                C = C,
  #                est.years = est.years,
  #                est.years.floor = est.years-0.5,
  #                year1.est.nn = year1.est.nn, ## will be NA if no nmr
  #                year1.est.u1 = year1.est.u1,
  #                year1.est.u5 = year1.est.u5,
  #                nyears = nyears,
  #                a0.c = a0.c,
  #                a1to4.c = a1to4.c,
  #                pop0.ct = pop0.ct,
  #                pop1to4.ct = pop1to4.ct,
  #                pop0.orig.ct = pop0.orig.ct,
  #                pop1to4.orig.ct = pop1to4.orig.ct,
  #                livebirths.ct = livebirths.ct,
  #                year1 = year1,
  #                year2 = year2,
  #                year4 = year4,
  #                year.target = year.target,
  #                factor.target = factor.target)
  #   save(info, file = file.path(output.dir, "info.rda"))
  #   cat(paste0("Information about the aggregates have been saved to ", output.dir, ".\n"))
  # }
  # save samples
  save(death0.ct, file = file.path(output.dir, paste0("death0.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(death1to4.ct, file = file.path(output.dir, paste0("death1to4.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(deathu5.ct, file = file.path(output.dir, paste0("deathu5.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(dx.array.by.c, file = file.path(output.dir, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  save(lx.array.by.c, file = file.path(output.dir, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  if(!is.null(nmr.ctj)){
    save(deathnn.ct, file = file.path(output.dir, paste0("deathnn.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    save(dx.nn.array.by.c, file = file.path(output.dir, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    save(lx.nn.array.by.c, file = file.path(output.dir, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
  }
  # save(ARR.year1.year4.c, file = file.path(output.dir, paste0("ARR.year1.year4.c_", j, ".rda")))
  # save(ARR.year1.year2.c, file = file.path(output.dir, paste0("ARR.year1.year2.c_", j, ".rda")))
  # save(ARR.year2.year4.c, file = file.path(output.dir, paste0("ARR.year2.year4.c_", j, ".rda")))
  # save(required.ARR.c, file = file.path(output.dir, paste0("required.ARR.c_", j, ".rda")))
  # save(changeinARR.c, file = file.path(output.dir, paste0("changeinARR.c_", j, ".rda")))
  # save(decline.year1.year4.c, file = file.path(output.dir, paste0("decline.year1.year4.c_", j, ".rda")))
  # save(decline.year1.year2.c, file = file.path(output.dir, paste0("decline.year1.year2.c_", j, ".rda")))
  # save(decline.year2.year4.c, file = file.path(output.dir, paste0("decline.year2.year4.c_", j, ".rda")))
}
#-------------------------------------------------------------------------
CombineAndOutputCountryResultsBWC <- function(
  u5mr.ctj,
  imr.ctj,
  nmr.ctj=NULL,
  country.info,
  percentiles,
  ndigits,
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  round.output
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
  
  # lx.array.ctj <- dx.array.ctj <- array(NA, dim=c(3, nyears*52, C, nsim))
  # if(!is.null(nmr.ctj)) lx.nn.array.ctj <- dx.nn.array.ctj <- array(NA, dim=c(2, nyears*52, C, nsim))
  
  for (j in 1:nsim) {
    load(file.path(output.dir.samples, paste0("death0.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("death1to4.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("deathu5.ct_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("ARR.year1.year4.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("ARR.year1.year2.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("ARR.year2.year4.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("required.ARR.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("changeinARR.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("decline.year1.year4.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("decline.year1.year2.c_", j, ".rda")))
    # load(file.path(output.dir.samples, paste0("decline.year2.year4.c_", j, ".rda")))
    #load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    #load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    death0.ctj[, , j] <- death0.ct
    death1to4.ctj[, , j] <- death1to4.ct
    deathu5.ctj[, , j] <- deathu5.ct
    # ARR.year1.year4.cj[, j] <- ARR.year1.year4.c
    # ARR.year1.year2.cj[, j] <- ARR.year1.year2.c
    # ARR.year2.year4.cj[, j] <- ARR.year2.year4.c
    # required.ARR.cj[, j] <- required.ARR.c
    # changeinARR.cj[, j] <- changeinARR.c
    # decline.year1.year4.cj[, j] <- decline.year1.year4.c
    # decline.year1.year2.cj[, j] <- decline.year1.year2.c
    # decline.year2.year4.cj[, j] <- decline.year2.year4.c
    # dx.array.ctj[,,,j] <- dx.array.by.c
    # lx.array.ctj[,,,j] <- lx.array.by.c
    if(!is.null(nmr.ctj)){
      load(file.path(output.dir.samples, paste0("deathnn.ct_", j, ".rda")))
      deathnn.ctj[, , j] <- deathnn.ct
      # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
      # dx.nn.array.ctj[,,,j] <- dx.nn.array.by.c
      # lx.nn.array.ctj[,,,j] <- lx.nn.array.by.c
    }
  }
  
  rownames(death0.ctj) <- rownames(deathu5.ctj) <- rownames(death1to4.ctj) <- rownames(ARR.year1.year4.cj) <- rownames(ARR.year1.year2.cj) <- rownames(ARR.year2.year4.cj) <- rownames(required.ARR.cj) <- rownames(changeinARR.cj) <- rownames(decline.year1.year4.cj) <- rownames(decline.year1.year2.cj) <- rownames(decline.year2.year4.cj) <- iso.c

  colnames(death0.ctj) <- colnames(death1to4.ctj) <- colnames(deathu5.ctj) <- est.years.floor
 
   if(exists("deathnn.ctj")){
    rownames(deathnn.ctj) <- iso.c
    colnames(deathnn.ctj) <- est.years.floor
  }
  
  # save combined results
  save(death0.ctj, file = file.path(output.dir.samplescombined, "death0.ctj.rda"))
  save(death1to4.ctj, file = file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  save(deathu5.ctj, file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  # save(ARR.year1.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year4.cj.rda"))
  # save(ARR.year1.year2.cj, file = file.path(output.dir.samplescombined, "ARR.year1.year2.cj.rda"))
  # save(ARR.year2.year4.cj, file = file.path(output.dir.samplescombined, "ARR.year2.year4.cj.rda"))
  # save(required.ARR.cj, file = file.path(output.dir.samplescombined, "required.ARR.cj.rda"))
  # save(changeinARR.cj, file = file.path(output.dir.samplescombined, "changeinARR.cj.rda"))
  # save(decline.year1.year4.cj, file = file.path(output.dir.samplescombined, "decline.year1.year4.cj.rda"))
  # save(decline.year1.year2.cj, file = file.path(output.dir.samplescombined, "decline.year1.year2.cj.rda"))
  # save(decline.year2.year4.cj, file = file.path(output.dir.samplescombined, "decline.year2.year4.cj.rda"))
  # save(dx.array.ctj, file = file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
  # save(lx.array.ctj, file = file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
  save(info, file = file.path(output.dir.samplescombined, "info.rda"))
  
  if(!is.null(nmr.ctj)){
    save(deathnn.ctj, file = file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    # save(dx.nn.array.ctj, file = file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"))
    # save(lx.nn.array.ctj, file = file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"))
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
  # unlink(file.path(output.dir.samples, paste0("dx.array.ct_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("lx.array.ct_", 1:nsim, ".rda")))
  
  if(!is.null(nmr.ctj)){
    unlink(file.path(output.dir.samples, paste0("deathnn.ct_", 1:nsim, ".rda")))
    # unlink(file.path(output.dir.samples, paste0("dx.nn.array.ct_", 1:nsim, ".rda")))
    # unlink(file.path(output.dir.samples, paste0("lx.nn.array.ct_", 1:nsim, ".rda")))
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
  
  ifelse(round.output, u5mr.ui <- roundoff(u5mr.ui, digits = ndigits), u5mr.ui <- u5mr.ui)
  ifelse(round.output, imr.ui <- roundoff(imr.ui, digits = ndigits), imr.ui <- imr.ui)
  if(!is.null(nmr.ctj)){
  ifelse(round.output, nmr.ui <- roundoff(nmr.ui, digits = ndigits), nmr.ui <- nmr.ui)
  }
  
  if(is.null(nmr.ctj)){
    write.csv(cbind(country.info.output,
                    rep(c("Lower", "Median", "Upper"), C),
                    u5mr.ui,
                    imr.ui,
                    roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0))[select.rows, ],
              file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
              row.names = F, na = "")
  } else {
    write.csv(cbind(country.info.output,
                    rep(c("Lower", "Median", "Upper"), C),
                    u5mr.ui,
                    imr.ui,
                    nmr.ui,
                    roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0), roundoff(deathnn.ui, digits = 0))[select.rows, ],
              file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
              row.names = F, na = "")    
  }
  
  #----------------------------------------------------------------------
  # output country summaries - ARR
  # ARR.year1.year4.ui <- apply(ARR.year1.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.year1.year2.ui <- apply(ARR.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  # ARR.year2.year4.ui <- apply(ARR.year2.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # required.ARR.ui <- apply(required.ARR.cj, 1, quantile, probs = percentiles, na.rm = T)
  # changeinARR.ui <- apply(changeinARR.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year1.year4.ui <- apply(decline.year1.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year1.year2.ui <- apply(decline.year1.year2.cj, 1, quantile, probs = percentiles, na.rm = T)
  # decline.year2.year4.ui <- apply(decline.year2.year4.cj, 1, quantile, probs = percentiles, na.rm = T)
  # country.RoDs.ui <- cbind(t(ARR.year1.year4.ui), t(ARR.year1.year2.ui), t(ARR.year2.year4.ui),
  #                          t(required.ARR.ui), t(changeinARR.ui), t(decline.year1.year4.ui),
  #                          t(decline.year1.year2.ui), t(decline.year2.year4.ui))
  # # output to .csv
  # ui.colnames <- c(" lower bound", " median", " upper bound")
  # colnames(country.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("Required ARR", ui.colnames),
  #                                paste0("Change in ARR", ui.colnames),
  #                                paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
  #                                paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                                paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  # if (nsim == 1)
  #   country.RoDs.ui <- country.RoDs.ui[, !grepl("bound", colnames(country.RoDs.ui))]
  # write.csv(cbind(country.info, country.RoDs.ui),
  #           file = file.path(output.dir, "Rates of Decline_Country Summary.csv"),
  #           row.names = FALSE, na = "")
}
#-------------------------------------------------------------------------
CombineAndOutputCountryResultsBWC.replacemissingrates <- function(
  u5mr.ctj,
  imr.ctj,
  nmr.ctj=NULL,
  country.info,
  percentiles,
  ndigits,
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  replace.rates.reg
) {
  load(file.path(output.dir.samples, "info.rda"))
  list2env(info, envir = environment())
  
  nsim <- as.numeric(dim(u5mr.ctj)[3])
  nyears <- as.numeric(dim(u5mr.ctj)[2])
  ncountry <- as.numeric(dim(u5mr.ctj)[1])
  # est.years.floor <- est.years-0.5
  
  # combine all the samples into their respective arrays
  cat(paste0("Generating arrays for deaths by country..."))
  if(is.null(nmr.ctj)){
    death0.ctj<-death1to4.ctj<-deathu5.ctj<-array(NA, dim=c(ncountry, nyears, nsim))
    rownames(death0.ctj) <- rownames(deathu5.ctj) <- rownames(death1to4.ctj)  <- iso.c
    colnames(death0.ctj) <- colnames(death1to4.ctj) <- colnames(deathu5.ctj)  <- est.years.floor
  } else {
    death0.ctj<-death1to4.ctj<-deathu5.ctj<-deathnn.ctj<-array(NA, dim=c(ncountry, nyears, nsim))
    rownames(death0.ctj) <- rownames(deathu5.ctj) <- rownames(death1to4.ctj) <- rownames(deathnn.ctj) <- iso.c
    colnames(death0.ctj) <- colnames(death1to4.ctj) <- colnames(deathu5.ctj) <- colnames(deathnn.ctj) <- est.years.floor
  }
  

  # cat(paste0("Generating arrays for lx and dx..."))
  # lx.array.ctj <- dx.array.ctj <- array(NA, dim=c(3, nyears*52, ncountry, nsim))
  # if(!is.null(nmr.ctj)) lx.nn.array.ctj <- dx.nn.array.ctj <- array(NA, dim=c(2, nyears*52, ncountry, nsim))
  
  for (j in 1:nsim) {
    print(paste0("processesing ",j," of ",nsim))
    load(file.path(output.dir.samples, paste0("death0.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samples, paste0("death1to4.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samples, paste0("deathu5.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    # load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    # load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    death0.ctj[, , j] <- death0.ct
    death1to4.ctj[, , j] <- death1to4.ct
    deathu5.ctj[, , j] <- deathu5.ct
    # dx.array.ctj[,,,j] <- dx.array.by.c
    # lx.array.ctj[,,,j] <- lx.array.by.c
    if(!is.null(nmr.ctj)){
      load(file.path(output.dir.samples, paste0("deathnn.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      deathnn.ctj[, , j] <- deathnn.ct
      # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      # dx.nn.array.ctj[,,,j] <- dx.nn.array.by.c
      # lx.nn.array.ctj[,,,j] <- lx.nn.array.by.c
    }
  }
  # save combined results
  save(death0.ctj, file = file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
  save(death1to4.ctj, file = file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
  save(deathu5.ctj, file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
  # save(dx.array.ctj, file = file.path(output.dir.samplescombined, paste0("dx.array.ctj.", replace.rates.reg, "-replace.rda")))
  # save(lx.array.ctj, file = file.path(output.dir.samplescombined, paste0("lx.array.ctj.", replace.rates.reg, "-replace.rda")))
  # save(info, file = file.path(output.dir.samplescombined, "info.rda"))
  
  if(!is.null(nmr.ctj)){
    save(deathnn.ctj, file = file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    # save(dx.nn.array.ctj, file = file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.", replace.rates.reg, "-replace.rda")))
    # save(lx.nn.array.ctj, file = file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.", replace.rates.reg, "-replace.rda")))
  }
  # delete samples
  unlink(file.path(output.dir.samples, paste0("death0.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  unlink(file.path(output.dir.samples, paste0("death1to4.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  # unlink(file.path(output.dir.samples, paste0("dx.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  # unlink(file.path(output.dir.samples, paste0("lx.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  
  if(!is.null(nmr.ctj)){
    unlink(file.path(output.dir.samples, paste0("deathnn.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
    # unlink(file.path(output.dir.samples, paste0("dx.nn.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
    # unlink(file.path(output.dir.samples, paste0("lx.nn.array.ct_", 1:nsim, "_", replace.rates.reg, "-replace.rda")))
  }
  #----------------------------------------------------------------------
  # # output country summaries
  # u5mr.qct <- apply(u5mr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # imr.qct <- apply(imr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # deathu5.qct <- apply(deathu5.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # death0.qct <- apply(death0.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  # u5mr.ui <- imr.ui <- deathu5.ui <- death0.ui <- NULL
  # for (c in 1:C) {
  #   u5mr.ui <- rbind(u5mr.ui, u5mr.qct[, c, ])
  #   imr.ui <- rbind(imr.ui, imr.qct[, c, ])
  #   deathu5.ui <- rbind(deathu5.ui, deathu5.qct[, c, ])
  #   death0.ui <- rbind(death0.ui, death0.qct[, c, ])
  # }
  # 
  # if(!is.null(nmr.ctj)){
  #   nmr.qct <- apply(nmr.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  #   deathnn.qct <- apply(deathnn.ctj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  #   nmr.ui <- deathnn.ui <- NULL
  #   for (c in 1:C) {
  #     nmr.ui <- rbind(nmr.ui, nmr.qct[, c, ])
  #     deathnn.ui <- rbind(deathnn.ui, deathnn.qct[, c, ])
  #   }
  #   colnames(nmr.ui) <- paste0("NMR ", est.years.floor)
  #   colnames(deathnn.ui) <- paste0("Neonatal Deaths ", est.years.floor)
  # }
  # # output to .csv
  # colnames(u5mr.ui) <- paste0("U5MR ", est.years.floor)
  # colnames(imr.ui) <- paste0("IMR ", est.years.floor)
  # colnames(deathu5.ui) <- paste0("Under-five Deaths ", est.years.floor)
  # colnames(death0.ui) <- paste0("Infant Deaths ", est.years.floor)
  # country.info.output <- matrix(rep(unlist(country.info), each = 3), C*3, ncol(country.info))
  # colnames(country.info.output) <- colnames(country.info)
  # if (nsim == 1) {
  #   select.rows <- seq(1, nrow(u5mr.ui), 3)+1
  # } else {
  #   select.rows <- seq(1, nrow(u5mr.ui), 1)
  # }
  # 
  # if(is.null(nmr.ctj)){
  #   write.csv(cbind(country.info.output,
  #                   rep(c("Lower", "Median", "Upper"), C),
  #                   roundoff(u5mr.ui, digits = ndigits), roundoff(imr.ui, digits = ndigits),
  #                   roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0))[select.rows, ],
  #             file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
  #             row.names = F, na = "")
  # } else {
  #   write.csv(cbind(country.info.output,
  #                   rep(c("Lower", "Median", "Upper"), C),
  #                   roundoff(u5mr.ui, digits = ndigits), roundoff(imr.ui, digits = ndigits), roundoff(nmr.ui, digits = ndigits),
  #                   roundoff(deathu5.ui, digits = 0), roundoff(death0.ui, digits = 0), roundoff(deathnn.ui, digits = 0))[select.rows, ],
  #             file = file.path(output.dir, "Rates & Deaths_Country Summary.csv"),
  #             row.names = F, na = "")    
  # }
}
#----------------------------------------------------------------------
CalculateWorldDeathsBWC <- function(
  output.dir.samplescombined,
  output.dir.samples,
  output.dir,
  percentiles,
  ndigits,
  run.on.server=run.on.server,
  replace.rates.reg,
  round.output
) {
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  
  nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  } else {
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined,paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    if(nn.exists) load(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
  } # if/else  replace.rates.reg
  
  nsim <- dim(deathu5.ctj)[3]
  
  ## load country mortality rates for later death calc -- these do not have missing rates replaced
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # round off to 1 d.p. before calculation (for median only)
  # if (nsim == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(!is.null(nmr.ctj)) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  # edit DJS 2018-03-09
  
  # edit DJS 2018-07-27 round deaths at country level before summing to region and world
  deathu5.ctj <- roundoff(deathu5.ctj, digits = 0)
  death0.ctj <- roundoff(death0.ctj, digits = 0)
  death1to4.ctj <- roundoff(death1to4.ctj, digits = 0)
  if(nn.exists) deathnn.ctj <- roundoff(deathnn.ctj, digits = 0)
  # edit DJS 2018-07-27
  
  # ## load country deaths and cohorts for later death calc
  # load(file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
  # load(file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
  # if(nn.exists) load(file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"))
  # if(nn.exists) load(file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"))
  
  ## wgt and year matrixes for later death calc
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(nn.exists){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  
  # Note: w stands for w, and w = 1
  # death0.wtj <- death1to4.wtj <- deathu5.wtj <-
  #   death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <-
  #   M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <-
  #   u5mr.wtj <- imr.wtj <- array(data = NA, c(1, nyears, nsim))
  
  death0.wtj <- death1to4.wtj <- deathu5.wtj <- deathnn.wtj <- 
    death0.all.wtj <- death1to4.all.wtj <- deathu5.all.wtj <- deathnn.all.wtj <-
    M0.wtj <- M1to4.wtj <- q0.wtj <- q1to4.wtj <- q5.wtj <- qnn.wtj <- 
    u5mr.wtj <- imr.wtj <- nmr.wtj <- array(data = NA, c(1, nyears, nsim))
  
  # imr.dx.bwc1 <- imr.lx.bwc1 <- cmr.dx.bwc1 <- cmr.lx.bwc1 <- 
  #   nmr.dx.bwc1 <- nmr.lx.bwc1 <- array(data = NA, c(1, nyears, nsim))
  
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
  
  for (i in 1:nyears) {
    # Check that population coverage > 50%
    pop0.wt[, i] <- sum(pop0.ct[, i])
    pop1to4.wt[, i] <- sum(pop1to4.ct[, i])
    pop0.orig.wt[, i] <- sum(pop0.orig.ct[, i])
    pop1to4.orig.wt[, i] <- sum(pop1to4.orig.ct[, i])
    popu5.orig.wt[, i] <- pop0.orig.wt[, i] + pop1to4.orig.wt[, i]
    coverage0.wt[, i] <- pop0.wt[, i]/pop0.orig.wt[, i]
    coverageu5.wt[, i] <- (pop0.wt[, i] + pop1to4.wt[, i])/(popu5.orig.wt[, i])
  }

  if(run.on.server){  
  # for (j in 1:nsim) {
  registerDoMC(cores=detectCores())
  foreach (j = 1:nsim) %dopar% {
    # dx.array.by.c <- dx.array.ctj[,,,j]
    # lx.array.by.c <- lx.array.ctj[,,,j]
    # if(nn.exists){
    # dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
    # lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
    # }
    
    if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
      load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
      if(nn.exists){
        load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
        load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
      }
    } else {
      load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
      load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
      if(nn.exists){
        load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
        load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
      }
    } # if/else is.null(replace.rates.reg)
    
    death0.wt <- deathu5.wt <- deathnn.wt <- 
      death0.all.wt <- deathu5.all.wt <- deathnn.all.wt <-
      q0.wt <- q1to4.wt <- q5.wt <- qnn.wt <- matrix(data = NA, 1, nyears)
    
    for (i in 1:nyears) {
      death0.wt[, i] <- sum(death0.ctj[, i, j], na.rm = T)
      deathu5.wt[, i] <- sum(deathu5.ctj[, i, j], na.rm = T)
      if(nn.exists) deathnn.wt[, i] <- sum(deathnn.ctj[, i, j], na.rm = T)
      
      q0.wt[, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
      if(i>1){
        q1to4.wt[, i]<- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)))^4)
      }
      q5.wt[, i] <- 1-(1-q0.wt[, i])*(1-q1to4.wt[, i])
      
      if(nn.exists){
        qnn.wt[, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
      }
    } #i loop years
    
    ## save qs and deaths for trajectories to combine outside jnsim loop
    save(death0.wt, file = file.path(output.dir.samples, paste0("death0.wt_", j, ".rda")))
    save(deathu5.wt, file = file.path(output.dir.samples, paste0("deathu5.wt_", j, ".rda")))
    if(nn.exists) save(deathnn.wt, file = file.path(output.dir.samples, paste0("deathnn.wt_", j, ".rda")))
    save(q0.wt, file = file.path(output.dir.samples, paste0("q0.wt_", j, ".rda")))
    #save(q1to4.wt, file = file.path(output.dir.samples, paste0("q1to4.wt_", j, ".rda")))
    save(q5.wt, file = file.path(output.dir.samples, paste0("q5.wt_", j, ".rda")))
    if(nn.exists) save(qnn.wt, file = file.path(output.dir.samples, paste0("qnn.wt_", j, ".rda")))
  } # j sim loop 1
  } else {
    for (j in 1:nsim) {
    # registerDoMC()
    # foreach (j = 1:nsim) %dopar% {
      # dx.array.by.c <- dx.array.ctj[,,,j]
      # lx.array.by.c <- lx.array.ctj[,,,j]
      # if(nn.exists){
      # dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
      # lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
      # }
      
      if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
      load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
      if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
        }
      } else {
        load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
        load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
        if(nn.exists){
          load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
          load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
        }
      } # if/else is.null(replace.rates.reg)
      
      death0.wt <- deathu5.wt <- deathnn.wt <- 
        death0.all.wt <- deathu5.all.wt <- deathnn.all.wt <-
        q0.wt <- q1to4.wt <- q5.wt <- qnn.wt <- matrix(data = NA, 1, nyears)
      
      for (i in 1:nyears) {
        death0.wt[, i] <- sum(death0.ctj[, i, j], na.rm = T)
        deathu5.wt[, i] <- sum(deathu5.ctj[, i, j], na.rm = T)
        if(nn.exists) deathnn.wt[, i] <- sum(deathnn.ctj[, i, j], na.rm = T)
        
        q0.wt[, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
        if(i>1){
          q1to4.wt[, i]<- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),], na.rm=removeNA)))^4)
        }
        q5.wt[, i] <- 1-(1-q0.wt[, i])*(1-q1to4.wt[, i])
        
        if(nn.exists){
          qnn.wt[, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=removeNA)
        }
      } #i loop years
      
      ## save qs and deaths for trajectories to combine outside jnsim loop
      save(death0.wt, file = file.path(output.dir.samples, paste0("death0.wt_", j, ".rda")))
      save(deathu5.wt, file = file.path(output.dir.samples, paste0("deathu5.wt_", j, ".rda")))
      if(nn.exists) save(deathnn.wt, file = file.path(output.dir.samples, paste0("deathnn.wt_", j, ".rda")))
      save(q0.wt, file = file.path(output.dir.samples, paste0("q0.wt_", j, ".rda")))
      #save(q1to4.wt, file = file.path(output.dir.samples, paste0("q1to4.wt_", j, ".rda")))
      save(q5.wt, file = file.path(output.dir.samples, paste0("q5.wt_", j, ".rda")))
      if(nn.exists) save(qnn.wt, file = file.path(output.dir.samples, paste0("qnn.wt_", j, ".rda")))
    } # j sim loop 1
  } # else - run.on.server
    
    ## combine trajectory files from j loop
    for(j in 1:nsim){
      load(file.path(output.dir.samples, paste0("death0.wt_", j, ".rda")))
      death0.wtj[,,j] <- death0.wt
      load(file.path(output.dir.samples, paste0("deathu5.wt_", j, ".rda")))
      deathu5.wtj[,,j] <- deathu5.wt
      
      load(file.path(output.dir.samples, paste0("q0.wt_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("q5.wt_", j, ".rda")))
      q0.wtj[,,j] <- q0.wt
      q5.wtj[,,j] <- q5.wt
      
      if(nn.exists){
        load(file.path(output.dir.samples, paste0("deathnn.wt_", j, ".rda")))
        load(file.path(output.dir.samples, paste0("qnn.wt_", j, ".rda")))
        deathnn.wtj[,,j] <- deathnn.wt
        qnn.wtj[,,j] <- qnn.wt
      } # if nn.exists
    }# j loop for combining
 
 if(is.null(replace.rates.reg)){
    ## do BWC method again for all countries replacing missing rates with world rates, then sum deaths at world level for deathXX.all.wtj
  if(run.on.server){ 
  # for (j in 1:nsim) {
  registerDoMC(cores=detectCores())
  foreach (j = 1:nsim) %dopar% {
    for(k in 1:dim(u5mr.ctj)[1]){
      u5mr.temp.ct <- u5mr.ctj[,,j]
      imr.temp.ct <- imr.ctj[,,j]
      if(nn.exists) nmr.temp.ct <- nmr.ctj[,,j]
      
      ## get live births for years.k
      wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
      #if(nrow(wpp.livebirths.k)<1) next
      bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
      
      for(ik in 1:length(years.k)){
        if(years.k[ik]+5>max(years.k)){
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
          # combine appropriate rates in mortality rate vector
          nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } else {
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
          # combine in mortality rate vector for lifetable function
          nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } #if/else 
        
        ## turn nmx.i into matrix
        ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
        if(nsim==1) nmx.mat.k <- roundoff(nmx.mat.k, digits=1)
        
        # nmr
        if(nn.exists){
          if(years.k[ik]+1>max(years.k)){
            nmr.i <- c(nmr.temp.ct[k,match(years.k[ik],years)], NA)
            nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)],j]*1000
          } else {
            nmr.i <- nmr.temp.ct[k,match(years.k[ik]:years.k[ik+1],years)]
            nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)],j]*1000
          } # if/else
          
          ## turn nmr.i into matrix
          ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
        if(nsim==1) nmr.mat.k <- roundoff(nmr.mat.k,digits=1)
        }
      } # i loop for nmx matrix
      
      ## get infant and u5 deaths
      wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
      nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
      npx.mat.k <- 1-nqx.mat.k
      lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
      dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
      
      ## get nn deaths
      if(nn.exists){
        wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
        nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
        npx.nn.mat.k <- 1-nqx.nn.mat.k
        lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
        dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
      }
      
      years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
      years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
      
      for(yrk in min(years.k):max(years.k)){
        deathu5.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
        death0.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
        if(nn.exists) deathnn.temp.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
      } # for loop for summing deaths
      # edit DJS 2018-07-27 round deaths at country level before summing to region and world
      deathu5.temp.ct <- roundoff(deathu5.temp.ct, digits = 0)
      death0.temp.ct <- roundoff(death0.temp.ct, digits = 0)
      if(nn.exists) deathnn.temp.ct <- roundoff(deathnn.temp.ct, digits = 0)
      # edit DJS 2018-07-27
    } # k loop
    
    # to remove LIE from regional aggregate calcualtions -- to implement in 2018
    death0.all.wt <- apply(death0.temp.ct[apply(!is.na(death0.ctj),1,sum)>0,], 2, sum, na.rm=F)
    deathu5.all.wt <- apply(deathu5.temp.ct[apply(!is.na(deathu5.ctj),1,sum)>0,], 2, sum, na.rm=F)
    if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct[apply(!is.na(deathnn.ctj),1,sum)>0,], 2, sum, na.rm=F)

    # death0.all.wt <- apply(death0.temp.ct, 2, sum, na.rm=F)
    # deathu5.all.wt <- apply(deathu5.temp.ct, 2, sum, na.rm=F)
    # if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct, 2, sum, na.rm=F)
    
    ## save deaths.all objects for combining outside j loop
    save(death0.all.wt, file = file.path(output.dir.samples, paste0("death0.all.wt_", j, ".rda")))
    save(deathu5.all.wt, file = file.path(output.dir.samples, paste0("deathu5.all.wt_", j, ".rda")))
    if(nn.exists) save(deathnn.all.wt, file = file.path(output.dir.samples, paste0("deathnn.all.wt_", j, ".rda")))
  } # j loop nsim 2
  } else {
    for (j in 1:nsim) {
    # registerDoMC()
    # foreach (j = 1:nsim) %dopar% {
      for(k in 1:dim(u5mr.ctj)[1]){
        u5mr.temp.ct <- u5mr.ctj[,,j]
        imr.temp.ct <- imr.ctj[,,j]
        if(nn.exists) nmr.temp.ct <- nmr.ctj[,,j]
        
        ## get live births for years.k
        wpp.livebirths.k <- as.numeric(livebirths.ct[k,match(years.k,years)])
        #if(nrow(wpp.livebirths.k)<1) next
        bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
        
        for(ik in 1:length(years.k)){
          if(years.k[ik]+5>max(years.k)){
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
            # combine appropriate rates in mortality rate vector
            nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
          } else {
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
            # combine in mortality rate vector for lifetable function
            nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
          } #if/else 
          
          ## turn nmx.i into matrix
          ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
          #nmx.mat.k <- roundoff(nmx.mat.k, digits = 1)
          # nmr
          if(nn.exists){
            if(years.k[ik]+1>max(years.k)){
              nmr.i <- c(nmr.temp.ct[k,match(years.k[ik],years)], NA)
              nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)],j]*1000
            } else {
              nmr.i <- nmr.temp.ct[k,match(years.k[ik]:years.k[ik+1],years)]
              nmr.i[is.na(nmr.i)] <- qnn.wtj[,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)],j]*1000
            } # if/else
            
            ## turn nmr.i into matrix
            ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
            # nmr.mat.k <- roundoff(nmr.mat.k, digits=1)
          }
        } # i loop for nmx matrix
        
        ## get infant and u5 deaths
        wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
        nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
        npx.mat.k <- 1-nqx.mat.k
        lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
        dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
        
        ## get nn deaths
        if(nn.exists){
          wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
          nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
          npx.nn.mat.k <- 1-nqx.nn.mat.k
          lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
          dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
        }
        
        years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
        years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
        
        for(yrk in min(years.k):max(years.k)){
          deathu5.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T)
          death0.temp.ct[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
          if(nn.exists) deathnn.temp.ct[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
        } # for loop for summing deaths
        # edit DJS 2018-07-27 round deaths at country level before summing to region and world
        deathu5.temp.ct <- roundoff(deathu5.temp.ct, digits = 0)
        death0.temp.ct <- roundoff(death0.temp.ct, digits = 0)
        if(nn.exists) deathnn.temp.ct <- roundoff(deathnn.temp.ct, digits = 0)
        # edit DJS 2018-07-27
      } # k loop
      
      
      # to remove LIE from world calculation -- to be implemented in 2018
      death0.all.wt <- apply(death0.temp.ct[apply(!is.na(death0.ctj),1,sum)>0,], 2, sum, na.rm=F)
      deathu5.all.wt <- apply(deathu5.temp.ct[apply(!is.na(deathu5.ctj),1,sum)>0,], 2, sum, na.rm=F)
      if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct[apply(!is.na(deathnn.ctj),1,sum)>0,], 2, sum, na.rm=F)
      # death0.all.wt <- apply(death0.temp.ct, 2, sum, na.rm=F)
      # deathu5.all.wt <- apply(deathu5.temp.ct, 2, sum, na.rm=F)
      # if(nn.exists) deathnn.all.wt <- apply(deathnn.temp.ct, 2, sum, na.rm=F)
      
      ## save deaths.all objects for combining outside j loop
      save(death0.all.wt, file = file.path(output.dir.samples, paste0("death0.all.wt_", j, ".rda")))
      save(deathu5.all.wt, file = file.path(output.dir.samples, paste0("deathu5.all.wt_", j, ".rda")))
      if(nn.exists) save(deathnn.all.wt, file = file.path(output.dir.samples, paste0("deathnn.all.wt_", j, ".rda")))
    } # j loop nsim 2
  }
  
  ## combine trajectory files from j loop
  for(j in 1:nsim){
    load(file.path(output.dir.samples, paste0("death0.all.wt_", j, ".rda")))
    death0.all.wtj[,,j] <- death0.all.wt
    load(file.path(output.dir.samples, paste0("deathu5.all.wt_", j, ".rda")))
    deathu5.all.wtj[,,j] <- deathu5.all.wt
    
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("deathnn.all.wt_", j, ".rda")))
      deathnn.all.wtj[,,j] <- deathnn.all.wt
    } # is nn.exists
  }# j loop for combining
   
   unlink(file.path(output.dir.samples, paste0("death0.all.wt_", 1:nsim, ".rda")))
   unlink(file.path(output.dir.samples, paste0("deathu5.all.wt_", 1:nsim, ".rda")))
   if(nn.exists){
     unlink(file.path(output.dir.samples, paste0("deathnn.all.wt_", 1:nsim, ".rda")))
     }
   
 } else {
   death0.all.wtj <- death0.wtj
   deathu5.all.wtj <- deathu5.wtj
   if(nn.exists){
     deathnn.all.wtj <- deathnn.wtj
   } # if nn.exists
 } # if replace.reg
  
  # delete samples
  unlink(file.path(output.dir.samples, paste0("death0.wt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("deathu5.wt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("death0.all.wt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples, paste0("deathu5.all.wt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("q0.wt_", 1:nsim, ".rda")))
  unlink(file.path(output.dir.samples, paste0("q5.wt_", 1:nsim, ".rda")))
  if(nn.exists){
    unlink(file.path(output.dir.samples, paste0("deathnn.wt_", 1:nsim, ".rda")))
    # unlink(file.path(output.dir.samples, paste0("deathnn.all.wt_", 1:nsim, ".rda")))
    unlink(file.path(output.dir.samples, paste0("qnn.wt_", 1:nsim, ".rda")))
  }
  
  ## NA for columns (years) where BWC method doesn't have full count yet
  death0.all.wtj[,1:3,] <- NA 
  deathnn.all.wtj[,1:3,] <- NA
  deathu5.all.wtj[,1:8,] <- NA # first year of complete nmx schedule for deaths + 5 years
  
  u5mr.wtj <- q5.wtj*1000
  imr.wtj <- q0.wtj*1000
  if(nn.exists) nmr.wtj <- qnn.wtj*1000
  
  # world summary
  u5mr.qwt <- apply(u5mr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qwt <- apply(imr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists) nmr.qwt <- apply(nmr.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qwt <- apply(deathu5.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qwt <- apply(death0.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists) deathnn.all.qwt <- apply(deathnn.all.wtj, c(1, 2), quantile, probs = percentiles, na.rm = T) 
  
  # NA if coverage < 0.5 and for some years beyond that for BWC method
  for (q in 1:length(percentiles)) {
    u5mr.qwt[q, , ][coverageu5.wt < 0.5] <- NA
    imr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    if(nn.exists) nmr.qwt[q, , ][coverage0.wt < 0.5] <- NA
    deathu5.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1):(max(which(coverageu5.wt < 0.5))+5))] <- NA
    death0.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1))] <- NA
    if(nn.exists) deathnn.all.qwt[q, , ][c(which(coverageu5.wt < 0.5), (max(which(coverageu5.wt < 0.5))+1))] <- NA
  }
  
  # world summary
  if(nn.exists){
    ifelse(round.output, t.u5mr.qwt <- roundoff(t(u5mr.qwt[, 1, ]), digits = ndigits), t.u5mr.qwt <- t(u5mr.qwt[, 1, ]))
    ifelse(round.output, t.imr.qwt <- roundoff(t(imr.qwt[, 1, ]), digits = ndigits),  t.imr.qwt <- t(imr.qwt[, 1, ]))
    ifelse(round.output, t.nmr.qwt <- roundoff(t(nmr.qwt[, 1, ]), digits = ndigits), t.nmr.qwt <- t(nmr.qwt[, 1, ]))
    
    res.world <- cbind(est.years.floor,
                       roundoff(t(popu5.orig.wt), digits = 0),
                       roundoff(t(pop0.orig.wt), digits = 0),
                       roundoff(t(coverageu5.wt)*100, digits = 1),
                       roundoff(t(coverage0.wt)*100, digits = 1),
                       t.u5mr.qwt,
                       t.imr.qwt,
                       t.nmr.qwt,
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
  } else {
    ifelse(round.output, t.u5mr.qwt <- roundoff(t(u5mr.qwt[, 1, ]), digits = ndigits), t.u5mr.qwt <- t(u5mr.qwt[, 1, ]))
    ifelse(round.output, t.imr.qwt <- roundoff(t(imr.qwt[, 1, ]), digits = ndigits),  t.imr.qwt <- t(imr.qwt[, 1, ]))
    
    res.world <- cbind(est.years.floor,
                       roundoff(t(popu5.orig.wt), digits = 0),
                       roundoff(t(pop0.orig.wt), digits = 0),
                       roundoff(t(coverageu5.wt)*100, digits = 1),
                       roundoff(t(coverage0.wt)*100, digits = 1),
                       t.u5mr.qwt,
                       t.imr.qwt,
                       roundoff(t(deathu5.all.qwt[, 1, ]), digits = 0),
                       roundoff(t(death0.all.qwt[, 1, ]), digits = 0))
    ui.colnames <- c(" lower bound", " median", " upper bound")
    colnames(res.world) <- c("Year", "Under-five population", "Infant population",
                             "Population coverage (under 5)",
                             "Population coverage (age 0)",
                             paste0("U5MR", ui.colnames),
                             paste0("IMR", ui.colnames),
                             paste0("Under-five deaths", ui.colnames),
                             paste0("Infant deaths", ui.colnames))
  }
  save(res.world, file = file.path(output.dir.samplescombined, "res.world.rda"))
  if (nsim == 1) {
    res.world <- res.world[, !grepl("bound", colnames(res.world))]
  }
  write.csv(res.world, file = file.path(output.dir, "Rates & Deaths_World.csv"),
            row.names = F, na = "")
  # save all quantities # change JR, 26 Aug 2013
  save(u5mr.wtj, file = file.path(output.dir.samplescombined, "u5mr.wtj.rda"))
  save(imr.wtj, file = file.path(output.dir.samplescombined, "imr.wtj.rda"))
  save(deathu5.all.wtj, file = file.path(output.dir.samplescombined, "deathu5.all.wtj.rda"))
  save(death0.all.wtj, file = file.path(output.dir.samplescombined, "death0.all.wtj.rda"))
  save(pop0.wt, file = file.path(output.dir.samplescombined, "pop0.wt.rda"))
  save(pop1to4.wt, file = file.path(output.dir.samplescombined, "pop1to4.wt.rda"))
  save(pop0.orig.wt, file = file.path(output.dir.samplescombined, "pop0.orig.wt.rda"))
  save(pop1to4.orig.wt, file = file.path(output.dir.samplescombined, "pop1to4.orig.wt.rda"))
  save(popu5.orig.wt, file = file.path(output.dir.samplescombined, "popu5.orig.wt.rda"))
  save(coverage0.wt, file = file.path(output.dir.samplescombined, "coverage0.wt.rda"))
  save(coverageu5.wt, file = file.path(output.dir.samplescombined, "coverageu5.wt.rda"))
  if(nn.exists){
    save(nmr.wtj, file = file.path(output.dir.samplescombined, "nmr.wtj.rda"))
    save(deathnn.all.wtj, file = file.path(output.dir.samplescombined, "deathnn.all.wtj.rda"))
  }
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.wtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.wtj <- roundoff(u5mr.wtj, digits = ndigits)
    imr.wtj <- roundoff(imr.wtj, digits = ndigits)
  }
  
  # world summary - rates of decline
  # ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year1, year.end = year4)
  # ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year1, year.end = year2)
  # ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                   year.start = year2, year.end = year4)
  # required.ARR.j <- ifelse(year4 < year.target,
  #                          1/(year.target-year4)*
  #                            log(roundoff(u5mr.wtj[1, est.years == year1, ]*factor.target, digits = ndigits)/
  #                                  u5mr.wtj[1, est.years == year4, ])*-100, NA)
  # changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
  # decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year1, year.end = year4)
  # decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year1, year.end = year2)
  # decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.wtj[1, , ], years = est.years,
  #                                           year.start = year2, year.end = year4)
  # ARR.year1.year4.ui <- quantile(ARR.year1.year4.j, probs = percentiles)
  # ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
  # ARR.year2.year4.ui <- quantile(ARR.year2.year4.j, probs = percentiles)
  # # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
  # # indicating that year4 = year.target
  # required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
  #                             na.rm = all(is.na(required.ARR.j)))
  # changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
  # decline.year1.year4.ui <- quantile(decline.year1.year4.j, probs = percentiles)
  # decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
  # decline.year2.year4.ui <- quantile(decline.year2.year4.j, probs = percentiles)
  # global.RoDs.ui <- rbind(c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
  #                           required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
  #                           decline.year1.year2.ui, decline.year2.year4.ui))
  # colnames(global.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("Required ARR", ui.colnames),
  #                               paste0("Change in ARR", ui.colnames),
  #                               paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  # save(global.RoDs.ui, file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  # if (nsim == 1) {
  #   global.RoDs.ui <- global.RoDs.ui[, !grepl("bound", colnames(global.RoDs.ui))]
  #   global.RoDs.ui.output <- rbind(colnames(global.RoDs.ui), global.RoDs.ui)
  # } else {
  #   global.RoDs.ui.output <- cbind(data.frame(Region = "World"), global.RoDs.ui)
  # }
  # write.csv(global.RoDs.ui.output,
  #           file = file.path(output.dir, "Rates of Decline_World.csv"), row.names = F, na = "")
}

# GetRegionalResultsBWC ------------------------------------------------
# which main wraps `CalculateRegionalDeathsBWC`
GetRegionalResultsBWC <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regions, 
  regiontypes,
  filename,
  run.on.server,
  percentiles,
  ndigits,
  replace.rates.reg,
  round.output# DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  cat(paste0("Generating output for ", filename, "...\n"))
  nregs <- length(regiontypes)
  regions[is.na(regions)] <- 0 # to remove NAs

  if(is.null(replace.rates.reg)){ # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
  load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  } else {
  load(file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda"))) 
  }
  nsim <- dim(deathu5.ctj)[3] #change back 20170818
  #nsim<-50
  # ## load dx and lx arrays once so not loaded at every j
  # load(file.path(output.dir.samplescombined, paste0("dx.array.ctj.rda")))
  # load(file.path(output.dir.samplescombined, paste0("lx.array.ctj.rda")))
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  # if(nn.exists){
  #   load(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  #   load(file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.rda")))
  # }
  
  if (run.on.server) {
    # calculate once to get population arrays, because multiple chains will be running at once for parallel computing
    CalculateRegionalDeathsBWC(j = 1, output.dir.samples = output.dir.samples,
                               output.dir.samplescombined = output.dir.samplescombined,
                               regions = regions, regiontypes = regiontypes, filename = filename,
                               replace.rates.reg = replace.rates.reg)
    cat(paste0("Output generated for trajectory ", 1, " out of ", nsim,
               ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    if (nsim > 1) {
   registerDoMC(cores = 6)
   #registerDoParallel(cores=6)
      foreach (j=1:nsim) %dopar% {
        CalculateRegionalDeathsBWC(j = j, output.dir.samples = output.dir.samples,
                                   output.dir.samplescombined = output.dir.samplescombined,
                                   regions = regions, regiontypes = regiontypes, filename = filename,
                                   replace.rates.reg = replace.rates.reg)
        cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                   ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
      }
    }
  } else {
    for (j in 1:nsim) {
      CalculateRegionalDeathsBWC(j = j, output.dir.samples = output.dir.samples,
                                 output.dir.samplescombined = output.dir.samplescombined,
                                 regions = regions, regiontypes = regiontypes, filename = filename,
                                 replace.rates.reg = replace.rates.reg)
      cat(paste0("Output generated for trajectory ", j, " out of ", nsim,
                 ifelse(nsim > 1, " trajectories.\n", " trajectory.\n")))
    }
  }
  cat(paste0("Combining and outputting regional results...\n"))
  CombineAndOutputRegionalResultsBWC(output.dir = output.dir,
                                  output.dir.samples = output.dir.samples,
                                  output.dir.samplescombined = output.dir.samplescombined,
                                  regiontypes = regiontypes,
                                  filename = filename,
                                  percentiles = percentiles,
                                  ndigits = ndigits,
                                  replace.rates.reg = replace.rates.reg,
                                  round.output = round.output)
}


#----------------------------------------------------------------------
CalculateRegionalDeathsBWC <- function(
  j,
  output.dir.samples,
  output.dir.samplescombined,
  regions,
  regiontypes,
  filename,
  replace.rates.reg # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  # load(file.path(output.dir.samplescombined, "info.rda"))
  # list2env(info, envir = environment())
  # load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
  # load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
  # load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  # if(file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))){
  #   load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  # }
  
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, "death0.ctj.rda"))
    load(file.path(output.dir.samplescombined, "death1to4.ctj.rda"))
    load(file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
    # 
    nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda")) 
    if(nn.exists){
      load(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
    }
  } else {
    load(file.path(output.dir.samplescombined, "info.rda"))
    list2env(info, envir = environment())
    load(file.path(output.dir.samplescombined, paste0("death0.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("death1to4.ctj.", replace.rates.reg, "-replace.rda")))
    load(file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    if(nn.exists){
      load(file.path(output.dir.samplescombined, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda")))
    } 
  } # if/else  replace.rates.reg
  
  # edit DJS 2018-07-27 round deaths at country level before summing to region and world
  deathu5.ctj <- roundoff(deathu5.ctj, digits = 0)
  death0.ctj <- roundoff(death0.ctj, digits = 0)
  death1to4.ctj <- roundoff(death1to4.ctj, digits = 0)
  if(nn.exists) deathnn.ctj <- roundoff(deathnn.ctj, digits = 0)
  # edit DJS 2018-07-27
  
  nregs <- length(regiontypes)
  
  # infant and u5 deaths BWC method
  ## load dx and lx arrays once so not loaded at every j
  # load(file.path(output.dir.samplescombined, paste0("dx.array.ctj.rda")))
  # load(file.path(output.dir.samplescombined, paste0("lx.array.ctj.rda")))
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  # if(nn.exists){
  #   load(file.path(output.dir.samplescombined, paste0("dx.nn.array.ctj.rda")))
  #   load(file.path(output.dir.samplescombined, paste0("lx.nn.array.ctj.rda")))
  # }
  # 
  # dx.array.by.c <- dx.array.ctj[,,,j]
  # lx.array.by.c <- lx.array.ctj[,,,j]
  # if(nn.exists){
  #   dx.nn.array.by.c <- dx.nn.array.ctj[,,,j]
  #   lx.nn.array.by.c <- lx.nn.array.ctj[,,,j]
  # }
  
  # nn.exists <- file.exists(file.path(output.dir.samplescombined, "deathnn.ctj.rda"))
  # load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
  # load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
  # if(nn.exists){
  # load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
  # load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
  # }
  
  if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
    }
  } else {
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, "_", replace.rates.reg, "-replace.rda")))
    if(nn.exists){
      load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
      load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, "_", replace.rates.reg, "-replace.rda"))) 
    }
  } # if/else is.null(replace.rates.reg)
  
  ## load country mortality rates for later death calc
  load(file.path(output.dir.samplescombined, "u5mr.ctj.rda"))
  load(file.path(output.dir.samplescombined, "imr.ctj.rda"))
  if(nn.exists) load(file.path(output.dir.samplescombined, "nmr.ctj.rda"))
  
  nsim <- dim(u5mr.ctj)[3]
  
  # edit DJS 2018-03-09 remove rounding for median calculation
  # if (nsim == 1) {
  #   u5mr.ctj <- roundoff(u5mr.ctj, digits = ndigits)
  #   imr.ctj <- roundoff(imr.ctj, digits = ndigits)
  #   if(nn.exists) nmr.ctj <- roundoff(nmr.ctj, digits = ndigits)
  # }
  # end edit DJS 2018-03-09
  
  ## wgt and year matrixes for later death calc
  # wgt.mat
  weight.j.1 <- (53-(1:52))/52 #weight for mortality rate in year 1
  wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
  if(nn.exists){
    # wgt.nmr.mat
    weight.nmr.j.1 <- c(rep(1,49), ((53-(50:52))/4)) #weight for mortality rate in year 1
    wgt.nmr.mat <- t(matrix(cbind(weight.nmr.j.1, 1-weight.nmr.j.1), ncol=2, nrow=52))
  }
  # years.mat
  years <- years.k <- seq(1950,1950+ncol(u5mr.ctj)-1,1)
  for(yr in years){
    ifelse(yr==1950, years.mat <- seq(yr+.5,yr+5,.5), years.mat <- cbind(years.mat, seq(yr+.5,yr+5,.5)))
  }
  
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
  M0.rt <- M1to4.rt <- q0.rt <- q1to4.rt <- q5.rt <- qnn.rt <- 
    death0.rt <- death1to4.rt <- deathu5.rt <- deathnn.rt <- death0.all.rt <- death1to4.all.rt <- deathu5.all.rt <- deathnn.all.rt <- matrix(NA, nregs, nyears)
  
  removeNA <- T
  
  which.no.rates <- which(apply(!is.na(u5mr.ctj),1,sum)<1)
  
  for (r in 1:nregs) {
    if (filename %in% c("UNICEFProgRegion", "UNICEFReportRegion", "MDGRegion", "SDGRegion", "SDGSimpleRegion", "WBRegion", "UNPDRegion", "OICRegion", "M49Region", "Wealthall", "Wealthdata")) {
      reg.num <- ChooseRegion(region = regiontypes[r], regiontype = filename)
      region.cols <- regions[, is.element(colnames(regions),
                                                           paste0(filename, reg.num)), drop = FALSE]
      if(ncol(region.cols) > 1) region.cols <- region.cols[, 1, drop = TRUE]  # take first column if multiple match
      select.reg <- (1:nrow(regions))[region.cols == regiontypes[r]]
      # to remove LIE from regional aggregate calcualtions -- to implement in 2018
      select.reg.og <- select.reg
      ifelse(is.na(match(which.no.rates,select.reg.og)), select.reg <- select.reg.og, select.reg <- select.reg.og[-match(which.no.rates,select.reg.og)])
    } else if (filename %in% c("WHORegion", "CountdownCountries", "ECAAfricaRegion","GlobalStrategyCountries",
                               "AURegion", "FragileCountries2013", "FragileCountries2014", "FragileCountries2015", 
                               "FragileCountries2017", "FragileCountries2018", "FragileCountries2018OECD1", "FragileCountries2018OECD2", "WealthallGlobal", "WealthdataGlobal", "WorldBankReg2", 
                               "NewWorldBank", "USAIDCountries", "AfricanEconomicCommunityRegion", "ECACountries", 
                               "GAVICountries", "AdhocCountries")) {
      select.reg <- (1:length(regions))[regions == regiontypes[r]]
      # to remove LIE from regional aggregate calcualtions -- to implement in 2018
      select.reg.og <- (1:length(regions))[regions == regiontypes[r]]
      ifelse(is.na(match(which.no.rates,select.reg.og)), select.reg <- select.reg.og, 
             select.reg <- select.reg.og[-match(which.no.rates, select.reg.og)])
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
      if(nn.exists) deathnn.rt[r, i] <- sum(deathnn.ctj[select.reg, i, j], na.rm = T)
      
      # calculate rates
      q0.rt[r, i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)
      
      if(i>1){
        q1to4.rt[r, i] <- 1-((1-(sum(dx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)/sum(lx.array.by.c[3,getBWC(year=floor(est.years)[i-1], bwc=1),select.reg], na.rm=removeNA)))^4) 
      }
      
      if(nn.exists){
        qnn.rt[r, i] <- sum(dx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA)/sum(lx.nn.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),select.reg], na.rm=removeNA) 
      }
      
      q5.rt[r, i] <- 1-(1-q0.rt[r, i])*(1-q1to4.rt[r, i])
    } # i loop for years
    
    if(is.null(replace.rates.reg)){ # DJS edit 2018-07-27 to include option to use regional aggregate to replace missing historical rates
    ## do BWC method again for all countries in region replacing missing rates with regional rates, then sum deaths at world level for deathXX.all.wtj
    u5mr.temp.rt <- u5mr.ctj[select.reg,,j]
    imr.temp.rt <- imr.ctj[select.reg,,j]
    if(nn.exists) nmr.temp.rt <- nmr.ctj[select.reg,,j]
    livebirths.rt <- livebirths.ct[select.reg,]
    
    deathu5.temp.rt <- death0.temp.rt <- deathnn.temp.rt <- matrix(NA, nrow(u5mr.temp.rt), nyears)
    
    for(k in 1:dim(u5mr.temp.rt)[1]){
      ## get live births for years.k
      wpp.livebirths.k <- as.numeric(livebirths.rt[k,match(years.k,years)])
      #if(nrow(wpp.livebirths.k)<1) next
      bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k,52))]
      
      for(ik in 1:length(years.k)){
        if(years.k[ik]+5>max(years.k)){
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
          # combine appropriate rates in mortality rate vector
          nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } else {
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
          # combine in mortality rate vector for lifetable function
          nmx.i <- c(nmx.u1.i[1:2], nmx.1to4.i.alt[1], rep(nmx.1to4.i.alt[2],2), rep(nmx.1to4.i.alt[3],2), rep(nmx.1to4.i.alt[4],2), nmx.1to4.i.alt[5])
        } #if/else 
        
        ## turn nmx.i into matrix
        ifelse(ik==1, nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52), nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52)))
        #if(nsim==1) nmx.mat.k <- roundoff(nmx.mat.k, digits=1)
        # nmr
        if(nn.exists){
          if(years.k[ik]+1>max(years.k)){
            nmr.i <- c(nmr.temp.rt[k,match(years.k[ik],years)], NA)
            nmr.i[is.na(nmr.i)] <- qnn.rt[r,match(years.k[ik:length(years.k)],years)[is.na(nmr.i)]]*1000
          } else {
            nmr.i <- nmr.temp.rt[k,match(years.k[ik]:years.k[ik+1],years)]
            nmr.i[is.na(nmr.i)] <- qnn.rt[r,match(years.k[ik]:years.k[ik+1],years)[is.na(nmr.i)]]*1000
          } # if/else
          
          ## turn nmr.i into matrix
          ifelse(ik==1, nmr.mat.k <- matrix(nmr.i, nrow=2, ncol=52), nmr.mat.k <- cbind(nmr.mat.k ,matrix(nmr.i, nrow=2, ncol=52)))
         # if(nsim==1) nmr.mat.k <- roundoff(nmr.mat.k,digits=1)
        }
      } # ik loop for nmx matrix
      
      ## get infant and u5 deaths
      wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
      nqx.mat.k <- wgt.mat.k*(nmx.mat.k/1000)
      npx.mat.k <- 1-nqx.mat.k
      lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k),2,cumprod)
      dx.mat.k <- -apply(lx.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
      
      ## get nn deaths
      if(nn.exists){
        wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
        nqx.nn.mat.k <- wgt.nn.mat.k*(nmr.mat.k/1000)
        npx.nn.mat.k <- 1-nqx.nn.mat.k
        lx.nn.mat.k <- apply(rbind(bwc.vec.k, npx.nn.mat.k),2,cumprod)
        dx.nn.mat.k <- -apply(lx.nn.mat.k,2,diff) ## ages (half year) x weeks (52*length(years.k))
      }
      
      years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), nrow=10, ncol=length(years.k)*52)
      years.k.mat <- years.k.mat[,order(years.k.mat[1,])]
      
      for(yrk in min(years.k):max(years.k)){
        deathu5.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T) 
        death0.temp.rt[k,match(yrk,years)] <- sum(dx.mat.k[1:2,][floor(years.k.mat[1:2,])==yrk], na.rm=T)
        if(nn.exists) deathnn.temp.rt[k,match(yrk,years)] <- sum(dx.nn.mat.k[floor(years.k.mat[1:2,])==yrk], na.rm=T)
      } # for loop for summing deaths
      # edit DJS 2018-07-27 round deaths at country level before summing to region and world
      deathu5.temp.rt <- roundoff(deathu5.temp.rt, digits = 0)
      death0.temp.rt <- roundoff(death0.temp.rt, digits = 0)
      if(nn.exists) deathnn.temp.rt <- roundoff(deathnn.temp.rt, digits = 0)
      # edit DJS 2018-07-27
    } # k loop for countries in the region
    
    
    death0.all.rt[r,] <- apply(death0.temp.rt, 2, sum, na.rm=F)
    deathu5.all.rt[r,] <- apply(deathu5.temp.rt, 2, sum, na.rm=F)
    if(nn.exists) deathnn.all.rt[r,] <- apply(deathnn.temp.rt, 2, sum, na.rm=F)
    
    } else {# if replace.rates.reg
      death0.all.rt <- death0.rt
      deathu5.all.rt <- deathu5.rt
      if(nn.exists) deathnn.all.rt <- deathnn.rt
    } # else 
    
    if(!dir.exists(output.dir.samples.region)) dir.create(output.dir.samples.region, recursive = TRUE)
    # message("output.dir.samples.region is: ", output.dir.samples.region)
    save(q0.rt, file = file.path(output.dir.samples.region, paste0("q0.rt_", j, ".rda")))
    save(q1to4.rt, file = file.path(output.dir.samples.region, paste0("q1to4.rt_", j, ".rda")))
    save(q5.rt, file = file.path(output.dir.samples.region, paste0("q5.rt_", j, ".rda")))
    save(death0.all.rt, file = file.path(output.dir.samples.region, paste0("death0.all.rt_", j, ".rda")))
    save(death1to4.all.rt, file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    save(deathu5.all.rt, file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    if(nn.exists){
      save(qnn.rt, file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
      save(deathnn.all.rt, file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
    } # if
  } # r loop regions
}
#----------------------------------------------------------------------
CombineAndOutputRegionalResultsBWC <- function(
  output.dir,
  output.dir.samples,
  output.dir.samplescombined,
  regiontypes,
  filename,
  percentiles,
  ndigits,
  replace.rates.reg,
  round.output# DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
) {
  # load one file first to get dimensions
  if(is.null(replace.rates.reg)){ # DJS 2018-07-26 edit to give option to use deaths calculated with missing rates replaced with regional aggregate
    load(file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
  } else {
    load(file = file.path(output.dir.samplescombined, paste0("deathu5.ctj.", replace.rates.reg, "-replace.rda")))
  }
  
  nsim <- dim(deathu5.ctj)[3]
  nregs <- length(regiontypes)
  load(file.path(output.dir.samplescombined, "info.rda"))
  list2env(info, envir = environment())
  
  # load world results
  world.results.exist <- file.exists(file.path(output.dir.samplescombined, "res.world.rda"))
  if(world.results.exist){
  load(file = file.path(output.dir.samplescombined, "res.world.rda"))
  # load(file = file.path(output.dir.samplescombined, "global.RoDs.ui.rda"))
  }
  
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
    # load(file = file.path(output.dir.samples.region, paste0("death1to4.all.rt_", j, ".rda")))
    load(file = file.path(output.dir.samples.region, paste0("deathu5.all.rt_", j, ".rda")))
    
    nn.exists <- file.exists(file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    if(nn.exists){
      load(file = file.path(output.dir.samples.region, paste0("qnn.rt_", j, ".rda")))
    }
    if(nn.exists){
      load(file = file.path(output.dir.samples.region, paste0("deathnn.all.rt_", j, ".rda")))
    }
    
    q0.rtj[ , , j] <- q0.rt
    q1to4.rtj[ , , j] <- q1to4.rt
    q5.rtj[ , , j] <- q5.rt
    if(nn.exists) qnn.rtj[ , , j] <- qnn.rt
    death0.all.rtj[ , , j] <- death0.all.rt
    # death1to4.all.rtj[ , , j] <- death1to4.all.rt
    deathu5.all.rtj[ , , j] <- deathu5.all.rt
    if(nn.exists) deathnn.all.rtj[ , , j] <- deathnn.all.rt
  }
  u5mr.rtj <- q5.rtj*1000
  imr.rtj <- q0.rtj*1000
  if(nn.exists) nmr.rtj <- qnn.rtj*1000

  
  # save the samples
  dimnames(u5mr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(u5mr.rtj)[2]-1)))
  save(u5mr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_u5mr.rtj.rda")))
  dimnames(imr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(imr.rtj)[2]-1)))
  save(imr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_imr.rtj.rda")))
  dimnames(deathu5.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(deathu5.all.rtj)[2]-1)))
  save(deathu5.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathu5.all.rtj.rda")))
  dimnames(death0.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(death0.all.rtj)[2]-1)))
  save(death0.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_death0.all.rtj.rda")))
  if(nn.exists){
    dimnames(nmr.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(nmr.rtj)[2]-1)))
    save(nmr.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_nmr.rtj.rda")))
    dimnames(deathnn.all.rtj) <- list(c(regiontypes), c(1950.5:(1950.5+dim(deathnn.all.rtj)[2]-1)))
    save(deathnn.all.rtj, file = file.path(output.dir.samplescombined, paste0(filename, "_deathnn.all.rtj.rda")))
  }
  
  # delete samples
  # unlink(file.path(output.dir.samples.region, paste0("q0.rt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples.region, paste0("q1to4.rt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples.region, paste0("q5.rt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples.region, paste0("death0.all.rt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples.region, paste0("death1to4.all.rt_", 1:nsim, ".rda")))
  # unlink(file.path(output.dir.samples.region, paste0("deathu5.all.rt_", 1:nsim, ".rda")))
  # if(nn.exists){
  #   unlink(file.path(output.dir.samples.region, paste0("qnn.rt_", 1:nsim, ".rda")))
  #   unlink(file.path(output.dir.samples.region, paste0("deathnn.all.rt_", 1:nsim, ".rda")))
  # }
  
  # load population and coverage info
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_popu5.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_pop0.orig.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverageu5.rt.rda")))
  load(file = file.path(output.dir.samplescombined, paste0(filename, "_coverage0.rt.rda")))
  
  # regional summaries
  u5mr.qrt <- apply(u5mr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  imr.qrt <- apply(imr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  deathu5.all.qrt <- apply(deathu5.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  death0.all.qrt <- apply(death0.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  if(nn.exists){
    nmr.qrt <- apply(nmr.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
    deathnn.all.qrt <- apply(deathnn.all.rtj, c(1, 2), quantile, probs = percentiles, na.rm = T)
  }
  
  # NA if coverage < 0.5
  for (q in 1:length(percentiles)) {
    u5mr.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    imr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    if(nn.exists) nmr.qrt[q, , ][coverage0.rt < 0.5] <- NA
    # deathu5.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1):(max(which(coverageu5.rt < 0.5))+5))] <- NA
    # death0.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    # if(nn.exists) deathnn.all.qrt[q, , ][c(which(coverageu5.rt < 0.5), (max(which(coverageu5.rt < 0.5))+1))] <- NA
    deathu5.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    death0.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    if(nn.exists) deathnn.all.qrt[q, , ][coverageu5.rt < 0.5] <- NA
    for(regy in 1:dim(u5mr.qrt)[2]){
      # use presence of rate to put NA for deaths
      ifelse(length(which(is.na(u5mr.qrt[q,regy,])))>0,
             deathu5.all.qrt[q,regy,(max(which(is.na(u5mr.qrt[q,regy,])))):(max(which(is.na(u5mr.qrt[q,regy,])))+5)] <- NA,
             deathu5.all.qrt[q,regy,1:5] <- NA)
      ifelse(length(which(is.na(imr.qrt[q,regy,])))>0,
             death0.all.qrt[q,regy,(max(which(is.na(imr.qrt[q,regy,])))):(max(which(is.na(imr.qrt[q,regy,])))+1)] <- NA,
             death0.all.qrt[q,regy,1] <- NA)
      if(nn.exists){
      ifelse(length(which(is.na(nmr.qrt[q,regy,])))>0,
             deathnn.all.qrt[q,regy,(max(which(is.na(nmr.qrt[q,regy,])))):(max(which(is.na(nmr.qrt[q,regy,])))+1)] <- NA,
             deathnn.all.qrt[q,regy,1] <- NA)
      }

       # use coverage to put NAs for death 
      # if(length(which(coverageu5.rt[regy,]<0.5))>0){
      # deathu5.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1):(max(which(coverageu5.rt[regy,] < 0.5))+5)] <- NA
      # death0.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1)] <- NA
      # if(nn.exists) deathnn.all.qrt[q,regy,(max(which(coverageu5.rt[regy,] < 0.5))+1)] <- NA
      # } else {
      # deathu5.all.qrt[q,regy,1:5] <- NA
      # death0.all.qrt[q,regy,1] <- NA
      # if(nn.exists) deathnn.all.qrt[q,regy,1] <- NA
      # }
    } # regy loop
  }
  
  # regional summary
  res.year <- NULL
  for (i in 1:nyears) {
    if(nn.exists){
      ifelse(round.output, t.u5mr.qrt <- roundoff(t(u5mr.qrt[,,i]), digits = ndigits), t.u5mr.qrt <- t(u5mr.qrt[,,i]))
      ifelse(round.output, t.imr.qrt <- roundoff(t(imr.qrt[,,i]), digits = ndigits), t.imr.qrt <- t(imr.qrt[,,i]))
      ifelse(round.output, t.nmr.qrt <- roundoff(t(nmr.qrt[,,i]), digits = ndigits), t.nmr.qrt <- t(nmr.qrt[,,i]))
      
      if(world.results.exist){
      res.year <- rbind(res.year,
                        rbind(
                          cbind(est.years.floor[i],
                                roundoff(popu5.orig.rt[,i], digits = 0),
                                roundoff(pop0.orig.rt[,i], digits = 0),
                                roundoff(coverageu5.rt[,i]*100, digits = 2),
                                roundoff(coverage0.rt[,i]*100, digits = 2),
                                t.u5mr.qrt,
                                t.imr.qrt,
                                t.nmr.qrt,
                                roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                roundoff(t(death0.all.qrt[,,i]), digits = 0),
                                roundoff(t(deathnn.all.qrt[,,i]), digits = 0)),
                          res.world[res.world[, 1] == est.years.floor[i], ]
                        )) 
      } else {
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  t.nmr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0),
                                  roundoff(t(deathnn.all.qrt[,,i]), digits = 0))
                          ))         
      } # if(world.results.exist)
    } else {
      ifelse(round.output, t.u5mr.qrt <- roundoff(t(u5mr.qrt[,,i]), digits = ndigits), t.u5mr.qrt <- t(u5mr.qrt[,,i]))
      ifelse(round.output, t.imr.qrt <- roundoff(t(imr.qrt[,,i]), digits = ndigits), t.imr.qrt <- t(imr.qrt[,,i]))
      
      if(world.results.exist){
      res.year <- rbind(res.year,
                        rbind(
                          cbind(est.years.floor[i],
                                roundoff(popu5.orig.rt[,i], digits = 0),
                                roundoff(pop0.orig.rt[,i], digits = 0),
                                roundoff(coverageu5.rt[,i]*100, digits = 2),
                                roundoff(coverage0.rt[,i]*100, digits = 2),
                                t.u5mr.qrt,
                                t.imr.qrt,
                                roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                roundoff(t(death0.all.qrt[,,i]), digits = 0)),
                          res.world[res.world[, 1] == est.years.floor[i], ]
                        ))
      } else {
        res.year <- rbind(res.year,
                          rbind(
                            cbind(est.years.floor[i],
                                  roundoff(popu5.orig.rt[,i], digits = 0),
                                  roundoff(pop0.orig.rt[,i], digits = 0),
                                  roundoff(coverageu5.rt[,i]*100, digits = 2),
                                  roundoff(coverage0.rt[,i]*100, digits = 2),
                                  t.u5mr.qrt,
                                  t.imr.qrt,
                                  roundoff(t(deathu5.all.qrt[,,i]), digits = 0),
                                  roundoff(t(death0.all.qrt[,,i]), digits = 0))
                          ))        
      } # if(world.results.exist)
    } # if/else
  }
  ifelse(world.results.exist, 
         res.region <- cbind(rep(c(regiontypes, "World"), nyears), res.year), 
         res.region <- cbind(rep(c(regiontypes), nyears), res.year)
         )
  
  # output to .csv
  ui.colnames <- c(" lower bound", " median", " upper bound")
  if(nn.exists){
    colnames(res.region) <- c("Region", "Year",
                              "Under-five population", "Infant population",
                              "Population coverage (under 5)", "Population coverage (age 0)",
                              paste0("U5MR", ui.colnames),
                              paste0("IMR", ui.colnames),
                              paste0("NMR", ui.colnames),
                              paste0("Under-five deaths", ui.colnames),
                              paste0("Infant deaths", ui.colnames),
                              paste0("Neonatal deaths", ui.colnames))
  } else {
    colnames(res.region) <- c("Region", "Year",
                              "Under-five population", "Infant population",
                              "Population coverage (under 5)", "Population coverage (age 0)",
                              paste0("U5MR", ui.colnames),
                              paste0("IMR", ui.colnames),
                              paste0("Under-five deaths", ui.colnames),
                              paste0("Infant deaths", ui.colnames))
  }
  
  if (nsim == 1) res.region <- res.region[, !grepl("bound", colnames(res.region))]
  write.csv(res.region, file = file.path(output.dir, paste0("Rates & Deaths_", filename, ".csv")),
            row.names = F, na = "")
  
  # round off to 1 d.p. before calculation (for median only)
  if (dim(u5mr.rtj)[3] == 1) { # change JR, 26 Aug 2013
    u5mr.rtj <- roundoff(u5mr.rtj, digits = ndigits)
    imr.rtj <- roundoff(imr.rtj, digits = ndigits)
  }
  
  # regional summary - rates of decline
  # region.RoDs.ui <- NULL
  # for (r in 1:nregs) {
  #   ARR.year1.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year4)
  #   ARR.year1.year2.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year1, year.end = year2)
  #   ARR.year2.year4.j <- CalculateARR(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                     year.start = year2, year.end = year4)
  #   required.ARR.j <- ifelse(year4 < year.target,
  #                            1/(year.target-year4)*
  #                              log(roundoff(u5mr.rtj[r, est.years == year1, ]*factor.target, digits = ndigits)/
  #                                    u5mr.rtj[1, est.years == year4, ])*-100, NA)
  #   changeinARR.j <- ARR.year2.year4.j - ARR.year1.year2.j
  #   decline.year1.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year1, year.end = year4)
  #   decline.year1.year2.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year1, year.end = year2)
  #   decline.year2.year4.j <- CalculateDecline(u5mr = u5mr.rtj[r, , ], years = est.years,
  #                                             year.start = year2, year.end = year4)
  #   ARR.year1.year4.ui <- quantile(ARR.year1.year4.j, probs = percentiles)
  #   ARR.year1.year2.ui <- quantile(ARR.year1.year2.j, probs = percentiles)
  #   ARR.year2.year4.ui <- quantile(ARR.year2.year4.j, probs = percentiles)
  #   # change JR, 20150605: set na.rm = TRUE if all required.ARR are NAs,
  #   # indicating that year4 = year.target
  #   required.ARR.ui <- quantile(required.ARR.j, probs = percentiles,
  #                               na.rm = all(is.na(required.ARR.j)))
  #   changeinARR.ui <- quantile(changeinARR.j, probs = percentiles)
  #   decline.year1.year4.ui <- quantile(decline.year1.year4.j, probs = percentiles)
  #   decline.year1.year2.ui <- quantile(decline.year1.year2.j, probs = percentiles)
  #   decline.year2.year4.ui <- quantile(decline.year2.year4.j, probs = percentiles)
  #   region.RoDs.ui <- rbind(region.RoDs.ui,
  #                           c(ARR.year1.year4.ui, ARR.year1.year2.ui, ARR.year2.year4.ui,
  #                             required.ARR.ui, changeinARR.ui, decline.year1.year4.ui,
  #                             decline.year1.year2.ui, decline.year2.year4.ui))
  #   
  # }
  # colnames(region.RoDs.ui) <- c(paste0("ARR ", year1-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("ARR ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("ARR ", year2-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("Required ARR", ui.colnames),
  #                               paste0("Change in ARR", ui.colnames),
  #                               paste0("Percentage decline ", year1-0.5, "-", year4-0.5, ui.colnames),
  #                               paste0("Percentage decline ", year1-0.5, "-", year2-0.5, ui.colnames),
  #                               paste0("Percentage decline ", year2-0.5, "-", year4-0.5, ui.colnames))
  # ifelse(world.results.exist,
  # region.RoDs <- data.frame(Region = c(regiontypes, "World"), rbind(region.RoDs.ui, global.RoDs.ui)),
  # region.RoDs <- data.frame(Region = c(regiontypes), region.RoDs.ui)
  # )
  # if (nsim == 1)
  #   region.RoDs <- region.RoDs[, !grepl("bound", colnames(region.RoDs))]
  # write.csv(region.RoDs, file = file.path(output.dir, paste0("Rates of Decline_", filename, ".csv")),
  #           row.names = F, na = "")
  cat(paste0("Output generated for ", filename, ".\n"))
}
