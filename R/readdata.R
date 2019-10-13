#----------------------------------------------------------------------
# readdata.R
# Jin Rou New & Leontine Alkema, 2012-2013
#----------------------------------------------------------------------
ReadData <- function( # Read in country information and child mortality data and create a list with the data
  country.codes = NULL, ##<< Vector of ISO codes of countries to read in data for 
  get.HIV.removed.data = FALSE, ##<< Logical value indicating if HIV-removed data are desired
  get.log.scale.data = FALSE, ##<< Logical value indicating if data should be on log scale
  country.B3info.file, ##<< File path to a .csv with country B3 information
  data.file, ##<< File path to a .csv with database
  hiv.file, ##<< File path to a .csv with UNAIDS HIV estimates
  adj.file, ##<< File path to a .csv with WHO crisis mortality adjustment data
  isos.to.exclude.for.global.run = c("LIE", "PRK", "SPS", "MCO"), ##<< Vector of 3-character ISO country codes
  ## of country data to exclude for global run. Default: Liechtenstein, Korea DPR & Sudan pre-secession (as estimates 
  ## are no longer produced with B3 for these countries); Andorra & Monaco (as neighbouring regions' data are used) # change JR, 20140501
  settings, ##<< List of settings
  print.messages = TRUE
) {
  info <- read.csv(file = country.B3info.file, header = T, as.is = T, stringsAsFactors = F, strip.white = T)
  data.readin <- read.csv(file = data.file, header = T, as.is = T, stringsAsFactors = F, strip.white = T, encoding = "latin1")
  data.hiv <- read.csv(file = hiv.file, header = T, stringsAsFactors = F)
  data.adj <- read.csv(file = adj.file, header = T, stringsAsFactors = F)
  list2env(settings, envir = environment())
  
  ##details<< Country information for all countries in both \code{country.B3info.file} and
  ## \code{data.file} is read in if \code{country.codes} is \code{NULL}.
  if (is.null(country.codes)) { # change JR, 20140502
    select <- info$iso.c %in% data.readin$countrycode.i & 
      !(info$iso.c %in% isos.to.exclude.for.global.run)
    if (!include.HIV.countries) {
      select <- select & !(info$iso.c %in% data.hiv$countrycode.hiv[data.hiv$unaids.hiv != 0])
    }
  } else {
    if (any(!(country.codes %in% data.readin$countrycode.i)))
      stop(paste0(paste(country.codes[!(country.codes %in% data.readin$countrycode.i)], collapse = ", "),
                  " cannot be found in ", data.file))
    select <- info$iso.c %in% country.codes
  }
  iso.c <- info$iso.c[select]
  uncode.c <- info$uncode.c[select]
  N <- length(data.readin[, 1]) # 8600
  C <- length(iso.c)
  #----------------------------------------------------------------------
  hiv.c <- iso.c %in% data.hiv$countrycode.hiv[data.hiv$unaids.hiv != 0]
  if (print.messages & any(hiv.c))
    cat(paste0(sum(hiv.c), " out of ", length(hiv.c), " countries will have HIV adjustments applied: ",
               paste(iso.c[hiv.c], collapse = ", "), "\n"))
  crisisadj.c <- iso.c %in% data.adj$countrycode.adj[data.adj$add.adj != 0]
  if (print.messages & any(crisisadj.c))
    cat(paste0(sum(crisisadj.c), " out of ", length(crisisadj.c), 
               " countries will have crisis adjustments applied to the estimates after model fitting: ",
               paste(iso.c[crisisadj.c], collapse = ", "), "\n"))
  # change JR, 20150515: as of IGME 2015, data in crisis-years will be excluded for model fitting
  # instead of being subtracted for crisis mortality before model fitting
  crisisadjfordata.c <- rep(FALSE, C)
  if (print.messages & any(crisisadjfordata.c))
    cat(paste0(sum(crisisadjfordata.c), " out of ", length(crisisadjfordata.c), 
               " countries will have crisis adjustments applied to the data before model fitting: ",
               paste(iso.c[crisisadjfordata.c], collapse = ", "), "\n"))
  useglobalsmoothing.c <- iso.c %in% isos.for.global.smoothing
  if (print.messages & any(useglobalsmoothing.c))
    cat(paste0(sum(useglobalsmoothing.c), " out of ", length(useglobalsmoothing.c), 
               " countries have global smoothing applied: ",
               paste(iso.c[useglobalsmoothing.c], collapse = ", "), "\n"))
  #----------------------------------------------------------------------
  select.fixserieslevelbias <- data.readin$fixserieslevelbias.i == 1
  if (print.messages & any(select.fixserieslevelbias))
    cat(paste0(length(iso.c[iso.c %in% data.readin$countrycode.i[select.fixserieslevelbias]]), 
               " out of ", C, " countries have series with fixed series level bias (for ", 
               sum(select.fixserieslevelbias), " observations in total): ",
               paste(iso.c[iso.c %in% data.readin$countrycode.i[select.fixserieslevelbias]], collapse = ", "), "\n"))
  select.hasbiasnonvr <- data.readin$hasbias.i == 1 & data.readin$sourcetype.i != "VR"
  if (print.messages & any(select.hasbiasnonvr))
    cat(paste0(length(iso.c[iso.c %in% data.readin$countrycode.i[select.hasbiasnonvr]]), 
               " out of ", C, " countries have non-VR relative bias (for ", 
               sum(select.hasbiasnonvr), " observations in total): ",
               paste(iso.c[iso.c %in% data.readin$countrycode.i[select.hasbiasnonvr]], collapse = ", "), "\n"))
  select.hasbiasvr <- data.readin$hasbias.i == 1 & data.readin$sourcetype.i == "VR"
  if (print.messages & any(select.hasbiasvr))
    cat(paste0(length(iso.c[iso.c %in% data.readin$countrycode.i[select.hasbiasvr]]), 
               " out of ", C, " countries have VR relative bias (for ", 
               sum(select.hasbiasvr), " observations in total): ",
               paste(iso.c[iso.c %in% data.readin$countrycode.i[select.hasbiasvr]], collapse = ", "), "\n"))
  select.vrincomplete <- data.readin$setasminimum.i == 1 & is.na(data.readin$minimumcompleteness.i)
  if (print.messages & any(select.vrincomplete))
    cat(paste0(length(iso.c[iso.c %in% data.readin$countrycode.i[select.vrincomplete]]), 
               " out of ", C, " countries have observations set as minimum (for ", 
               sum(select.vrincomplete), " observations in total): ",
               paste(iso.c[iso.c %in% data.readin$countrycode.i[select.vrincomplete]], collapse = ", "), "\n"))
  select.vrincompminmax <- data.readin$setasminimum.i == 1 & !is.na(data.readin$minimumcompleteness.i)
  if (print.messages & any(select.vrincompminmax))
    cat(paste0(length(iso.c[iso.c %in% data.readin$countrycode.i[select.vrincompminmax]]), 
               " out of ", C, " countries have incomplete VR with min/max completeness specified (for ", 
               sum(select.vrincompminmax), " observations in total): ",
               paste(iso.c[iso.c %in% data.readin$countrycode.i[select.vrincompminmax]], collapse = ", "), "\n"))  
  #----------------------------------------------------------------------
  uvr.Lc.j <- yearvr.Lc.j <- sevr.Lc.j <- senonNAvr.Lc.j <- hasbiasvr.Lc.j <-
    isincompletevr.Lc.j <- mincompincompletevr.Lc.j <- maxcompincompletevr.Lc.j <- list()
  uvr.Lcs.j <- yearvr.Lcs.j <- sevr.Lcs.j <- senonNAvr.Lcs.j <- includedvr.Lcs.j <- intervalvr.Lcs.j <- 
    hasbiasvr.Lcs.j <- isincompletevr.Lcs.j <- mincompincompletevr.Lcs.j <- maxcompincompletevr.Lcs.j <- list()
  sourcevr.Lc.s <- sourceidvr.Lc.s <- list()
  u.Lcs.j <- year.Lcs.j <- se.Lcs.j <- senonNA.Lcs.j <- included.Lcs.j <-
    sourcetype.Lcs.j <- method.Lcs.j <- sourceid.Lcs.j <- interval.Lcs.j <- list() 
  source.Lc.s <- sourceid.Lc.s <- isDHSdirectany.Lc.s <- 
    sourcetype.Lc.s <- method.Lc.s <- surveyyear.Lc.s <- seriesyear.Lc.s <- 
    isserieslevelbiasatprior.Lc.s <-
    hasbias.Lc.s <- list()
  # make dimensions equal to C+1 to avoid problems when running it for one country
  nseriesvr.c <- nseriesnonvr.c <- nuniqueseriesnonvr.c <- n.c <- nvr.c <- nnonvr.c <- c(rep(0, C), NA)
  minyear.c <- maxyear.c <- rep(NA, C+1)
  name.c <- rep("", C)
  for (c in 1:C) {
    name.c[c] <- data.readin$country.i[data.readin$countrycode.i==iso.c[c]][1]
    uvr.Lcs.j[[c]] <- yearvr.Lcs.j[[c]] <- sevr.Lcs.j[[c]] <- senonNAvr.Lcs.j[[c]] <- 
      includedvr.Lcs.j[[c]] <- intervalvr.Lcs.j[[c]] <- hasbiasvr.Lcs.j[[c]] <- # change JR, 20140429 
      isincompletevr.Lcs.j[[c]] <- mincompincompletevr.Lcs.j[[c]] <- maxcompincompletevr.Lcs.j[[c]] <- 
      list()
    u.Lcs.j[[c]] <- year.Lcs.j[[c]] <- senonNA.Lcs.j[[c]] <- se.Lcs.j[[c]] <- included.Lcs.j[[c]] <- # change JR, 1 Jun
      sourcetype.Lcs.j[[c]] <- method.Lcs.j[[c]] <- sourceid.Lcs.j[[c]] <- interval.Lcs.j[[c]] <- list()
    # VR obs
    if (sum(data.readin$sourcetype.i[data.readin$countrycode.i == iso.c[c]] == "VR") > 0) {
      select.obs <- seq(1,N)[data.readin$countrycode.i == iso.c[c] & data.readin$sourcetype.i == "VR"]
      nvr.c[c] <- length(select.obs)
      # order years (easier for plotting)
      year.unsorted <- data.readin$year.i[select.obs]
      order <- order(year.unsorted)
      yearvr.Lc.j[[c]] <- year.unsorted[order]
      uvr.Lc.j[[c]] <- data.readin$u.i[select.obs][order]
      sevr.Lc.j[[c]] <- data.readin$se.i[select.obs][order]
      senonNAvr.Lc.j[[c]] <- ifelse(is.na(sevr.Lc.j[[c]]), 0, sevr.Lc.j[[c]])
      hasbiasvr.Lc.j[[c]] <- data.readin$hasbias.i[select.obs][order]
      isincompletevr.Lc.j[[c]] <- data.readin$setasminimum.i[select.obs][order]
      mincompincompletevr.Lc.j[[c]] <- data.readin$minimumcompleteness.i[select.obs][order]
      maxcompincompletevr.Lc.j[[c]] <- data.readin$maximumcompleteness.i[select.obs][order]
      if (get.HIV.removed.data) {
        if (hiv.c[c]) {
          uvr.Lc.j[[c]] <- GetHIVSubtractedSeries(u.i = uvr.Lc.j[[c]], year.i = yearvr.Lc.j[[c]], 
                                                  iso = iso.c[c], hiv.file = hiv.file)
        }
      }
      sourceIDsvr <- unique(data.readin$sourceID.i[select.obs])
      nseriesvr <- length(sourceIDsvr)
      if (nseriesvr > 0) {
        nseriesvr.c[c] <- nseriesvr
        sourcevr.Lc.s[[c]] <- sourceidvr.Lc.s[[c]] <- rep(NA, nseriesvr)
        for (series in 1:nseriesvr) {
          sourceIDvr <- sourceIDsvr[series]
          select.obs <- seq(1,N)[data.readin$countrycode.i == iso.c[c] & data.readin$sourcetype.i == "VR" &
                                   data.readin$sourceID.i == sourceIDvr]
          # order years (easier for plotting)
          year.unsorted <- data.readin$year.i[select.obs]
          order <- order(year.unsorted)
          yearvr.Lcs.j[[c]][[series]] <- year.unsorted[order]
          uvr.Lcs.j[[c]][[series]] <- data.readin$u.i[select.obs][order]
          if (get.HIV.removed.data) {
            if (hiv.c[c]) {
              uvr.Lcs.j[[c]][[series]] <- GetHIVSubtractedSeries(u.i = uvr.Lcs.j[[c]][[series]], 
                                                                 year.i = yearvr.Lcs.j[[c]][[series]], 
                                                                 iso = iso.c[c], 
                                                                 hiv.file = hiv.file)
            }
          }
          sevr.Lcs.j[[c]][[series]] <- data.readin$se.i[select.obs][order]
          senonNAvr.Lcs.j[[c]][[series]] <- ifelse(is.na(sevr.Lcs.j[[c]][[series]]), 0, sevr.Lcs.j[[c]][[series]])
          includedvr.Lcs.j[[c]][[series]] <- data.readin$included.i[select.obs][order]
          intervalvr.Lcs.j[[c]][[series]] <- data.readin$interval.i[select.obs][order]
          hasbiasvr.Lcs.j[[c]][[series]] <- data.readin$hasbias.i[select.obs][order] # change JR, 20140429
          isincompletevr.Lcs.j[[c]][[series]] <- data.readin$setasminimum.i[select.obs][order]
          mincompincompletevr.Lcs.j[[c]][[series]] <- data.readin$minimumcompleteness.i[select.obs][order]
          maxcompincompletevr.Lcs.j[[c]][[series]] <- data.readin$maximumcompleteness.i[select.obs][order]
          sourceidvr.Lc.s[[c]][series] <- sourceIDvr
          sourcevr.Lc.s[[c]][series] <- paste(unique(data.readin$source.i[select.obs]))
          if (is.element("SVR", data.readin$seriescategory.i[select.obs]))
            sourcevr.Lc.s[[c]][series] <- paste("SVR", sourcevr.Lc.s[[c]][series])
        }
      }
    } # end VR obs
    
    # non-VR obs
    sourceIDs <- unique(data.readin$sourceID.i[
      data.readin$countrycode.i==iso.c[c] & data.readin$sourcetype.i != "VR"])
    nnonvr.c[c] <- sum(data.readin$countrycode.i == iso.c[c] & data.readin$sourcetype.i != "VR")
    nseriesnonvr <- length(sourceIDs)
    if (nseriesnonvr > 0) {
      nseriesnonvr.c[c] <- nseriesnonvr
      sourceid.Lc.s[[c]] <- source.Lc.s[[c]] <- isDHSdirectany.Lc.s[[c]] <-
        sourcetype.Lc.s[[c]] <- method.Lc.s[[c]] <- surveyyear.Lc.s[[c]] <- 
        seriesyear.Lc.s[[c]] <- isDHSdirectany.Lc.s[[c]] <- isserieslevelbiasatprior.Lc.s[[c]] <-
        hasbias.Lc.s[[c]] <- rep(NA, nseriesnonvr) # change JR, 20140407
      for (series in 1:nseriesnonvr) {
        sourceID <- sourceIDs[series]
        select.obs <- seq(1,N)[data.readin$countrycode.i==iso.c[c] & data.readin$sourcetype.i != "VR" &
                                 data.readin$sourceID.i==sourceID]
        # order years (easier for plotting)
        year.unsorted <- data.readin$year.i[select.obs]
        order <- order(year.unsorted)
        year.Lcs.j[[c]][[series]] <- year.unsorted[order]                                           
        u.Lcs.j[[c]][[series]] <- data.readin$u.i[select.obs][order]
        if (get.HIV.removed.data) {
          if (hiv.c[c]) {
            u.Lcs.j[[c]][[series]] <- GetHIVSubtractedSeries(u.i = u.Lcs.j[[c]][[series]], 
                                                             year.i = year.Lcs.j[[c]][[series]], 
                                                             iso = iso.c[c], 
                                                             hiv.file = hiv.file)
          }
        }
        se.Lcs.j[[c]][[series]] <- data.readin$se.i[select.obs][order]
        senonNA.Lcs.j[[c]][[series]] <- ifelse(is.na(se.Lcs.j[[c]][[series]]), 0, se.Lcs.j[[c]][[series]])
        included.Lcs.j[[c]][[series]] <- data.readin$included.i[select.obs][order]
        sourcetype.Lcs.j[[c]][[series]] <- data.readin$sourcetype.i[select.obs]
        method.Lcs.j[[c]][[series]] <- data.readin$method.i[select.obs]
        sourceid.Lcs.j[[c]][[series]] <- data.readin$sourceID.i[select.obs]
        interval.Lcs.j[[c]][[series]] <- data.readin$interval.i[select.obs]
        sourceid.Lc.s[[c]][series] <- sourceID
        source.Lc.s[[c]][series] <- paste(unique(data.readin$source.i[select.obs]), # should be same for all obs in series
                                          ifelse(sum(data.readin$tsfb.i[select.obs]) > 0, "TSFB", "")) # change JR, 20140530
        isDHSdirectany.Lc.s[[c]][series] <- ifelse(is.element(unique(paste(data.readin$sourcetype.i[select.obs], 
                                                                           data.readin$method.i[select.obs])), 
                                                              c("DHS Direct", "Other DHS Direct")), 1, 0)
        surveyyear.Lc.s[[c]][series] <- max(data.readin$surveyyear.i[select.obs]) # obs year plugged in for some, so take max
        seriesyear.Lc.s[[c]][series] <- max(data.readin$seriesyear.i[select.obs])
        if (seriesyear.Lc.s[[c]][series] == 0) seriesyear.Lc.s[[c]][series] <- round(surveyyear.Lc.s[[c]][series])       
        method.Lc.s[[c]][series] <- unique(data.readin$method.i[select.obs])
        sourcetype.Lc.s[[c]][series] <- unique(data.readin$sourcetype.i[select.obs])
        isserieslevelbiasatprior.Lc.s[[c]][series] <- ifelse(is.element(1, data.readin$fixserieslevelbias.i[select.obs]), 
                                                             1, 0)
        hasbias.Lc.s[[c]][series] <- ifelse(is.element(1, data.readin$hasbias.i[select.obs]), 1, 0) # change JR, 20140407
      }
      nuniqueseriesnonvr.c[c] <- length(unique(paste(source.Lc.s[[c]], seriesyear.Lc.s[[c]])))
    } # end non-VR obs
  } # end country loop
  # need to make sure dim of lists are C+1
  uvr.Lc.j[[C+1]] <- yearvr.Lc.j[[C+1]] <- sevr.Lc.j[[C+1]] <- senonNAvr.Lc.j[[C+1]] <- hasbiasvr.Lc.j[[C+1]] <- # change JR, 20140429
    isincompletevr.Lc.j[[C+1]] <- mincompincompletevr.Lc.j[[C+1]] <- maxcompincompletevr.Lc.j[[C+1]] <- NA
  uvr.Lcs.j[[C+1]] <- yearvr.Lcs.j[[C+1]] <- sevr.Lcs.j[[C+1]] <- senonNAvr.Lcs.j[[C+1]] <- 
    includedvr.Lcs.j[[C+1]] <- intervalvr.Lcs.j[[C+1]] <- hasbiasvr.Lcs.j[[C+1]] <- # change JR, 20140429
    isincompletevr.Lcs.j[[C+1]] <- mincompincompletevr.Lcs.j[[C+1]] <- maxcompincompletevr.Lcs.j[[C+1]] <- NA
  sourcevr.Lc.s[[C+1]] <- sourceidvr.Lc.s[[C+1]] <- ""
  u.Lcs.j[[C+1]] <- year.Lcs.j[[C+1]] <- se.Lcs.j[[C+1]] <- included.Lcs.j[[C+1]] <-
    sourcetype.Lcs.j[[C+1]] <- method.Lcs.j[[C+1]] <- sourceid.Lcs.j[[C+1]] <- interval.Lcs.j[[C+1]] <- NA
  surveyyear.Lc.s[[C+1]] <- seriesyear.Lc.s[[C+1]] <- NA
  source.Lc.s[[C+1]] <- sourceid.Lc.s[[C+1]] <- sourcetype.Lc.s[[C+1]] <- method.Lc.s[[C+1]] <- ""
  isDHSdirectany.Lc.s[[C+1]] <- isserieslevelbiasatprior.Lc.s[[C+1]] <-
    hasbias.Lc.s[[C+1]] <- NA # change JR, 20140407
  for (c in 1:C) {
    year.i <- c(unlist(year.Lcs.j[[c]]), unlist(yearvr.Lc.j[[c]]))
    minyear.c[c] <- min(year.i)
    maxyear.c[c] <- max(year.i)
  }
  if (get.log.scale.data) {
    uvr.Lc.j.temp <- uvr.Lc.j
    u.Lcs.j.temp <- u.Lcs.j
    u.Lcs.j.temp <- u.Lcs.j
    uvr.Lcs.j.temp <- uvr.Lcs.j
    se.Lcs.j.temp <- se.Lcs.j
    sevr.Lcs.j.temp <- sevr.Lcs.j
    # transform to log scale
    uvr.Lc.j <- lapply(uvr.Lc.j.temp, function(x) { if (!is.null(x)) log(x) })
    sevr.Lc.j <- mapply(function(x, y) { if (!is.null(x)) x/y }, sevr.Lc.j, uvr.Lc.j.temp)
    senonNAvr.Lc.j <- mapply(function(x, y) { if (!is.null(x)) x/y }, senonNAvr.Lc.j, uvr.Lc.j.temp)
    u.Lcs.j <- lapply(u.Lcs.j.temp, function(x) { lapply(x, function(x) { if (!is.null(x)) log(x) }) })
    uvr.Lcs.j <- lapply(uvr.Lcs.j.temp, function(x) { lapply(x, function(x) { if (!is.null(x)) log(x) }) })
    for (c in 1:C) {
      se.Lcs.j[[c]] <- mapply(function(x, y) { if(length(x) > 0) x/y }, 
                              se.Lcs.j.temp[[c]], u.Lcs.j.temp[[c]], SIMPLIFY = FALSE)
      sevr.Lcs.j[[c]] <- mapply(function(x, y) { if(length(x) > 0) x/y }, 
                                sevr.Lcs.j.temp[[c]], uvr.Lcs.j.temp[[c]], SIMPLIFY = FALSE)
    }
  }
  ##details<<
  ## Notation: Index c is used for country, j for observation in a series, 
  ## and s for non-VR series in a country. "L" in front of the index indicates a list.
  ##value<< List of data with 
  data <- list(
    C = C, ##<< Number of countries
    name.c = name.c, ##<< Vector of country names
    iso.c = iso.c, ##<< Vector of 3-character country ISO codes
    uncode.c = uncode.c, ##<< Vector of 3-digit country ISO codes
    hiv.c = hiv.c, ##<< Vector of logical values indicating if HIV adjustment is to be applied to country
    crisisadj.c = crisisadj.c, ##<< Vector of logical values indicating if a crisis adjustment is to be applied to country
    crisisadjfordata.c = crisisadjfordata.c, ##<< Vector of logical values indicating if a crisis adjustment is to be applied to country data before fitting model
    useglobalsmoothing.c = useglobalsmoothing.c, ##<< Vector of logical values indicating if global smoothing is used for country
    # alpha.c = info$alpha.c[select], ##<< Vector of IGME 2012 country Loess alpha parameter values
    # alpha2.c = info$alpha2.c[select], ##<< Vector of IGME 2012 country Loess alpha parameter values 2
    method.c = info$method.c[select], ##<< Vector of IGME country estimation method
    imrmethod.c = info$imrmethod.c[select], ##<< Vector of IGME country IMR estimation method
    sahel.c = info$sahel.c[select], ##<< Vector of Sahel country values, if applicable
    # smallcountry.c = ifelse(info$smallcountry.c == "Small country", 
    #                         TRUE, FALSE)[select], ##<< Vector indicating if country is a small country
    n.c = nvr.c + nnonvr.c, ##<< Vector with number of observations
    nvr.c = nvr.c, ##<< Vector with number of VR observations
    nnonvr.c = nnonvr.c,  ##<< Vector with number of non-VR observations
    nseriesvr.c = nseriesvr.c, ##<< Vector with number of VR series
    nseriesnonvr.c = nseriesnonvr.c, ##<< Vector with number of non-VR series
    nuniqueseriesnonvr.c = nuniqueseriesnonvr.c, ##<< Vector with number of unique non-VR series
    nmax = max(nvr.c + nnonvr.c, na.rm = T), ##<< Maximum number of observations
    nvrmax = max(nvr.c, na.rm = T), ##<< Maximum number of VR observations
    nnonvrmax = max(nnonvr.c, na.rm = T), ##<< Maximum number of non-VR observations
    Smax = max(nseriesnonvr.c, na.rm = T), ##<< Maximum number of non-VR series
    minyear.c = minyear.c, ##<< vector with minimum observation year in each country
    maxyear.c = maxyear.c, ##<< vector with maximum observation year in each country
    u.Lcs.j = u.Lcs.j, ##<< Country-series list with non-VR observations, with DHS Direct (any, with/without SEs) observations arranged after all other observations
    year.Lcs.j = year.Lcs.j, ##<< Country-series list with non-VR observation years
    se.Lcs.j = se.Lcs.j, ##<< Country-series list with non-VR SEs, including missing values
    senonNA.Lcs.j = senonNA.Lcs.j, ##<< Country-series list with non-VR SEs, 0 if missing
    included.Lcs.j = included.Lcs.j, ##<< Country-series list with 1 if non-VR observation is included and 0 otherwise
    sourcetype.Lcs.j = sourcetype.Lcs.j, ##<< Country-series list with non-VR series source type (DHS, MICS, Census or Others)
    method.Lcs.j = method.Lcs.j, ##<< Country-series list with non-VR series method (Direct, Indirect or Others)
    sourceid.Lcs.j = sourceid.Lcs.j, ##<< Country-series list with non-VR series names
    interval.Lcs.j = interval.Lcs.j, ##<< Country-series list with interval lengths of reference periods (for Direct only).
    sourcetype.Lc.s = sourcetype.Lc.s, ##<< Country list with non-VR series source type (DHS, MICS, Census or Others)
    method.Lc.s = method.Lc.s, ##<< Country list with non-VR series method (Direct, Indirect or Others)
    sourceid.Lc.s = sourceid.Lc.s, ##<< Country list with non-VR series names
    source.Lc.s = source.Lc.s, ##<< Country list with non-VR series names (not used?)
    surveyyear.Lc.s = surveyyear.Lc.s, ##<< Country list with non-VR series date
    seriesyear.Lc.s = seriesyear.Lc.s, ##<< Country list with non-VR published series year
    recall.mid = recall.mid, ##<< Mean retrospective period of all data, used to centre retrospective period
    isDHSdirectany.Lc.s = isDHSdirectany.Lc.s, ##<< Country list indicating 1 if non-VR series is DHS Direct any with SE
    isserieslevelbiasatprior.Lc.s = isserieslevelbiasatprior.Lc.s, ##<< Country list indicating 1 if non-VR series has series level bias set at prior and NA otherwise
    hasbias.Lc.s = hasbias.Lc.s, ##<< Country list indicating 1 if non-VR series has relative bias and NA otherwise # change JR, 20140407
    uvr.Lc.j = uvr.Lc.j, ##<< Country list with VR observations, with complete VR observations given before incomplete VR observations within a country
    yearvr.Lc.j = yearvr.Lc.j,##<< Country list with VR observation years
    sevr.Lc.j = sevr.Lc.j, ##<< Country list with VR SEs
    senonNAvr.Lc.j = senonNAvr.Lc.j, ##<< Country list with VR SEs, 0 if missing
    hasbiasvr.Lc.j = hasbiasvr.Lc.j, ##<< Country list indicating 1 if VR observation has relative bias and NA otherwise # change JR, 20140429 
    isincompletevr.Lc.j = isincompletevr.Lc.j, ##<< Country list indicating 1 if incomplete VR, 0 otherwise
    mincompincompletevr.Lc.j = mincompincompletevr.Lc.j, ##<< Country list with minimum level of completeness of VR obs, NA if missing
    maxcompincompletevr.Lc.j = maxcompincompletevr.Lc.j, ##<< Country list with maximum level of completeness of VR obs, NA if missing # change JR, 5 Sep 2013
    uvr.Lcs.j = uvr.Lcs.j, ##<< Country-series list with VR observations
    yearvr.Lcs.j = yearvr.Lcs.j, ##<< Country-series list with VR observation years
    sevr.Lcs.j = sevr.Lcs.j, ##<< Country-series list with VR SEs
    senonNAvr.Lcs.j = senonNAvr.Lcs.j, ##<< Country-series list with VR SEs, 0 if missing
    includedvr.Lcs.j = includedvr.Lcs.j, ##<< Country-series list with 1 if VR observation is included and 0 otherwise
    intervalvr.Lcs.j = intervalvr.Lcs.j, ##<< Country-series list with interval lengths of reference periods for VR.
    hasbiasvr.Lcs.j = hasbiasvr.Lcs.j, ##<< Country-series list indicating 1 if VR observation has relative bias and NA otherwise # change JR, 20140429 
    isincompletevr.Lcs.j = isincompletevr.Lcs.j, ##<< Country-series list indicating 1 if VR series is incomplete VR
    mincompincompletevr.Lcs.j = mincompincompletevr.Lcs.j, ##<< Country-series list with minimum completeness levels of incomplete VR series
    maxcompincompletevr.Lcs.j = maxcompincompletevr.Lcs.j, ##<< Country-series list with maximum completeness levels of incomplete VR series
    sourcevr.Lc.s = sourcevr.Lc.s, ##<< Country list with VR series names
    sourceidvr.Lc.s = sourceidvr.Lc.s ##<< Country list with non-VR series names
  )
  return(data)
} # end function
#----------------------------------------------------------------------
### deprecated as of 5 Jul 2013
# ProcessDataToSetAllDataAsVR <- function( # Process data to set all data as VR data when \code{fit.B2.model} is \code{TRUE} 
#   data,
#   hiv.file, ##<< File path to a .csv with UNAIDS HIV estimates.
#   adj.file ##<< File path to a .csv with crisis estimates.
# ) {
#   for (c in 1:data$C) {
#     # treat non-VR series as VR
#     year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
#     u.i <- c(unlist(data$u.Lcs.j[[c]]), unlist(data$uvr.Lc.j[[c]]))    
#     if (data$crisisadjfordata.c[c]) # get crisis-free data # change JR, 5 Jul
#       u.i <- GetCrisisSubtractedSeries(u.i = u.i, year.i = year.i, iso = data$iso.c[c],
#                                        adj.file = adj.file)
#     if (data$hiv.c[c]) # get HIV-free data for HIV countries
#       u.i <- GetHIVSubtractedSeries(u.i = u.i, year.i = year.i, iso = data$iso.c[c],
#                                     hiv.file = hiv.file)
#     data$yearvr.Lc.j[[c]] <- year.i
#     data$uvr.Lc.j[[c]] <- u.i
#     data$year.Lcs.j[[c]] <- list()
#     data$u.Lcs.j[[c]] <- list()
#   }
#   ##value<<
#   return(data) ##<< \code{data}, with only \code{yearvr.Lc.j}, \code{uvr.Lc.j}, \code{year.Lcs.j} and \code{u.Lcs.j} modified as these are used to get JAGS data.
# }
#----------------------------------------------------------------------
GetIGME <- function(# Read in IGME 2012 estimates for selected countries.
  country.codes, ##<< Vector of country codes.
  is.hiv.country = NULL, ##<< Vector of logical values indicating if country is HIV country where
  ## HIV adjustment needs to be applied, required if \code{get.HIV.removed.data} is \code{TRUE}. 
  estimates.file, ##<< File path to a .csv with IGME 2012 estimates.
  hiv.file, ##<< File path to a .csv with UNAIDS HIV estimates.
  get.HIV.removed.data = FALSE, ##<< Logical value indicating if HIV-removed data are desired.
  get.log.scale.data = FALSE ##<< Logical value indicating if data should be on log scale.
) {
  IGME <- read.csv(file = estimates.file, header = T, as.is = T, stringsAsFactors = F, strip.white = T)
  igme.col.seq <- seq(1, length(colnames(IGME)))[grepl(".5", colnames(IGME))]
  nyears <- length(igme.col.seq)
  igme.years <- sapply(colnames(IGME)[igme.col.seq], ExtractYearFromString)
  u.ct <- matrix(NA, length(country.codes), nyears)
  for (c in 1:length(country.codes)) {
    u.ct[c, ] <- t(IGME[(IGME$ISO.Code == country.codes[c]), igme.col.seq])
  }
  if (get.HIV.removed.data) {
    if (is.null(is.hiv.country)) {
      cat("Error: is.hiv.country is required.")
      return(invisible())
    } else if (length(country.codes) != length(is.hiv.country)) {
      cat("Error: Length of is.hiv.country does not match up with length of country.codes.")
      return(invisible())
    } else {
      for (c in (1:length(country.codes))[is.hiv.country]) {
        u.ct[c, ] <- GetHIVSubtractedSeries(u.i = u.ct[c, ], 
                                            year.i = igme.years, 
                                            iso = country.codes[c],
                                            hiv.file = hiv.file)
      }
    }
  }
  if (get.log.scale.data) {
    u.ct <- log(u.ct)
  }
  ##value<< List containing:
  return(list(u.ct = u.ct, ##<< IGME child mortality estimates for country c in year t (can be NA).
              t = igme.years ##<< IGME estimation years.
  ))
}
#----------------------------------------------------------------------
ExtractYearFromString <- function(# Extracts mid-point year in numeric format from a string.
  string ##<< A single string to extract mid-point year from.
) {
  year <- as.numeric(substr(string, which(strsplit(string, "")[[1]]=='.') - 4, 
                            which(strsplit(string, "")[[1]]=='.') + 1))
  ##value<< Mid-point year
  return(year)
}
