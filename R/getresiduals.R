#----------------------------------------------------------------------
# getresiduals.R
# Jin Rou New, 2013-2014
#----------------------------------------------------------------------
GetResiduals <- function(# Get residuals
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Output directory to save files to. If \code{NULL}, {output/runname} is used.
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles for 90% UIs.
) {
  if (is.null(output.dir))
    output.dir <- file.path("output", runname)
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  nsim <- dim(mcmc.array)[1]*dim(mcmc.array)[2]
  C <- mcmc.meta$data$C
  bias.ci <- stbias.ci <- q.ci <- y.ci <- matrix(NA, C, mcmc.meta$data$nmax)
  ypredict.hivremoved.ciq <- array(NA, dim = c(C, mcmc.meta$data$nmax, length(percentiles)))
  ypredict.cij <- array(NA, dim = c(C, mcmc.meta$data$nmax, nsim))
  if (is.null(mcmc.meta$data.val)) {
    y.ci <- mcmc.meta$jags.data$y.ci
  } else {
    # get y.ci for validation (since test data is set to NA)
    for (c in 1:C) {
      year.i <- c(unlist(mcmc.meta$data$year.Lcs.j[[c]]), unlist(mcmc.meta$data$yearvr.Lc.j[[c]]))
      u.i <- c(unlist(mcmc.meta$data$u.Lcs.j[[c]]), unlist(mcmc.meta$data$uvr.Lc.j[[c]]))
      # get crisis-free data for crisisadjfordata.c countries
      if (mcmc.meta$data$crisisadjfordata.c[c]) {
        u.i <- GetCrisisSubtractedSeries(u.i = u.i, year.i = year.i, iso = mcmc.meta$data$iso.c[c],
                                         adj.file = adj.file)
      } else {
        u.i <- u.i
      }
      # get HIV-free data for HIV countries
      if (mcmc.meta$data$hiv.c[c]) {
        y.i <- log(GetHIVSubtractedSeries(u.i = u.i, year.i = year.i, iso = mcmc.meta$data$iso.c[c],
                                          hiv.file = hiv.file))
      } else {
        y.i <- log(u.i)
      }
      y.ci[c, 1:mcmc.meta$data$n.c[c]] <- y.i
    }
  } # end y.ci
  for (c in 1:C) {
    if (is.null(mcmc.meta$data.val)) {
      indices.normdist <- indices.tdist <- NA
      if (c %in% mcmc.meta$jags.data$getc.normdist.d)
        indices.normdist <- mcmc.meta$jags.data$geti.normdist.cj[c, ]
      if (c %in% mcmc.meta$jags.data$getc.tdist.d)
        indices.tdist <- mcmc.meta$jags.data$geti.tdist.cj[c, ]
      indices.select <- sort(c(indices.normdist[!is.na(indices.normdist)], 
                               indices.tdist[!is.na(indices.tdist)]))
    } else {
      indices.testnormdist <- indices.testtdist <- NA
      if (c %in% mcmc.meta$jags.data$getc.testnormdist.d)
        indices.testnormdist <- mcmc.meta$jags.data$geti.testnormdist.cj[c, ]
      if (c %in% mcmc.meta$jags.data$getc.testtdist.d)
        indices.testtdist <- mcmc.meta$jags.data$geti.testtdist.cj[c, ]
      indices.select <- sort(c(indices.testnormdist[!is.na(indices.testnormdist)], 
                               indices.testtdist[!is.na(indices.testtdist)],))
    }
    for (i in indices.select) {
      ypredict.cij[c, i, ] <- ysamp <- c(mcmc.array[, , paste0("ypredict.ci[", c, ",", i, "]")])
      ypredict.hivremoved.ciq[c, i, ] <- quantile(ysamp, probs = percentiles)
      bias.ci[c, i] <- y.ci[c, i] - mean(ysamp) 
      stbias.ci[c, i] <- bias.ci[c, i]/sd(ysamp) 
      q.ci[c, i] <- mean(ysamp <= y.ci[c, i])
    }
  } # end country loop for ypredict.cij etc.
  save(y.ci, file = file.path(output.dir, "y.ci.rda"))
  save(ypredict.cij, file = file.path(output.dir, "ypredict.cij.rda"))
  save(ypredict.hivremoved.ciq, file = file.path(output.dir, "ypredict.hivremoved.ciq.rda"))
  save(bias.ci, file = file.path(output.dir, "bias.ci.rda"))
  save(stbias.ci, file = file.path(output.dir, "stbias.ci.rda"))
  save(q.ci, file = file.path(output.dir, "q.ci.rda"))
  cat(paste0("Residuals saved to ", output.dir, "\n"))
  return(invisible())
}
#----------------------------------------------------------------------
GetResidualsData <- function(# Get data frame with residuals and covariates for residual analysis.
  runname = "test", ##<< Run name.
  output.dir = NULL, ## Output directory of validation results. If \code{NULL}, defaults to \code{output/runname.full}.
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  demo.indicators.file = NULL, ##<< File path of demographic indicators file. If \code{NULL}, demographic 
  ## indicators data included in package is used. 
  weight.alpha.select = 0.5, ##<< weight.alpha to use for U5MR estimates. # change JR, 2013120: from 0
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles for 90% UIs.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname)
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  if (is.null(demo.indicators.file))
    demo.indicators.file <- file.path("input", "WPP2012_INT_F01_ANNUAL_DEMOGRAPHIC_INDICATORS.csv") # change JR, 20140522
  # read in input files
  demo <- read.csv(file = demo.indicators.file, header = T, skip = 16, # change JR, 20140522
                   stringsAsFactors = F, strip.white = T)
  # get mcmc.meta
  if (!file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat("Error: mcmc.meta does not exist! Do RunMCMC first to get mcmc.meta.\n")
    return(invisible())
  } else {
    load(file.path(output.dir, "mcmc.meta.rda"))
    cat("mcmc.meta for all data loaded.\n")
  }
  # load all required objects
  country.info.file <- mcmc.meta$files$country.info.file
  country.info <- read.csv(file = country.info.file, header = T, 
                           stringsAsFactors = F, strip.white = T)
  hiv.file <- mcmc.meta$files$hiv.file
  load(file.path(output.dir, "year.t.rda"))
  load(file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
  res.hivremoved.cqt <- res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]]
  C <- length(mcmc.meta$data$iso.c)
  load(file.path(output.dir, "mcmc.array.rda")) # change JR, 20140522
  mcmc.array <- mcmc.array[, , grepl("beta.csr", dimnames(mcmc.array)[[3]])]
  load(file.path(output.dir, "bias.ci.rda")) 
  load(file.path(output.dir, "stbias.ci.rda"))  
  load(file.path(output.dir, "q.ci.rda"))
  #----------------------------------------------------------------------
  # get country info
  # get region
  mdgregs.ordered <- c("Sub-Saharan Africa", "Northern Africa", "Latin America", "Caribbean", 
                       "Caucasus and Central Asia", "Eastern Asia", "Southern Asia",
                       "South-eastern Asia", "Western Asia", "Oceania", "Developed regions")
  mdgafrregs.ordered <- c("North Africa", "Eastern Africa", "South Africa", "West Africa", 
                          "Central Africa")
  # get regions
  match <- match(mcmc.meta$data$iso.c, country.info$ISO3Code)
  country.c <- country.info$CountryName[match]
  mdgreg.c <- ifelse(country.info$MDGRegion5 == "", country.info$MDGRegion1, 
                     country.info$MDGRegion5)[match]
  mdgregs <- mdgregs.ordered[is.element(mdgregs.ordered, mdgreg.c)]
  mdgafrreg.c <- country.info$MDGRegion3[match]
  mdgafrregs <- mdgafrregs.ordered[is.element(mdgafrregs.ordered, mdgafrreg.c)]
  # get data frame with country.info
  country.info.summary <- data.frame(iso.i = mcmc.meta$data$iso.c,
                                     country.i = country.c,
                                     region.i = mdgreg.c,
                                     mdgafrreg.i = mdgafrreg.c)
  #----------------------------------------------------------------------
  # get change in TFR in last 15 years before survey
  demo <- demo[demo$Country.code < 900, grepl("Major.area|Reference.date|Total.fertility", colnames(demo))] # exclude region data
  colnames(demo)[grepl("Major.area", colnames(demo))] <- "Country"
  colnames(demo)[grepl("Reference.date", colnames(demo))] <- "Year"
  colnames(demo)[grepl("Total.fertility", colnames(demo))] <- "TFR"
  demo$Country <- StandardiseCountryNames(demo$Country)
  demo$Country[grepl("Ivoire", demo$Country)] <- "Cote d Ivoire"
  # no tfr estimates for 15 countries - small island countries, Monaco and South Sudan!
  name.noTFRestimates <- mcmc.meta$data$name.c[!is.element(mcmc.meta$data$name.c, 
                                                           unique(demo$Country))]
  if (length(name.noTFRestimates) > 0) cat("Warning: No TFR estimates available for:\n")
  cat(paste0(paste(name.noTFRestimates, collapse = ", "), "\n"))
  demo <- demo[is.element(demo$Country, mcmc.meta$data$name.c), ]
   # TFR estimates only available for 179 countries
  cat(paste0("Note: TFR estimates are available for ", length(unique(demo$Country)), 
             " out of ", C, " countries.\n"))
      demo$ISO <- mcmc.meta$data$iso.c[match(demo$Country, mcmc.meta$data$name.c)]
  #----------------------------------------------------------------------
  # get standardised biases for each observation
  iso.i <- sourcetype.i <- source.i <- seriesyear.i <- surveyyear.i <- q5.i <- 
    surveyyearminus15.i <- tfrinsurveyyear.i <- tfrinsurveyyearminus15.i <- changeintfr.i <- 
    year.i <- recallnotcentred.i <- recall.i <- interval.i <- bias.i <- 
    stbias.i <- q.i <- leftoutobs.i <- 
    b0.median.i <- b0.lower.i <- b0.upper.i <- b1.median.i <- b1.lower.i <- b1.upper.i <- NULL
  for (c in 1:C) {
    # non-VR observations
    if (mcmc.meta$data$nnonvr.c[c] > 0) {
      for (s in 1:mcmc.meta$data$nseriesnonvr.c[c]) {
        nobs <- length(mcmc.meta$data$u.Lcs.j[[c]][[s]])
        # series-specific covariates
        source.i <- c(source.i, rep(mcmc.meta$data$source.Lc.s[[c]][s], nobs))
        seriesyear.i <- c(seriesyear.i, rep(mcmc.meta$data$seriesyear.Lc.s[[c]][s], nobs))
        sourcetype.i <- c(sourcetype.i, rep(mcmc.meta$data$typename.cs[c, s], nobs))
        surveyyear.i <- c(surveyyear.i, rep(mcmc.meta$data$surveyyear.Lc.s[[c]][s], nobs))
        surveyyearminus15.i <- c(surveyyearminus15.i, 
                                 rep(mcmc.meta$data$surveyyear.Lc.s[[c]][s]-15, nobs))
        # change JR, 20140522
        parname.b0 <- paste0("beta.csr[", c, ",", s, ",1]")
        if (parname.b0 %in% dimnames(mcmc.array)[[3]]) {
          b0.CIs <- quantile(c(mcmc.array[, , parname.b0]), probs = percentiles)
          b0.median.i <- c(b0.median.i, rep(b0.CIs[2], nobs))
          b0.lower.i <- c(b0.lower.i, rep(b0.CIs[1], nobs))
          b0.upper.i <- c(b0.upper.i, rep(b0.CIs[3], nobs))
        } else {
          cat(paste0("Warning: ", parname.b0, " cannot be found in mcmc.array!"))
          b0.median.i <- c(b0.median.i, rep(NA, nobs))
          b0.lower.i <- c(b0.lower.i, rep(NA, nobs))
          b0.upper.i <- c(b0.upper.i, rep(NA, nobs))         
        }
        parname.b1 <- paste0("beta.csr[", c, ",", s, ",2]")
        if (parname.b1 %in% dimnames(mcmc.array)[[3]]) {
          b1.CIs <- quantile(c(mcmc.array[, , parname.b1]), probs = percentiles)
          b1.median.i <- c(b1.median.i, rep(b1.CIs[2], nobs))
          b1.lower.i <- c(b1.lower.i, rep(b1.CIs[1], nobs))
          b1.upper.i <- c(b1.upper.i, rep(b1.CIs[3], nobs))
        } else {
          cat(paste0("Warning: ", parname.b1, " cannot be found in mcmc.array!"))
          b1.median.i <- c(b1.median.i, rep(NA, nobs))
          b1.lower.i <- c(b1.lower.i, rep(NA, nobs))
          b1.upper.i <- c(b1.upper.i, rep(NA, nobs))
        }
        # get WPP TFR estimate of the survey year and (survey year - 15)
        if (!is.element(mcmc.meta$data$name.c[c], name.noTFRestimates)) {
          # a bit crude because for years earlier or later than 1950 and 2009 we're just taking the closest TFR est!
          # min(surveyyear.i) = 1948, max(surveyyear.i) = 2012
          absdiff.demo <- abs(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]] - 
                                mcmc.meta$data$surveyyear.Lc.s[[c]][s])
          # take max because the survey year may be in the middle of two demo$Year's
          year.select.demo <- max(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]][absdiff.demo == min(absdiff.demo)]) 
          absdiffminus15.demo <- abs(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]] - 
                                       mcmc.meta$data$surveyyear.Lc.s[[c]][s] + 15)
          # take max because the survey year may be in the middle of two demo$Year's
          yearminus15.select.demo <- max(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]][absdiffminus15.demo == min(absdiffminus15.demo)])
          tfrinsurveyyear.i <- c(tfrinsurveyyear.i, 
                                 rep(demo$TFR[demo$ISO == mcmc.meta$data$iso.c[c] & 
                                                demo$Year == year.select.demo], nobs))
          tfrinsurveyyearminus15.i <- c(tfrinsurveyyearminus15.i, 
                                        rep(demo$TFR[demo$ISO == mcmc.meta$data$iso.c[c] & 
                                                       demo$Year == yearminus15.select.demo], nobs))
        } else {
          tfrinsurveyyear.i <- c(tfrinsurveyyear.i, rep(NA, nobs)) 
          tfrinsurveyyearminus15.i <- c(tfrinsurveyyearminus15.i, rep(NA, nobs))
        }
        # observation-specific covariates
        years <- mcmc.meta$data$year.Lcs.j[[c]][[s]]
        year.i <- c(year.i, years)
        recallnotcentred.i <- c(recallnotcentred.i, mcmc.meta$data$surveyyear.Lc.s[[c]][s] - years)
        recall.i <- c(recall.i, mcmc.meta$data$surveyyear.Lc.s[[c]][s] - years - 
                        mcmc.meta$data$recall.mid)
        # get B3 q5 estimate for the obs year (round to nearest estyear available)
        q5s <- NULL
        for (j in 1:nobs) {
          q5s <- c(q5s, res.hivremoved.cqt[c, 2, year.t == 
                                             GetNearestEstimateYear(year = years[j], c = c,
                                                                    year.t = year.t, res.cqt = res.hivremoved.cqt)])
        }
        q5.i <- c(q5.i, q5s)
        interval.i <- c(interval.i, mcmc.meta$data$interval.Lcs.j[[c]][[s]])
      }
    } # end non-VR loop
    # VR observations
    if (mcmc.meta$data$nvr.c[c] > 0) {
      sourcevr <- yearvr <- intervalvr <- tfrinsurveyyearvr <- 
        tfrinsurveyyearminus15vr <- q5vr <- NULL
      for (s in 1:mcmc.meta$data$nseriesvr.c[c]) {
        nobsvr <- length(mcmc.meta$data$uvr.Lcs.j[[c]][[s]])  
        # series-specific covariates
        sourcevr <- c(sourcevr, rep(mcmc.meta$data$sourcevr.Lc.s[[c]][s], nobsvr))
        years <- unlist(mcmc.meta$data$yearvr.Lcs.j[[c]][[s]])
        yearvr <- c(yearvr, years)    
        sourcetype.i <- c(sourcetype.i, rep("VR", nobsvr))        
        
        # observation-specific covariates
        recallnotcentred.i <- c(recallnotcentred.i, rep(0, nobsvr))
        recall.i <- c(recall.i, rep(0, nobsvr) - mcmc.meta$data$recall.mid)
        intervalvr <- c(intervalvr, mcmc.meta$data$intervalvr.Lcs.j[[c]][[s]])
        
        # get WPP TFR estimate of the survey year and (survey year - 15)
        if (!is.element(mcmc.meta$data$name.c[c], name.noTFRestimates)) {
          for (j in 1:nobsvr) {
            # a bit crude because for years earlier or later than 1950 and 2009 we're just taking the closest TFR est!
            # min(surveyyear.i) = 1948, max(surveyyear.i) = 2012
            absdiff.demo <- abs(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]] - years[j])
            # take max because the survey year may be in the middle of two demo$Year's
            year.select.demo <- max(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]][absdiff.demo == min(absdiff.demo)]) 
            absdiffminus15.demo <- abs(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]] - 
                                         years[j] + 15)
            # take max because the survey year may be in the middle of two demo$Year's
            yearminus15.select.demo <- max(demo$Year[demo$ISO == mcmc.meta$data$iso.c[c]][absdiffminus15.demo == min(absdiffminus15.demo)])
            tfrinsurveyyearvr <- c(tfrinsurveyyearvr, 
                                   demo$TFR[demo$ISO == mcmc.meta$data$iso.c[c] & 
                                     demo$Year == year.select.demo])
            tfrinsurveyyearminus15vr <- c(tfrinsurveyyearminus15vr, 
                                          demo$TFR[demo$ISO == mcmc.meta$data$iso.c[c] & 
                                            demo$Year == yearminus15.select.demo])
          }
        } else {
          tfrinsurveyyearvr <- c(tfrinsurveyyearvr, rep(NA, nobsvr)) 
          tfrinsurveyyearminus15vr <- c(tfrinsurveyyearminus15vr, rep(NA, nobsvr))
        }
        # get B3 q5 estimate for the obs year (round to nearest estyear available)
        for (j in 1:nobsvr) {
          q5vr <- c(q5vr, 
                    res.hivremoved.cqt[c, 2, year.t == 
                      GetNearestEstimateYear(year = years[j], c = c,
                                             year.t = year.t, res.cqt = res.hivremoved.cqt)])
        }
      }
      # reorder VR obs by obs year 
      # (because of difference in ordering bt uvr.Lc.j and uvr.Lcs.j)
      ordervr <- order(yearvr)
      # series-specific covariates
      source.i <- c(source.i, sourcevr[ordervr])
      seriesyear.i <- c(seriesyear.i, rep(NA, length(yearvr)))
      surveyyear.i <- c(surveyyear.i, yearvr[ordervr])
      surveyyearminus15.i <- c(surveyyearminus15.i, yearvr[ordervr]-15)
      b0.median.i <- c(b0.median.i, rep(NA, length(yearvr)))
      b0.lower.i <- c(b0.lower.i, rep(NA, length(yearvr)))
      b0.upper.i <- c(b0.upper.i, rep(NA, length(yearvr)))
      b1.median.i <- c(b1.median.i, rep(NA, length(yearvr)))
      b1.lower.i <- c(b1.lower.i, rep(NA, length(yearvr)))
      b1.upper.i <- c(b1.upper.i, rep(NA, length(yearvr)))
      # observation-specific covariates
      year.i <- c(year.i, yearvr[ordervr])
      interval.i <- c(interval.i, intervalvr[ordervr])
      tfrinsurveyyear.i <- c(tfrinsurveyyear.i, tfrinsurveyyearvr[ordervr])
      tfrinsurveyyearminus15.i <- c(tfrinsurveyyearminus15.i, 
                                    tfrinsurveyyearminus15vr[ordervr])
      q5.i <- c(q5.i, q5vr[ordervr])
    } # end VR loop    
    iso.i <- c(iso.i, rep(mcmc.meta$data$iso.c[c], mcmc.meta$data$n.c[c]))
    bias.i <- c(bias.i, bias.ci[c, 1:mcmc.meta$data$n.c[c]])
    stbias.i <- c(stbias.i, stbias.ci[c, 1:mcmc.meta$data$n.c[c]])
    q.i <- c(q.i, q.ci[c, 1:mcmc.meta$data$n.c[c]])
    # left out observations
    if (!is.null(mcmc.meta$data.val)) {
      indices.test <- c(mcmc.meta$data.val$geti.test.cj[c, ])[!is.na(c(mcmc.meta$data.val$geti.test.cj[c, ]))]
      leftoutobs <- rep(0, mcmc.meta$data$n.c[c])
      leftoutobs[indices.test] <- 1
      leftoutobs.i <- c(leftoutobs.i, leftoutobs)
    } else {
      leftoutobs.i <- c(leftoutobs.i, rep(0, mcmc.meta$data$n.c[c]))
    }
  } # end country loop
  recallnotcentred.i <- ifelse(is.element(sourcetype.i, c("VR", 
                                                          "Others Life Table", "Others Household Deaths", 
                                                          "Others Others")), 0, recallnotcentred.i)
  recall.i <- ifelse(is.element(sourcetype.i, c("VR", 
                                                "Others Life Table", "Others Household Deaths", 
                                                "Others Others")), 0, recall.i)
  decreaseinTFRin15years.i <- tfrinsurveyyearminus15.i - tfrinsurveyyear.i
  
  hist(decreaseinTFRin15years.i) # should be mostly > 0!
  
  # get country and region info
  res <- join(data.frame(iso.i = iso.i), country.info.summary)
  country.i <- res$country.i
  region.i <- res$region.i
  afrregion.i <- res$mdgafrreg.i
  
  # check that no intervals are NA for DHS Direct?
  print(table(interval.i[sourcetype.i == "DHS Direct"]))
  #print(data.frame(country.i, source.i)[interval.i == "" & sourcetype.i == "DHS Direct", ])
  interval.i[interval.i == "" & sourcetype.i != "VR"] <- 5
  interval.i[source.i == "Afghanistan Mortality Survey"] <- 2 
  # check
  # length(unlist(mcmc.meta$data$u.Lcs.j)[!is.na(unlist(mcmc.meta$data$u.Lcs.j))]) == length(country.i); length(region.i);
  # length(sourcetype.i); length(surveyyear.i); length(q5.i); length(tfrinsurveyyear.i)
  # length(year.i); length(recall.i); length(bias.i); length(stbias.i); length(q.i) # 5104; 6260 series obs in total
  # check that it is the island countries + south sudan without TFR estimates
  # unique(country.i[is.na(tfrinsurveyyear.i) | is.na(tfrinsurveyyearminus15.i)])
  
  residuals <- data.frame(country.i = res$country.i, 
                          iso.i = iso.i, 
                          region.i = region.i, 
                          afrregion.i = afrregion.i, 
                          source.i = source.i, 
                          seriesyear.i = seriesyear.i,
                          sourcetype.i = sourcetype.i, 
                          surveyyear.i = surveyyear.i, 
                          q5.i = q5.i,
                          year.i = year.i, 
                          recallnotcentred.i = recallnotcentred.i, 
                          recall.i = recall.i, 
                          interval.i = interval.i,
                          b0.median.i = b0.median.i,
                          b0.lower.i = b0.lower.i,
                          b0.upper.i = b0.upper.i,
                          b1.median.i = b1.median.i,
                          b1.lower.i = b1.lower.i,
                          b1.upper.i = b1.upper.i, 
                          bias.i = bias.i,
                          absbias.i = abs(bias.i),
                          stbias.i = stbias.i,
                          absstbias.i = abs(stbias.i),
                          q.i = q.i,
                          decreaseinTFRin15years.i = decreaseinTFRin15years.i, 
                          tfrinsurveyyear.i = tfrinsurveyyear.i, 
                          tfrinsurveyyearminus15.i = tfrinsurveyyearminus15.i, 
                          surveyyearminus15.i = surveyyearminus15.i,
                          leftoutobs.i = leftoutobs.i)
  print(head(residuals))
  write.csv(residuals, file = file.path(output.dir, "residuals.csv"), row.names = F)
  cat(paste0("Residuals file written to ", output.dir, ".\n"))
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------------
GetNearestEstimateYear <- function(
  year, 
  c,
  year.t, 
  res.cqt
) {
  absdiff.t <- abs(year.t-year)
  return(max(max(year.t[absdiff.t == min(absdiff.t)]), # take max because the year may be in the middle of two year.t's
             min(year.t[!is.na(res.cqt[c, 2, ])]))) # take the earliest year with an estimate
}
