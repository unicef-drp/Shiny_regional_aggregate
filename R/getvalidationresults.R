#----------------------------------------------------------------------
# getvalidationresults.R
# Leontine Alkema & Jin Rou New, 2012-2013
#----------------------------------------------------------------------
GetValidationResults <- function(# Get results of validation exercise.
  runname.full, ##<< Run name of full run.
  runname.val, ##<< Run name of validation run.
  output.dir = NULL, ##<< Output directory of validation results. If \code{NULL}, defaults to \code{output/runname.full}.
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  estimates.file = NULL, ##<< If \code{NULL}, UN IGME estimates file (median estimates only) included in package is used. 
  file.defaultloess.res = file.path("output", runname.full, paste0("res_", indicator.type, "_IGMEdefault.rda")),
  file.defaultloessval.res = file.path("output", runname.full, paste0("res_", indicator.type, "_IGMEdefaultupto2006.rda")),
  file.igmeold.iso.c = "output/IGME2012/iso.c_IGME2012.rda",
  file.igmeold.year.t = "output/IGME2012/year.t_IGME2012.rda",
  file.igmeold.res.cqt = "output/IGME2012/res.cqt_IGME2012.rda",
  global.run = FALSE, ##<< Results used are from global run (or combined country-specific runs)?
  indicator.type = "U5MR", 
  get.plots = TRUE ,##<< Get plots?
  year.cutoff = 2006, ##<< Cut-off year used for validation exercise.
  weight.alpha.select = 0.5, ##<< Selected pooling weight.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  isos.to.exclude = c("SOM") ##<< Vector of 3-character ISO country codes of countries to exclude
  ## By default, Somalia is excluded because it is a crisis countries with special tweaks. # change JR, 20150602
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname.full)
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  hiv.file <- mcmc.meta$files$hiv.file # change JR, 20140508
  rm(mcmc.meta)
  if (is.null(estimates.file))
    estimates.file <- file.path("input", paste0(indicator.type, "_un.csv"))
  
  # load validation run results
  load(file.path("output", runname.val, "iso.c.rda"))
  load(file.path("output", runname.val, "year.t.rda"))
  load(file.path("output", runname.val, "res.cqt.Lw.rda"))
  load(file.path("output", runname.val, "resARR.cq.Lwy.rda"))
  iso.val.c <- iso.c
  year.val.t <- year.t
  res.val.cqt.Lw <- res.cqt.Lw
  resARR.val.cq.Lwy <- resARR.cq.Lwy
  
  # load full run results
  load(file.path("output", runname.full, "iso.c.rda"))
  load(file.path("output", runname.full, "year.t.rda"))
  load(file.path("output", runname.full, "res.cqt.Lw.rda"))
  load(file.path("output", runname.full, "resARR.cq.Lwy.rda"))
  iso.full.c <- iso.c
  year.full.t <- year.t
  res.full.cqt.Lw.temp <- res.cqt.Lw
  resARR.full.cq.Lwy.temp <- resARR.cq.Lwy
  
  weights.alpha.plusdefault <- names(res.full.cqt.Lw.temp)
  nweightsplus1 <- length(weights.alpha.plusdefault)
  
  res.full.cqt.Lw <- resARR.full.cq.Lwy <- list()
  for (w in 1:nweightsplus1) {
    res.full.cqt.Lw[[w]] <- res.full.cqt.Lw.temp[[w]][match(iso.val.c, iso.full.c), , is.element(year.full.t, year.val.t)]
    resARR.full.cq.Lwy[[w]] <- list()
    for (y in 1:length(resARR.full.cq.Lwy.temp[[1]])) {
      resARR.full.cq.Lwy[[w]][[y]] <- resARR.full.cq.Lwy.temp[[w]][[y]][match(iso.val.c, iso.full.c), ]
    }
    names(resARR.full.cq.Lwy[[w]]) <- names(resARR.full.cq.Lwy.temp[[w]])
  }
  names(res.full.cqt.Lw) <- names(res.full.cqt.Lw.temp)
  names(resARR.full.cq.Lwy) <- names(resARR.full.cq.Lwy.temp)
  
  # get mcmc.meta
  if (!file.exists(file.path("output", runname.val, "mcmc.meta.rda"))) {
    cat("Error: mcmc.meta does not exist! Do RunMCMC first to get mcmc.meta.\n")
    return(invisible())
  } else {
    load(file.path("output", runname.val, "mcmc.meta.rda"))
    cat("mcmc.meta for all validation data loaded.\n")
  }
  
  if (!file.exists(file.path("output", runname.val, "igme.rda"))) {
    igme <- GetIGME(country.codes = mcmc.meta$data$iso.c,
                    is.hiv.country = mcmc.meta$data$hiv.c,  
                    estimates.file = estimates.file,
                    hiv.file = hiv.file)
    save(igme, file = file.path("output", runname.val, "igme.rda"))
    cat("IGME estimates obtained.\n")
  } else {
    load(file.path("output", runname.val, "igme.rda"))
    cat("IGME estimates read in.\n")
  }
  
  C <- length(iso.val.c)
  name.c <- mcmc.meta$data$name.c
  select.crisis <- is.element(mcmc.meta$data$iso.c, isos.to.exclude)
  #----------------------------------------------------------------------
  # plots: comparison of validation and full run
  if (get.plots) {
    legend.append <- ifelse(global.run, "Global run", "Country-specific run")
    # plot all full runs on one plot and all validation runs on the other
    pdf(file = file.path(fig.dir, paste(runname.full, "Full and Validation Results 1.pdf")), 
        width = 14, height = 7)
    layout(matrix(c(1:2), 1, 2, byrow = T))
    for (c in 1:C) {
      for (is.validation in c(FALSE, TRUE)) {
        if (is.validation) {
          res.plot.cqt.Lw <- res.val.cqt.Lw
        } else {
          res.plot.cqt.Lw <- res.full.cqt.Lw
        }
        PlotDataAndEstimates(data = mcmc.meta$data,
                             c = c,
                             est.years = year.val.t,
                             ylab = mcmc.meta$settings$indicator.type,
                             plot.se = TRUE,
                             excludedobsandyears.Lc.i2 = mcmc.meta$data.val$excludedobsandyears.Lc.i2,
                             CIs.cqt = res.plot.cqt.Lw[[1]],
                             CIs.tr.cqt = res.plot.cqt.Lw[[2]],
                             CIs.iid.cqt = res.plot.cqt.Lw[[3]],
                             CIs2.cqt = res.plot.cqt.Lw[[4]],
                             CIs3.cqt = res.plot.cqt.Lw[[5]],
                             CIs4.cqt = res.plot.cqt.Lw[[6]],
                             legendfull = paste0(legend.append, ": weight.alpha = ", weights.alpha.plusdefault[1]),
                             legendtr = paste0(legend.append, ": weight.alpha = ", weights.alpha.plusdefault[2]),
                             legendiid = paste0(legend.append, ": weight.alpha = ", weights.alpha.plusdefault[3]),
                             legend2 = paste0(legend.append, ": weight.alpha = ", weights.alpha.plusdefault[4]),
                             legend3 = paste0(legend.append, ": weight.alpha = ", weights.alpha.plusdefault[5]),
                             legend4 = paste0(legend.append, ": weight.alpha = ", weights.alpha.plusdefault[6]),
                             Ytr.c = rep(year.cutoff, C),
                             zoom = FALSE, addlegend = FALSE, mfrow.suppress = TRUE,
                             cex.adj.factor = 0.6,
                             igme = igme)
        if (is.validation) {
          mtext("Validation runs")
        } else {
          mtext("Full runs")
        }
      }
    }
    dev.off()
    cat("Full and validation results on same plot plotted.\n")
    
    # plot results for different weight.alphas on separate plots
    pdf(file = file.path(fig.dir, paste(runname.full, "Full and Validation Results 2.pdf")), 
        width = ceiling(nweightsplus1/2)*7, height = 14)
    layout(matrix(c(1:(2*ceiling(nweightsplus1/2))), 2, ceiling(nweightsplus1/2), byrow = T))
    for (c in 1:C) {
      for (w in 1:nweightsplus1) {
        PlotDataAndEstimates(data = mcmc.meta$data,
                             c = c,
                             est.years = year.val.t,
                             ylab = mcmc.meta$settings$indicator.type,
                             plot.se = TRUE,
                             excludedobsandyears.Lc.i2 = mcmc.meta$data.val$excludedobsandyears.Lc.i2,
                             CIs.cqt = res.full.cqt.Lw[[paste0(weights.alpha.plusdefault[w])]],
                             CIs.tr.cqt = res.val.cqt.Lw[[paste0(weights.alpha.plusdefault[w])]],
                             legendfull = paste0(legend.append, ": All data"),
                             legendtr = paste0(legend.append, ": Data up to ", year.cutoff),
                             Ytr.c = rep(year.cutoff, C),
                             zoom = FALSE, addlegend = FALSE, mfrow.suppress = TRUE,
                             igme = igme)
        mtext(paste0("weight.alpha = ", weights.alpha.plusdefault[w]))
      }
      if (nweightsplus1 < (2*ceiling(nweightsplus1/2))) {
        EmptyPlot()
      }
    }
    dev.off()
    cat("Full and validation results on separate plots for different weights plotted.\n")
    
    pdf(file = file.path(fig.dir, paste(runname.full, "Full and Validation Results (Final).pdf")), 
        width = 21, height = 7)
    for (c in 1:C) {
      PlotDataAndEstimates(data = mcmc.meta$data,
                           c = c,
                           est.years = year.val.t,
                           ylab = mcmc.meta$settings$indicator.type,
                           plot.se = TRUE,
                           excludedobsandyears.Lc.i2 = mcmc.meta$data.val$excludedobsandyears.Lc.i2,
                           CIs.cqt = res.full.cqt.Lw[[paste0(weight.alpha.select)]],
                           CIs.tr.cqt = res.val.cqt.Lw[[paste0(weight.alpha.select)]],
                           legendfull = paste0(legend.append, ": All data"),
                           legendtr = paste0(legend.append, ": Data up to ", year.cutoff),
                           Ytr.c = rep(year.cutoff, C),
                           igme = igme)
    }
    dev.off()
    cat("Full and validation results on same plot plotted for selected weight.alpha.\n")
  } # end get.plots
  
  #-----------------------------------------------------------------------------
  # Coverage
  filename <- file.path(output.dir, "validation_summary.txt")
  fileout <- file(filename, open = "wt")
  sink(fileout, split = T)
  cat(paste("Summary of validation exercise is written to file ", filename), "\n")
  
  types.subset.countries <- c("All non-HIV countries", 
                              "High mortality non-HIV countries", "Low mortality non-HIV countries") 
  
  cat("#---------- Coverage and error results ----------#\n")
  for (subset.countries in types.subset.countries) {
    table.belowabove.all <- table.belowabove.output.all <- NULL
    cat("#----------------------------------------#\n")
    cat(paste0(subset.countries, "\n"))
    cat("#----------------------------------------#\n")
    for (w in 1:nweightsplus1) {
      # for estimates
      resval.cqt <- res.val.cqt.Lw[[w]]
      resfull.cqt <- res.full.cqt.Lw[[w]]
      # for ARR 1990-2005
      resARR.val.cq <- resARR.val.cq.Lwy[[w]][[paste0("1990.5-", year.cutoff-0.5)]]
      resARR.full.cq <- resARR.full.cq.Lwy[[w]][[paste0("1990.5-", year.cutoff-0.5)]]
      nyears <- length(year.val.t)
      diff.ct <- reldiff.ct <- below.ct <- above.ct <- intervalscore.ct <- # change JR, 29 Oct 2013 
        matrix(NA, C, nyears)
      for (t in 1:nyears) {
        diff.ct[, t] <- resfull.cqt[, 2, t] - resval.cqt[, 2, t]
        reldiff.ct[, t] <- diff.ct[, t]/resfull.cqt[, 2, t]
        results <- GetBelowAndAbove(CIs.tr.cq = resval.cqt[, , t], est.c = resfull.cqt[, 2, t],
                                    alpha = 1-percentiles[3]+percentiles[1])
        below.ct[, t] <- results$below.c
        above.ct[, t] <- results$above.c
        intervalscore.ct[, t] <- results$intervalscore.c # change JR, 29 Oct 2013
      }
      diffARR.c <- resARR.full.cq[, 2] - resARR.val.cq[, 2]
      reldiffARR.c <- diffARR.c/resARR.full.cq[, 2]
      resultsARR <- GetBelowAndAbove(CIs.tr.cq = resARR.val.cq, est.c = resARR.full.cq[, 2],
                                     1-percentiles[3]+percentiles[1], log.scale = F)
      belowARR.c <- resultsARR$below.c
      aboveARR.c <- resultsARR$above.c
      intervalscoreARR.c <- resultsARR$intervalscore.c
      if (subset.countries == "All non-HIV countries") {
        select <- which(!is.na(resfull.cqt[, 2, year.val.t == 1990.5]) & mcmc.meta$data$hiv.c == F &
                          !select.crisis)
      } else if (subset.countries == "High mortality non-HIV countries") {
        select <- which(resfull.cqt[, 2, year.val.t == 1990.5] >= 40 & mcmc.meta$data$hiv.c == F & 
                          !select.crisis &
                          !is.na(resfull.cqt[, 2, year.val.t == 1990.5]))
      } else {
        select <- which(resfull.cqt[, 2, year.val.t == 1990.5] < 40 & mcmc.meta$data$hiv.c == F & 
                          !select.crisis &
                          !is.na(resfull.cqt[, 2, year.val.t == 1990.5]))
      }
      if (w == 1) {
        cat(paste0("Number of countries selected: ", length(select), "\n"))
        cat(paste0("Countries selected are: ", paste(name.c[select], collapse = ", "), "\n"))
      }
      cat(paste0("B3: weight.alpha = ", weights.alpha.plusdefault[w], "\n"))
      
      # countries which are below/above
      countries.below <- countries.above <- NULL
      for (t in 1:nyears) {
        countries.below <- c(countries.below, name.c[below.ct[, t] == 1])
        countries.above <- c(countries.above, name.c[above.ct[, t] == 1])
      }
      cat("Countries below:\n")
      print(intersect(unique(countries.below[!is.na(countries.below)]), name.c[select]))
      cat("Countries above:\n")
      print(intersect(unique(countries.above[!is.na(countries.above)]), name.c[select]))
      cat("Countries below (for ARR):\n")
      print(name.c[belowARR.c == 1 & !is.na(belowARR.c) & is.element(1:C, select)])
      cat("Countries above (for ARR):\n")
      print(name.c[aboveARR.c == 1 & !is.na(aboveARR.c) & is.element(1:C, select)])
      cat("\n")
      
      # summarise into table
      error <- error.mean <- belowabove <- nbelowabove <- intervalscore <- intervalscore.mean <- NULL
      for (t in 1:nyears) {
        error <- rbind(error, c(median(diff.ct[select, t], na.rm = T),
                                median(abs(diff.ct[select, t]), na.rm = T),
                                median(reldiff.ct[select, t]*100, na.rm = T),
                                median(abs(reldiff.ct[select, t])*100, na.rm = T)))
        error.mean <- rbind(error.mean, c(mean(diff.ct[select, t], na.rm = T),
                                          mean(abs(diff.ct[select, t]), na.rm = T),
                                          mean(reldiff.ct[select, t]*100, na.rm = T),
                                          mean(abs(reldiff.ct[select, t])*100, na.rm = T)))
        belowabove <- rbind(belowabove, c(mean(below.ct[select, t]*100, na.rm = T), 
                                          mean(above.ct[select, t]*100, na.rm = T)))
        nbelowabove <- rbind(nbelowabove, c(sum(below.ct[select, t], na.rm = T), 
                                            sum(above.ct[select, t], na.rm = T)))
        intervalscore <- rbind(intervalscore, c(median(intervalscore.ct[select, t], na.rm = T))) # change JR, 29 Oct 2013
        intervalscore.mean <- rbind(intervalscore.mean, c(mean(intervalscore.ct[select, t], na.rm = T))) # change JR, 29 Oct 2013
      }
      error <- rbind(error, c(median(diffARR.c[select], na.rm = T),
                              median(abs(diffARR.c[select]), na.rm = T),
                              median(reldiffARR.c[select]*100, na.rm = T),
                              median(abs(reldiffARR.c[select])*100, na.rm = T)))
      error.mean <- rbind(error.mean, c(mean(diffARR.c[select], na.rm = T),
                                        mean(abs(diffARR.c[select]), na.rm = T),
                                        mean(reldiffARR.c[select]*100, na.rm = T),
                                        mean(abs(reldiffARR.c[select])*100, na.rm = T))) 
      belowabove <- rbind(belowabove, c(mean(belowARR.c[select]*100, na.rm = T), 
                                        mean(aboveARR.c[select]*100, na.rm = T)))
      nbelowabove <- rbind(nbelowabove, c(sum(belowARR.c[select], na.rm = T), 
                                          sum(aboveARR.c[select], na.rm = T)))
      intervalscore <- rbind(intervalscore, c(median(intervalscoreARR.c[select], na.rm = T)))
      intervalscore.mean <- rbind(intervalscore.mean, c(mean(intervalscoreARR.c[select], na.rm = T)))
      table.belowabove <- rbind(weights.alpha.plusdefault[w],
                                cbind(nbelowabove, belowabove, error, error.mean, 
                                      intervalscore, intervalscore.mean))
      rownames(table.belowabove) <- c("weight.alpha", year.val.t, paste0("ARR 1990-", year.cutoff-1))
      colnames(table.belowabove) <- c("ncountries below", "ncountries above", "Percentage below", "Percentage above", 
                                      "ME", "MAE", "MRE", "MARE",
                                      "ME mean", "MAE mean", "MRE mean", "MARE mean",
                                      "Interval score", "Interval score mean")
      table.belowabove.all <- cbind(table.belowabove.all, table.belowabove)
    }
    write.csv(table.belowabove.all,
              file = file.path(output.dir, paste0("Coverage and errors", " (", subset.countries, ").csv")))
  }
  # output to latex
  # require(xtable)
  # belowabove <- round(belowabove)
  # table.belowabove.tex <- rbind(belowabove[rownames(belowabove) == "1990.5"],
  #                               belowabove[rownames(belowabove) == "2000.5"],
  #                               belowabove[rownames(belowabove) == "2005.5"],
  #                               belowabove[rownames(belowabove) == "ARR 1990-2005"])
  # colnames(table.belowabove.tex) <- c("Below", "Above")
  # rownames(table.belowabove.tex) <- c("U5MR 1990", "U5MR 2000", "U5MR 2005", "ARR 1990-2005")
  # xtable(table.belowabove.tex)
  #-----------------------------------------------------------------------------
  # Errors at years.compare
  cat("#---------- Errors results ----------#\n")
  years.compare <- c(1990, 2000, 2005) + 0.5
  
  for (subset.countries in types.subset.countries) {
    cat("#----------------------------------------#\n")
    cat(paste0(subset.countries, "\n"))
    cat("#----------------------------------------#\n")
    for (w in 1:nweightsplus1) {
      # for estimates
      resval.cqt <- res.val.cqt.Lw[[w]]
      resfull.cqt <- res.full.cqt.Lw[[w]]
      # for ARR 1990-2005
      resARR.val.cq <- resARR.val.cq.Lwy[[w]][[paste0("1990.5-", year.cutoff-0.5)]]
      resARR.full.cq <- resARR.full.cq.Lwy[[w]][[paste0("1990.5-", year.cutoff-0.5)]]
      
      # select countries
      if (subset.countries == "All non-HIV countries") {
        select <- which(!is.na(resfull.cqt[, 2, year.val.t == 1990.5]) & mcmc.meta$data$hiv.c == F &
                          !select.crisis)        
      } else if (subset.countries == "High mortality non-HIV countries") {
        select <- which(resfull.cqt[, 2, year.val.t == 1990.5] >= 40 & mcmc.meta$data$hiv.c == F &
                          !select.crisis &
                          !is.na(resfull.cqt[, 2, year.val.t == 1990.5]))
      } else {
        select <- which(resfull.cqt[, 2, year.val.t == 1990.5] < 40 & mcmc.meta$data$hiv.c == F &
                          !select.crisis &
                          !is.na(resfull.cqt[, 2, year.val.t == 1990.5]))
      }
      if (w == 1) cat(paste0("Number of countries selected: ", length(select), "\n"))
      
      # get errors at last observation year
      diff <- reldiff <- matrix(NA, length(select), length(years.compare))
      for (t in 1:length(years.compare)) {
        diff[, t] <- resfull.cqt[select, 2, year.val.t == years.compare[t]] - 
          resval.cqt[select, 2, year.val.t == years.compare[t]]
        reldiff[, t] <- diff[, t]/resfull.cqt[select, 2, year.val.t == years.compare[t]] # change JR, 26 May: resval to resfull
        # note: divide by resval
      }
      # for ARR
      diff <- cbind(diff, resARR.full.cq[select, 2] - resARR.val.cq[select, 2])
      reldiff <- cbind(reldiff, diff[, ncol(diff)]/resARR.full.cq[select, 2]) # change JR, 26 May: resARR.val.cq to resARR.full.cq
      absdiff <- abs(diff)
      absreldiff <- abs(reldiff)
      diffs <- data.frame(diff, absdiff)
      reldiffs <- data.frame(reldiff*100, absreldiff*100)
      rownames(diffs) <- rownames(reldiffs) <- name.c[select]
      colnames(diffs) <- colnames(reldiffs) <- rep(c(paste("U5MR", floor(years.compare)), paste0("ARR 1990.5-", year.cutoff-0.5)), 2)
      diffs.summary <- apply(diffs, 2, summary)
      reldiffs.summary <- apply(reldiffs, 2, summary)
      # output B3 validation results
      cat(paste0("B3: weight.alpha = ", weights.alpha.plusdefault[w], "\n"))
      cat("B3[2006]-B3[2012] and abs(B3[2006]-B3[2012])\n")
      print(diffs.summary)
      cat("(B3[2006]-B3[2012])/B3[2006] and abs(B3[2006]-B3[2012])B3[2006]\n")
      print(reldiffs.summary)
      cat("\n")
    }
    #----------------------------------------------------------------------
    # output results for loess
    year.valloess.t <- 1990.5:2011.5
    # load default loess full run
    load(file.defaultloess.res)
    isofulldef <- res$igme$iso.c
    resfulldef <- res
    resfulldef$igme$u.ct <- resfulldef$igme$u.ct[match(iso.val.c, isofulldef), is.element(resfulldef$igme$t, year.valloess.t)]
    
    # load default loess validation run
    load(file.defaultloessval.res)
    isovaldef <- res$igme$iso.c
    resvaldef <- res
    resvaldef$igme$u.ct <- resvaldef$igme$u.ct[match(iso.val.c, isovaldef), is.element(resvaldef$igme$t, year.valloess.t)]
    
    # load UN IGME 2012 results
    load(file.igmeold.iso.c)
    load(file.igmeold.year.t)
    load(file.igmeold.res.cqt)
    isofulldef2 <- iso.c
    resfulldef2 <- list(); resfulldef2$igme <- list()
    resfulldef2$igme$t <- year.t
    resfulldef2$igme$u.ct <- res.cqt[match(iso.val.c, isofulldef2), 2, is.element(resfulldef2$igme$t, year.valloess.t)]
    
    # calculate ARR
    resfulldef$igme$ARR.c <- log(resfulldef$igme$u.ct[, year.valloess.t == 1990.5]/
                                   resfulldef$igme$u.ct[, year.valloess.t == year.cutoff-0.5])/(year.cutoff-0.5-1990.5)*100
    resvaldef$igme$ARR.c <- log(resvaldef$igme$u.ct[, year.valloess.t == 1990.5]/
                                  resvaldef$igme$u.ct[, year.valloess.t == year.cutoff-0.5])/(year.cutoff-0.5-1990.5)*100
    resfulldef2$igme$ARR.c <- log(resfulldef2$igme$u.ct[, year.valloess.t == 1990.5]/
                                    resfulldef2$igme$u.ct[, year.valloess.t == year.cutoff-0.5])/(year.cutoff-0.5-1990.5)*100
    
    # default loess
    diff.default <- reldiff.default <- diff2.default <- reldiff2.default <- 
      matrix(NA, length(select), length(years.compare))
    for(t in 1:length(years.compare)) {
      diff.default[, t] <- resfulldef$igme$u.ct[select, year.valloess.t == years.compare[t]] - 
        resvaldef$igme$u.ct[select, year.valloess.t == years.compare[t]]
      reldiff.default[, t] <- diff.default[, t]/resfulldef$igme$u.ct[select, year.valloess.t == years.compare[t]] # change JR, 26 May: resvaldef to resfulldef
      diff2.default[, t] <- resfulldef2$igme$u.ct[select, year.valloess.t == years.compare[t]] - 
        resvaldef$igme$u.ct[select, year.valloess.t == years.compare[t]]
      reldiff2.default[, t] <- diff2.default[, t]/resfulldef2$igme$u.ct[select, year.valloess.t == years.compare[t]] # change JR, 26 May: resvaldef to resfulldef
    }
    diff.default <- cbind(diff.default, resfulldef$igme$ARR.c[select] - resvaldef$igme$ARR.c[select])
    reldiff.default <- cbind(reldiff.default, diff.default[, ncol(diff.default)]/resfulldef$igme$ARR.c[select]) # change JR, 26 May: resvaldef to resfulldef
    diff2.default <- cbind(diff2.default, resfulldef2$igme$ARR.c[select] - resvaldef$igme$ARR.c[select])
    reldiff2.default <- cbind(reldiff2.default, diff2.default[, ncol(diff2.default)]/resfulldef2$igme$ARR.c[select]) # change JR, 26 May: resvaldef to resfulldef
    absdiff.default <- abs(diff.default)
    absreldiff.default <- abs(reldiff.default)
    absdiff2.default <- abs(diff2.default)
    absreldiff2.default <- abs(reldiff2.default)
    diffs.default <- data.frame(diff.default, absdiff.default)
    reldiffs.default <- data.frame(reldiff.default*100, absreldiff.default*100)
    diffs2.default <- data.frame(diff2.default, absdiff2.default)
    reldiffs2.default <- data.frame(reldiff2.default*100, absreldiff2.default*100)
    rownames(diffs.default) <- rownames(diffs2.default) <- rownames(reldiffs.default) <- 
      rownames(reldiffs2.default) <- name.c[select]
    colnames(diffs.default) <- colnames(diffs2.default) <- colnames(reldiffs.default) <- 
      colnames(reldiffs2.default) <- rep(c(paste("U5MR", floor(years.compare)), 
                                           paste0("ARR 1990-", year.cutoff-1)), 2)
    diffs.default.summary <- apply(diffs.default, 2, summary)
    reldiffs.default.summary <- apply(reldiffs.default, 2, summary)
    diffs2.default.summary <- apply(diffs2.default, 2, summary)
    reldiffs2.default.summary <- apply(reldiffs2.default, 2, summary)
    # output default loess validation results
    cat("Loess[2006]-Loess[2012] and abs(Loess[2006]-Loess[2012])\n")
    print(diffs.default.summary)
    cat("(Loess[2006]-Loess[2012])/Loess[2006] and abs(Loess[2006]-Loess[2012])/Loess[2006]\n")
    print(reldiffs.default.summary)
    cat("\n")
    # output default loess validation results 2
    cat("Loess[2006]-U[2012] and abs(Loess[2006]-U[2012])\n")
    print(diffs2.default.summary)
    cat("(Loess[2006]-U[2012])/Loess[2006] and abs(Loess[2006]-U[2012])/Loess[2006]\n")
    print(reldiffs2.default.summary)
    cat("\n")
  }
  cat("#---------- End of validation results ----------#\n")
  sink()
  closeAllConnections()
  ##value<< \code{NULL}.
  return(invisible())
}
#----------------------------------------------------------------------
GetBelowAndAbove <- function( # Calculate how often updated estimate is outside uncertainty intervals
  CIs.tr.cq, ##<< Matrix/array of uncertainty intervals and estimates by country. 
  est.c, ##<< Vector of updated estimates by country.
  alpha = NULL, ##<< \code{alpha} for \code{(1-alpha) x 100% uncertainty intervals}, required to calculate interval score.
  log.scale = TRUE ##<< Use log scale for interval score?
) { 
  # note: q's indices 1 and 3 are used!
  below.c <- ifelse(CIs.tr.cq[, 1] > est.c, 1, 0)
  above.c <- ifelse(CIs.tr.cq[, 3] < est.c, 1, 0)
  
  if (!is.null(alpha)) {
    if (log.scale) {
      intervalscore.c <- (log(CIs.tr.cq[, 3]) - log(CIs.tr.cq[, 1])) + 
        2/alpha*(log(CIs.tr.cq[, 1]) - log(est.c))*below.c + 
        2/alpha*(log(est.c) - log(CIs.tr.cq[, 3]))*above.c
    } else {
      intervalscore.c <- (CIs.tr.cq[, 3] - CIs.tr.cq[, 1]) + 
        2/alpha*(CIs.tr.cq[, 1] - est.c)*below.c + 
        2/alpha*(est.c - CIs.tr.cq[, 3])*above.c
    }
  } else {
    intervalscore.c <- NULL
  }
  ##value<<
  return(list(below.c = below.c, ##<< Vector of logical values indicating if estimate is below uncertainty intervals for the country
              above.c = above.c,  ##<< Vector of logical values indicating if estimate is above uncertainty intervals for the country
              intervalscore.c = intervalscore.c ##<< Vector of interval scores
              
  ))
}
