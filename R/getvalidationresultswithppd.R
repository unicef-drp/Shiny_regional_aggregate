#----------------------------------------------------------------------
# getvalidationresultswithppd.R
# Jin Rou New & Leontine Alkema, 2013
#----------------------------------------------------------------------
GetValidationResultsWithPPD <- function(# Get results of validation exercise with posterior predictive distribution.
  runname.full, ##<< Run name of full run.
  runname.val, ##<< Run name of validation run.
  output.dir = NULL, ##<<## Output directory of validation results. If \code{NULL}, defaults to \code{output/runname.val}.
  ntrials = 100, ##<< Number of trials for validation exercise.
  indicator.type = "U5MR", 
  weight.alpha.select = 0.5, ##<< weight.alpha of choice.
  weight.alpha.publish = 0.5, ##<< weight.alpha used for final estimates.
  year.cutoff = 2006, ##<< Cut-off year used for validation exercise.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  isos.to.exclude = c("SOM") ##<< Vector of 3-character ISO country codes of countries to exclude
  ## By default, Somalia is excluded because it is a crisis countries with special tweaks. # change JR, 20150602
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname.val)
  
  # load validation run results
  load(file.path("output", runname.val, "mcmc.meta.rda"))
  load(file.path("output", runname.val, "y.ci.rda"))
  load(file.path("output", runname.val, "ypredict.cij.rda"))
  hiv.file <- mcmc.meta$files$hiv.file
  adj.file <- mcmc.meta$files$adj.file
  
  output.dir.res <- file.path(output.dir, paste0("ValPPD (kappa ", weight.alpha.select, ")"))
  dir.create(output.dir.res, showWarnings = F)
  
  # process ypredict.cij to get ypredicts for weight.alpha.select
  if (weight.alpha.select != 0) {
    load(file.path("output", runname.val, "year.t.rda"))
    load(file.path("output", runname.val, "res.cqt.Lw.rda"))
    weights.alpha.plusdefault <- as.numeric(names(res.cqt.Lw))
    nweightsplus1 <- length(weights.alpha.plusdefault)
    weights.alpha <- weights.alpha.plusdefault[weights.alpha.plusdefault != 0]
    nweights <- length(weights.alpha)
    w <- which(as.character(weights.alpha) == as.character(weight.alpha.select)) # change JR, 20140416
    C <- mcmc.meta$data$C
    
    if (!file.exists(file.path(output.dir.res, "results.val.ppd.rda"))) {
      if (!file.exists(file.path(output.dir, paste0("ypredictnew", w, ".cij.rda")))) {
        load(file.path("output", runname.val, "u5full.ctj.rda"))
        load(file.path("output", runname.val, paste0("u5new", w, ".ctj.rda")))
        eval(parse(text = paste0("logu5.ctj <- logu5new", w, ".ctj <- array(NA, dim(u5full.ctj))")))
        eval(parse(text = paste0("ypredictnew", w, ".cij <- array(NA, dim(ypredict.cij))")))
        diffsplines.cij <- array(NA, dim(ypredict.cij))
        
        # get HIV and crisis-free trajectories on log scale
        logu5.ctj <- log(u5full.ctj)
        eval(parse(text = paste0("logu5new", w, ".ctj <- log(u5new", w, ".ctj)")))
        for (c in (1:C)[mcmc.meta$data$hiv.c | mcmc.meta$data$crisisadj.c]) {
          for (type in c("unmelded", "melded")) {
            if (type == "unmelded") {
              u5.tj <- u5full.ctj[c, , ]
            } else {
              eval(parse(text = paste0("u5.tj <- u5new", w, ".ctj[c, , ]")))
            }
            # crisis post-adjustment
            if (mcmc.meta$data$crisisadj.c[c]) {
              u.median2.t <- apply(u5.tj, 1, median)
              propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median2.t, year.t = year.t, 
                                                      iso = mcmc.meta$data$iso.c[c],
                                                      operation = "-",
                                                      adj.file = adj.file)$propadj.t
              u5temp2.tj <- apply(u5.tj, 2, "*", propadj.t)
            } else {
              u5temp2.tj <- u5.tj
            }
            if (mcmc.meta$data$hiv.c[c]) {
              u.median.t <- apply(u5temp2.tj, 1, median)
              propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                                      iso = mcmc.meta$data$iso.c[c],
                                                      operation = "-",
                                                      hiv.file = hiv.file)$propadjhiv.t
              u5tempnew.tj <- apply(u5temp2.tj, 2, "*", propadjhiv.t)
            } else {
              u5tempnew.tj <- u5temp2.tj
            }
            if (type == "unmelded") {
              logu5.ctj[c, , ] <- log(u5tempnew.tj)
            } else {
              eval(parse(text = paste0("logu5new", w, ".ctj[c, , ] <- log(u5tempnew.tj)")))
            }
          } # end type unmelded/melded
        } # end country loop
        
        # get diffsplines (difference between unmelded and melded trajectories on log scale)
        eval(parse(text = paste0("diffsplines.ctj <- logu5new", w, ".ctj-logu5.ctj")))
        cat("Difference between unmelded and melded trajectories on log scale obtained for estimate years.\n")
        
        # get diffsplines for observation years
        for (c in 1:C) {
          cat(paste0("Country ", c, " of ", C, " (", mcmc.meta$data$iso.c[c], ")\n"))
          datayears <- mcmc.meta$jags.data$year.ci[c, ]
          for (j in 1:dim(diffsplines.ctj)[3]) {
            diffsplines <- diffsplines.ctj[c, , j]
            diffsplines.cij[c, , j] <- approx(x = year.t, y = diffsplines, xout = datayears, 
                                              method = "linear", rule = 1)$y
            diffsplines.cij[c, , j] <- ifelse(datayears < min(year.t), 0, diffsplines.cij[c, , j]) # difference in splines only in extrapolation period
          }
        }
        cat("Difference between unmelded and melded trajectories on log scale obtained for observation years.\n")
        
        eval(parse(text = paste0("ypredictnew", w, ".cij <- ypredict.cij + diffsplines.cij")))
        eval(parse(text = paste0("diffsplines", w, ".cij <- diffsplines.cij")))
        eval(parse(text = paste0("save(diffsplines", w, ".cij, file = file.path(output.dir, \"diffsplines", 
                                 w, ".cij.rda", "\"))")))
        eval(parse(text = paste0("save(ypredictnew", w, ".cij, file = file.path(output.dir, \"ypredictnew", 
                                 w, ".cij.rda", "\"))")))  
        eval(parse(text = paste0("ypredict.cij <- ypredictnew", w, ".cij")))
        save(logu5.ctj, file = file.path(output.dir, "logu5.ctj.rda"))
        eval(parse(text = paste0("save(logu5new", w, ".ctj, file = file.path(output.dir, \"logu5new", 
                                 w, ".ctj.rda", "\"))")))
        cat("ypredict for weight.alpha.select = ", weight.alpha.select, " obtained.\n")
      }
      eval(parse(text = paste0("load(file = file.path(output.dir, \"ypredictnew", 
                               w, ".cij.rda", "\"))")))  
      eval(parse(text = paste0("ypredict.cij <- ypredictnew", w, ".cij")))
    } 
  } # end loop to get ypredictnew.cij
    
  # get country info
  iso.val.c <- mcmc.meta$data$iso.c
  name.c <- mcmc.meta$data$name.c
  C <- length(iso.val.c)
  select.crisis <- is.element(iso.val.c, isos.to.exclude)
  
  # load and process resfull.cqt and year.full.t (needs to correspond to iso.val.c used here!)
  load(file.path("output", runname.full, "iso.c.rda"))
  load(file.path("output", runname.full, "year.t.rda"))
  load(file.path("output", runname.full, "res.cqt.Lw.rda"))
  iso.full.c <- iso.c
  year.full.t <- year.t
  resfull.cqt.temp <- res.cqt.Lw[[paste0(weight.alpha.publish)]]
  resfull.cqt <- resfull.cqt.temp[match(iso.val.c, iso.full.c), , ]
  
  if (!file.exists(file.path(output.dir.res, "results.val.ppd.rda"))) {
    cat("Generating samples of left-out observations...\n")
    # select a random sample of one left-out observation per country (repeat ntrials times)
    index.sample.beforeandinclyearcutoffminus1.cs <- index.sample.afteryearcutoffminus1.cs <- 
      y.beforeandinclyearcutoffminus1.cs <- y.afteryearcutoffminus1.cs <- matrix(NA, C, ntrials)
    ytilde.beforeandinclyearcutoffminus1.cqs <- ytilde.afteryearcutoffminus1.cqs <- array(NA, c(C, length(percentiles), ntrials))
    for (trial in 1:ntrials) {
      cat(paste0("Trial ", trial, " of ", ntrials, " trials.\n"))
      for (c in 1:C) {
        set.seed(trial*as.numeric(mcmc.meta$data$uncode.c[c])) # change JR, 20140508
        indices.sample <- mcmc.meta$data.val$geti.test.cj[c, 1:mcmc.meta$data.val$ntest.c[c]]
        # distinguish between observations with reference dates <= year.cutoff-1 and > year.cutoff-1
        indices.beforeandinclyearcutoffminus1 <- (1:mcmc.meta$data$n.c[c])[mcmc.meta$jags.data$year.ci[c, ] <= year.cutoff-1]
        indices.beforeandinclyearcutoffminus1 <- indices.beforeandinclyearcutoffminus1[!is.na(indices.beforeandinclyearcutoffminus1)]
        indices.afteryearcutoffminus1 <- (1:mcmc.meta$data$n.c[c])[mcmc.meta$jags.data$year.ci[c, ] > year.cutoff-1]
        indices.afteryearcutoffminus1 <- indices.afteryearcutoffminus1[!is.na(indices.afteryearcutoffminus1)]
        # incomplete VR observations have no ypredicts, so do not sample from those
        indices.incompletevr <- (1:mcmc.meta$data$n.c[c])[mcmc.meta$data$is.incompletevrany.ci[c, ] == 1]
        indices.incompletevr <- indices.incompletevr[!is.na(indices.incompletevr)]
        # get sampled indices
        indices.sample.beforeandinclyearcutoffminus1 <- indices.sample[!is.element(indices.sample, indices.incompletevr) &
          is.element(indices.sample, indices.beforeandinclyearcutoffminus1)]
        indices.sample.afteryearcutoffminus1 <- indices.sample[!is.element(indices.sample, indices.incompletevr) &
          is.element(indices.sample, indices.afteryearcutoffminus1)]
        if (sum(!is.na(indices.incompletevr)) > 0)
          if (trial == 1)
            cat(paste0("Note for country ", c, " of ", C, " countries (", iso.val.c[c], 
                       "): Has incomplete observations without ypredicts.\n"))
        # sample from indices and get ypredicts and y's
        # before and including year.cutoff-1
        if (length(indices.sample.beforeandinclyearcutoffminus1) > 0) {
          if (length(indices.sample.beforeandinclyearcutoffminus1) == 1) {
            index.sample.beforeandinclyearcutoffminus1.cs[c, trial] <- indices.sample.beforeandinclyearcutoffminus1
          } else {
            index.sample.beforeandinclyearcutoffminus1.cs[c, trial] <- sample(indices.sample.beforeandinclyearcutoffminus1, 1)
          }
          ysamp <- ypredict.cij[c, index.sample.beforeandinclyearcutoffminus1.cs[c, trial], ]
          if (sum(!is.na(ysamp)) > 0) { 
            ytilde.beforeandinclyearcutoffminus1.cqs[c, , trial] <- quantile(ysamp, probs = percentiles)
          } else {
            cat(paste0("Warning for trial ", trial, ", country ", c, " of ", C, " countries (", iso.val.c[c], 
                       "): All NAs obtained for posterior predictive sample of y's (before and including ", year.cutoff-1, ").\n"))
            ytilde.beforeandinclyearcutoffminus1.cqs[c, , trial] <- rep(NA, length(percentiles))
          }
          y.beforeandinclyearcutoffminus1.cs[c, trial] <- y.ci[c, index.sample.beforeandinclyearcutoffminus1.cs[c, trial]]
        } else {
          if (trial == 1) # print warning once
            cat(paste0("Warning for country ", c, " of ", C, " countries (", iso.val.c[c], 
                       "): No observations in test set before and including ", year.cutoff-1, ".\n"))
        }
        # after year.cutoff-1
        if (length(indices.sample.afteryearcutoffminus1) > 0) {
          if (length(indices.sample.afteryearcutoffminus1) == 1) {
            index.sample.afteryearcutoffminus1.cs[c, trial] <- indices.sample.afteryearcutoffminus1
          } else {
            index.sample.afteryearcutoffminus1.cs[c, trial] <- sample(indices.sample.afteryearcutoffminus1, 1)
          }
          ysamp <- ypredict.cij[c, index.sample.afteryearcutoffminus1.cs[c, trial], ]
          if (sum(!is.na(ysamp)) > 0) {    
            ytilde.afteryearcutoffminus1.cqs[c, , trial] <- quantile(ysamp, probs = percentiles)
          } else {
            cat(paste0("Warning for trial ", trial, ", country ", c, " of ", C, " countries (", iso.val.c[c], 
                       "): All NAs obtained for posterior predictive sample of y's (after ", year.cutoff-1, ".\n"))
            ytilde.afteryearcutoffminus1.cqs[c, , trial] <- rep(NA, length(percentiles))
          }
          y.afteryearcutoffminus1.cs[c, trial] <- y.ci[c, index.sample.afteryearcutoffminus1.cs[c, trial]]
        } else {
          if (trial == 1) # print warning once
            cat(paste0("Warning for country ", c, " of ", C, " countries (", iso.val.c[c], 
                       "): No observations in test set after ", year.cutoff-1, ".\n"))
        }
      } # end country loop
    } # end trial loop
    # compile results
    results.val.ppd <- list(y.beforeandinclyearcutoffminus1.cs = y.beforeandinclyearcutoffminus1.cs,
                            ytilde.beforeandinclyearcutoffminus1.cqs = ytilde.beforeandinclyearcutoffminus1.cqs,
                            y.afteryearcutoffminus1.cs = y.afteryearcutoffminus1.cs,
                            ytilde.afteryearcutoffminus1.cqs = ytilde.afteryearcutoffminus1.cqs,
                            iso.c = iso.val.c,
                            name.c = name.c,
                            percentiles = percentiles,
                            ntrials = ntrials)
    save(results.val.ppd, file = file.path(output.dir.res, "results.val.ppd.rda"))
    cat(paste0("Samples of left-out observations saved to ", output.dir.res, "\n."))
  } else {
    load(file = file.path(output.dir.res, "results.val.ppd.rda"))
    cat(paste0("Samples of left-out observations loaded from ", output.dir.res, "\n."))
  }
  C <- length(results.val.ppd$iso.c)
  #-----------------------------------------------------------------------------
  filename <- file.path(output.dir.res, "validation_PPD_summary.txt")
  fileout <- file(filename, open = "wt")
  sink(fileout, split = T)
  cat(paste("Summary of validation exercise with posterior predictive distributions is written to file ", 
            filename), "\n")
  types.subset.countries <- c("All non-HIV countries", 
                              "High mortality non-HIV countries", "Low mortality non-HIV countries")
  indicators <- c(paste0("Before and including ", year.cutoff-1), paste0("After ", year.cutoff-1))
  cat("#---------- Coverage and error results ----------#\n")
  for (subset.countries in types.subset.countries) {
    table.belowabove.all <- table.belowabove.all.output <- NULL
    cat("#----------------------------------------#\n")
    cat(paste0(subset.countries, "\n"))
    cat("#----------------------------------------#\n")
    for (indicator in indicators) {
      if (indicator == paste0("Before and including ", year.cutoff-1)) {
        y.cs <- results.val.ppd$y.beforeandinclyearcutoffminus1.cs
        ytilde.cqs <- results.val.ppd$ytilde.beforeandinclyearcutoffminus1.cqs
      } else {
        y.cs <- results.val.ppd$y.afteryearcutoffminus1.cs
        ytilde.cqs <- results.val.ppd$ytilde.afteryearcutoffminus1.cqs
      }
      u.cs <- exp(y.cs)
      utilde.cqs <- exp(ytilde.cqs)
      diff.cs <- u.cs - utilde.cqs[, 2, ]
      reldiff.cs <- diff.cs/u.cs
      below.cs <- above.cs <- intervalscore.cs <- matrix(NA, C, ntrials) # change JR, 5 Aug 2013
      for (trial in 1:ntrials) {
        results <- GetBelowAndAbove(CIs.tr.cq = utilde.cqs[, , trial], est.c = u.cs[, trial],
                                    alpha = 1-percentiles[3]+percentiles[1]) # change JR, 5 Aug 2013
        below.cs[, trial] <- results$below.c
        above.cs[, trial] <- results$above.c
        intervalscore.cs[, trial] <- results$intervalscore.c # change JR, 5 Aug 2013
      }
      if (subset.countries == "All non-HIV countries") {
        select <- which(!is.na(resfull.cqt[, 2, year.full.t == 1990.5]) & mcmc.meta$data$hiv.c == F &
                          !select.crisis & !is.na(u.cs[, 1]))
      } else if (subset.countries == "High mortality non-HIV countries") {
        select <- which(resfull.cqt[, 2, year.full.t == 1990.5] >= 40 & mcmc.meta$data$hiv.c == F & 
                          !select.crisis & !is.na(u.cs[, 1]) &
                          !is.na(resfull.cqt[, 2, year.full.t == 1990.5]))
      } else {
        select <- which(resfull.cqt[, 2, year.full.t == 1990.5] < 40 & mcmc.meta$data$hiv.c == F & 
                          !select.crisis & !is.na(u.cs[, 1]) &
                          !is.na(resfull.cqt[, 2, year.full.t == 1990.5]))
      }
      cat("#----------------------------------------#\n")
      cat(paste0(indicator, "\n"))
      cat("#----------------------------------------#\n")
      cat(paste0("Number of countries selected: ", length(select), "\n"))
      cat(paste0("Countries selected are: ", paste(name.c[select], collapse = ", "), "\n"))

      below.c <- apply(below.cs, 1, sum) > 0
      above.c <- apply(above.cs, 1, sum) > 0
      cat(paste0("Countries with data below PIs are: ", 
                 paste(intersect(mcmc.meta$data$name.c[below.c], mcmc.meta$data$name.c[select]), collapse = ", "), "\n"))
      cat(paste0("Countries with data above PIs are: ", 
                 paste(intersect(mcmc.meta$data$name.c[above.c], mcmc.meta$data$name.c[select]), collapse = ", "), "\n"))

      # summarise into table
      error <- error.mean <- belowabove <- nbelowabove <- 
        intervalscore <- intervalscore.mean <- NULL # change JR, 5 Aug 2013  
      for (trial in 1:ntrials) {
        error <- rbind(error, c(median(diff.cs[select, trial], na.rm = T),
                                median(abs(diff.cs[select, trial]), na.rm = T),
                                median(reldiff.cs[select, trial]*100, na.rm = T),
                                median(abs(reldiff.cs[select, trial])*100, na.rm = T)))
        error.mean <- rbind(error.mean, c(mean(diff.cs[select, trial], na.rm = T),
                                          mean(abs(diff.cs[select, trial]), na.rm = T),
                                          mean(reldiff.cs[select, trial]*100, na.rm = T),
                                          mean(abs(reldiff.cs[select, trial])*100, na.rm = T)))
        belowabove <- rbind(belowabove, c(mean(below.cs[select, trial]*100, na.rm = T), 
                                          mean(above.cs[select, trial]*100, na.rm = T)))
        nbelowabove <- rbind(nbelowabove, c(sum(below.cs[select, trial], na.rm = T), 
                                            sum(above.cs[select, trial], na.rm = T)))
        intervalscore <- rbind(intervalscore, 
                               c(median(intervalscore.cs[select, trial], na.rm = T))) # change JR, 5 Aug 2013
        intervalscore.mean <- rbind(intervalscore.mean, 
                                    c(mean(intervalscore.cs[select, trial], na.rm = T))) # change JR, 5 Aug 2013
      }
      table.belowabove <- cbind(nbelowabove, belowabove, error, error.mean, 
                                intervalscore, intervalscore.mean) # change JR, 5 Aug 2013
      table.belowabove.all <- rbind(indicator,
                                    table.belowabove,
                                    apply(table.belowabove, 2, median),
                                    apply(table.belowabove, 2, sd))
      table.belowabove.all.output <- cbind(table.belowabove.all.output, table.belowabove.all)
    } # end indicator loop
    rownames(table.belowabove.all.output) <- c("Indicator", paste("Trial", 1:ntrials), 
                                               "Median over all trials", "Sd over all trials")
    colnames(table.belowabove.all.output) <- rep(c("ncountries below", "ncountries above", 
                                                   "Percentage below", "Percentage above", 
                                                   "ME", "MAE", "MRE", "MARE", 
                                                   "ME mean", "MAE mean", "MRE mean", "MARE mean",
                                                   "Interval score", "Interval score mean"), 2) # change JR, 5 Aug 2013  
    write.csv(table.belowabove.all.output,
              file = file.path(output.dir.res, paste0("Coverage and errors for PPD", " (", subset.countries, ").csv")))
  } # end subset.countries loop
  cat("#---------- End of validation results ----------#\n")
  sink()
  closeAllConnections()
  ##value<< \code{NULL}.
  return(invisible())
}
