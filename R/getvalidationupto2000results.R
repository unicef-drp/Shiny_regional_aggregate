#----------------------------------------------------------------------
# getvalidationupto2000results.R
#----------------------------------------------------------------------
GetValidationUpTo2000Results <- function(
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  year1 = NULL, ##<< Start year of ARR for validation. If \code{NULL}, validation results not computed for ARR.
  year2 = NULL, ##<< End year of ARR for validation. If \code{NULL}, validation results not computed for ARR.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  get.plots = TRUE ##<< Get plots?
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  
  # load results
  load(file.path(output.dir, "igme.rda"))
  load(file = file.path(output.dir, "iso.c.rda"))
  load(file = file.path(output.dir, "name.c.rda"))
  load(file = file.path(output.dir, "year.t.rda"))
  for (is.validation.up.to.2000 in c(T, F)) {
    name.append <- ifelse(is.validation.up.to.2000, "(upto2000).rda", ".rda")
    load(file.path(output.dir, paste0("res.cqt.Lw", name.append)))
    load(file.path(output.dir, paste0("resARR.cq.Lwy", name.append)))
    if (is.validation.up.to.2000) {
      res.val.cqt.Lw <- res.cqt.Lw
      resARR.val.cq.Lwy <- resARR.cq.Lwy
    } else {
      res.full.cqt.Lw <- res.cqt.Lw
      resARR.full.cq.Lwy <- resARR.cq.Lwy
    }
  }
  
  weights.alpha.plusdefault <- names(res.full.cqt.Lw)
  nweightsplus1 <- length(weights.alpha.plusdefault)
  
  if (get.plots) {
    # plot all full runs on one plot and all validation runs on the other
    pdf(file = file.path(fig.dir, paste(runname, "Full and Validation Results (Bayesian melding) 1.pdf")), 
        width = 14, height = 7)
    layout(matrix(c(1:2), 1, 2, byrow = T))
    for (c in 1:mcmc.meta$data$C) {
      for (is.validation.up.to.2000 in c(FALSE, TRUE)) {
        if (is.validation.up.to.2000) {
          res.plot.cqt.Lw <- res.val.cqt.Lw
        } else {
          res.plot.cqt.Lw <- res.full.cqt.Lw
        }
        PlotDataAndEstimates(data = mcmc.meta$data,
                             c = c,
                             est.years = year.t,
                             ylab = indicator.type,
                             plot.se = TRUE,
                             CIs.cqt = res.plot.cqt.Lw[[1]],
                             CIs.tr.cqt = res.plot.cqt.Lw[[2]],
                             CIs.iid.cqt = res.plot.cqt.Lw[[3]],
                             CIs2.cqt = res.plot.cqt.Lw[[4]],
                             CIs3.cqt = res.plot.cqt.Lw[[5]],
                             CIs4.cqt = res.plot.cqt.Lw[[6]],
                             legendfull = paste0("Global run: weight.alpha = ", weights.alpha.plusdefault[1]),
                             legendtr = paste0("Global run: weight.alpha = ", weights.alpha.plusdefault[2]),
                             legendiid = paste0("Global run: weight.alpha = ", weights.alpha.plusdefault[3]),
                             legend2 = paste0("Global run: weight.alpha = ", weights.alpha.plusdefault[4]),
                             legend3 = paste0("Global run: weight.alpha = ", weights.alpha.plusdefault[5]),
                             legend4 = paste0("Global run: weight.alpha = ", weights.alpha.plusdefault[6]),
                             Ytr.c = rep(2000, mcmc.meta$data$C),
                             zoom = FALSE,
                             addlegend = FALSE,
                             mfrow.suppress = TRUE,
                             cex.adj.factor = 0.7,
                             igme = igme)
        if (is.validation.up.to.2000) {
          mtext("Validation runs")
        } else {
          mtext("Full runs")
        }
      }
    }
    dev.off()
    cat("Full and validation results on same plot plotted.\n")
    
    # plot results for different weight.alphas on separate plots
    pdf(file = file.path(fig.dir, paste(runname, "Full and Validation Results (Bayesian melding) 2.pdf")), 
        width = ceiling(nweightsplus1/2)*7, height = 14)
    layout(matrix(c(1:(2*ceiling(nweightsplus1/2))), 2, ceiling(nweightsplus1/2), byrow = T))
    for (c in 1:mcmc.meta$data$C) {
      for (w in 1:nweightsplus1) {
        PlotDataAndEstimates(data = mcmc.meta$data,
                             c = c,
                             est.years = year.t,
                             ylab = indicator.type,
                             plot.se = TRUE,
                             CIs.cqt = res.full.cqt.Lw[[paste0(weights.alpha.plusdefault[w])]],
                             CIs.tr.cqt = res.val.cqt.Lw[[paste0(weights.alpha.plusdefault[w])]],
                             legendfull = "Global run",
                             legendtr = "Global run: project from 2000",
                             Ytr.c = rep(2000, mcmc.meta$data$C),
                             zoom = FALSE, addlegend = FALSE, mfrow.suppress = TRUE,
                             igme = igme)
        mtext(paste0("weight.alpha = ", weights.alpha.plusdefault[w]))
      }
    }
    dev.off()
    cat("Full and validation results on separate plots for different weights plotted.\n")
  }
  #-----------------------------------------------------------------------------
  # Coverage and errors
  filename <- file.path(output.dir, "validation (up to 2000)_summary.txt")
  fileout <- file(filename, open = "wt")
  sink(fileout, split = T)
  cat(paste("Summary of validation (up to 2000) exercise is written to file ", filename), "\n")
  
  types.subset.countries <- c("All countries", 
                              "High mortality countries", "Low mortality countries") 
  
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
      if (!is.null(year1) & !is.null(year2)) { 
        # for ARR 2000-2005
        resARR.val.cq <- resARR.val.cq.Lwy[[w]][[paste0(year1, "-", year2)]]
        resARR.full.cq <- resARR.full.cq.Lwy[[w]][[paste0(year1, "-", year2)]]
      }
      nyears <- length(year.t)
      diff.ct <- reldiff.ct <- below.ct <- above.ct <- intervalscore.ct <- matrix(NA, mcmc.meta$data$C, nyears)
      for (t in 1:nyears) { # change JR, 22 May
        diff.ct[, t] <- resfull.cqt[, 2, t] - resval.cqt[, 2, t]
        reldiff.ct[, t] <- diff.ct[, t]/resfull.cqt[, 2, t] # divide by resfull so comparable across weights.alpha
        results <- GetBelowAndAbove(CIs.tr.cq = resval.cqt[, , t], est.c = resfull.cqt[, 2, t], 
                                    alpha = 1-percentiles[3]+percentiles[1])
        below.ct[, t] <- results$below.c
        above.ct[, t] <- results$above.c
        intervalscore.ct[, t] <- results$intervalscore.c 
      }
      if (!is.null(year1) & !is.null(year2)) {
        diffARR.c <- resARR.full.cq[, 2] - resARR.val.cq[, 2]
        reldiffARR.c <- diffARR.c/resARR.full.cq[, 2] # divide by resfull so comparable across weights.alpha
        resultsARR <- GetBelowAndAbove(CIs.tr.cq = resARR.val.cq, est.c = resARR.full.cq[, 2],
                                       alpha = 1-percentiles[3]+percentiles[1], log.scale = F)
        belowARR.c <- resultsARR$below.c
        aboveARR.c <- resultsARR$above.c
        intervalscoreARR.c <- resultsARR$intervalscore.c
      }
      if (subset.countries == "All countries") { # ATG excluded because NA for 1990.5
        select <- which(!is.na(resfull.cqt[, 2, year.t == 1990.5]))
      } else if (subset.countries == "High mortality countries") {
        select <- which(resfull.cqt[, 2, year.t == 1990.5] >= 40 &
                          !is.na(resfull.cqt[, 2, year.t == 1990.5]))
      } else {
        select <- which(resfull.cqt[, 2, year.t == 1990.5] < 40 &
                          !is.na(resfull.cqt[, 2, year.t == 1990.5]))
      }
      if (w == 1) cat(paste0("Number of countries selected: ", length(select), "\n"))
      cat(paste0("weight.alpha = ", weights.alpha.plusdefault[w], "\n"))
      
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
      if (!is.null(year1) & !is.null(year2)) {
        cat("Countries below (for ARR):\n")
        print(name.c[belowARR.c == 1 & !is.na(belowARR.c) & is.element(1:mcmc.meta$data$C, select)])
        cat("Countries above (for ARR):\n")
        print(name.c[aboveARR.c == 1 & !is.na(aboveARR.c) & is.element(1:mcmc.meta$data$C, select)])
      }
      cat("\n")
      # summarise into table
      error <- belowabove <- nbelowabove <- intervalscore <- NULL
      for (t in 1:nyears) {
        error <- rbind(error, c(median(diff.ct[select, t], na.rm = T),
                                median(abs(diff.ct[select, t]), na.rm = T),
                                median(reldiff.ct[select, t]*100, na.rm = T),
                                median(abs(reldiff.ct[select, t])*100, na.rm = T)))
        belowabove <- rbind(belowabove, c(mean(below.ct[select, t]*100, na.rm = T), 
                                          mean(above.ct[select, t]*100, na.rm = T)))
        nbelowabove <- rbind(nbelowabove, c(sum(below.ct[select, t], na.rm = T), 
                                            sum(above.ct[select, t], na.rm = T)))
        intervalscore <- rbind(intervalscore, c(mean(intervalscore.ct[select, t], na.rm = T)))
      }
      if (!is.null(year1) & !is.null(year2)) {
        error <- rbind(error, c(median(diffARR.c[select], na.rm = T),
                                median(abs(diffARR.c[select]), na.rm = T),
                                median(reldiffARR.c[select]*100, na.rm = T),
                                median(abs(reldiffARR.c[select])*100, na.rm = T)))
        belowabove <- rbind(belowabove, c(mean(belowARR.c[select]*100, na.rm = T), 
                                          mean(aboveARR.c[select]*100, na.rm = T)))
        nbelowabove <- rbind(nbelowabove, c(sum(belowARR.c[select], na.rm = T), 
                                            sum(aboveARR.c[select], na.rm = T)))
        intervalscore <- rbind(intervalscore, c(mean(intervalscoreARR.c[select], na.rm = T)))
      } 
      
      ####################
      ######## mean or median???? colnames for error, need to update
      
      
      table.belowabove <- rbind(weights.alpha.plusdefault[w], 
                                cbind(belowabove, nbelowabove, intervalscore))
      table.belowabove.output <- cbind(error, belowabove, nbelowabove, intervalscore)
      if (!is.null(year1) & !is.null(year2)) {
        rownames(table.belowabove) <- c("weight.alpha", year.t, paste0("ARR ", year1-0.5, "-", year2-0.5))
        table.belowabove.output <- data.frame(weights.alpha.plusdefault[w],
                                              c(paste0("Estimate in ", year.t-0.5), 
                                                paste0("ARR ", year1-0.5, "-", year2-0.5)), 
                                              table.belowabove.output)
      } else {
        rownames(table.belowabove) <- c("weight.alpha", year.t)
        table.belowabove.output <- data.frame(weights.alpha.plusdefault[w],
                                              c(paste0("Estimate in ", year.t-0.5), 
                                                table.belowabove.output))
      }
      colnames(table.belowabove) <- c("% below", "% above", "ncountries below", "ncountries above",
                                      "mean(intervalscore)")
      colnames(table.belowabove.output) <- c("weight.alpha", "indicator", 
                                             "mean.error", "mean.relative.error",
                                             "percentage.below", "percentage.above", 
                                             "ncountries.below", "ncountries.above",
                                             "mean.intervalscore")
      table.belowabove.all <- cbind(table.belowabove.all, table.belowabove)
      table.belowabove.output.all <- rbind(table.belowabove.output.all, table.belowabove.output)
    }
    write.csv(table.belowabove.all,
              file = file.path(output.dir, paste0("Coverage and errors ", subset.countries, ".csv")), 
              row.names = F, na = "")
    write.csv(table.belowabove.output.all,
              file = file.path(output.dir, paste0("Coverage and error output ", subset.countries, ".csv")), 
              row.names = F, na = "")
  }
  #-----------------------------------------------------------------------------
  # Errors at last observation year and in 2011.5
  cat("#---------- Errors results ----------#\n")
  # which countries have no data after 2000?
  for (atLastObservationYear in c(T, F)) {
    if (atLastObservationYear) {
      print("Countries with no data after the year 2000")
      print(name.c[which(mcmc.meta$data$maxyear.c < 2000)])
    }
    for (subset.countries in types.subset.countries) {
      cat("#----------------------------------------#\n")
      cat(paste0(subset.countries, "\n"))
      cat("#----------------------------------------#\n")
      pdf(file = file.path(fig.dir, paste0(runname, " Errors ", ifelse(atLastObservationYear, 
                                                                            "at last observation year", 
                                                                            "in 2011.5"), 
                                           subset.countries, ".pdf")), width = 12)
      layout(matrix(c(1:nweightsplus1), 1, nweightsplus1, byrow = T))
      par(oma = c(0, 0, 3, 0))
      for(difftype in 1:2) {
        for (w in 1:nweightsplus1) {
          resval.cqt <- res.val.cqt.Lw[[w]]
          resfull.cqt <- res.full.cqt.Lw[[w]]
          
          # select countries
          if (subset.countries == "All countries") { # ATG excluded because NA for 1990.5
            select <- which(mcmc.meta$data$maxyear.c >= 2000)
          } else if (subset.countries == "High mortality countries") {
            select <- which(mcmc.meta$data$maxyear.c >= 2000 & 
                              resfull.cqt[ , 2, year.t == 1990.5] >= 40)
          } else {
            select <- which(mcmc.meta$data$maxyear.c >= 2000 & 
                              resfull.cqt[ , 2, year.t == 1990.5] < 40)
          }
          if (difftype == 1 & w == 1) cat(paste0("Number of countries selected: ", length(select), "\n"))
          
          # get errors at last observation year
          diff <- reldiff <- estyear.c <- rep(NA, mcmc.meta$data$C)
          for (c in (1:mcmc.meta$data$C)[select]) {
            estyear.c[c] <- ifelse(atLastObservationYear, max(year.t[year.t < mcmc.meta$data$maxyear.c[c]]),
                                   2011.5)
            diff[c] <- resfull.cqt[c, 2, year.t == estyear.c[c]] - resval.cqt[c, 2, year.t == estyear.c[c]]
            reldiff[c] <- diff[c]/resfull.cqt[c, 2, year.t == estyear.c[c]]  
            # note: divide by resfull so that it is comparable
          }
          absdiff <- abs(diff)
          absreldiff <- abs(reldiff)
          diffs <- data.frame(diff, absdiff, reldiff, absreldiff)
          rownames(diffs) <- name.c
          diffs.summary <- apply(diffs, 2, summary)
          if (difftype == 1) { 
            cat(paste0("weight.alpha = ", weights.alpha.plusdefault[w], "; ", subset.countries, "\n"))
            print(diffs.summary)
            cat("\n")
            boxplot(diff, ylim = c(-50, 100), ylab = "Errors")
            abline(h = 0, lty = 2)
            if (w == 1) { # plot once!
              mtext(paste0("Errors ", ifelse(atLastObservationYear, 
                                                  "at last observation year", 
                                                  "in 2011.5")), outer = TRUE, cex = 1.2)
            }
          } else if(difftype == 2) {
            boxplot(reldiff, ylim = c(-1, 2), ylab = "Relative errors")
            abline(h = 0, lty = 2)
            mtext(paste0("Relative errors ", ifelse(atLastObservationYear, 
                                                         "at last observation year", 
                                                         "in 2011.5")), outer = TRUE, cex = 1.2)
          }
          mtext(paste0("weight.alpha = ", weights.alpha.plusdefault[w]), cex = 1)
        }
      }
      dev.off()
    }
  }
  cat("#---------- End of validation (up to 2000) results ----------#\n")
  sink()
  closeAllConnections()
  ##value<< \code{NULL}.
  return(invisible())
}
