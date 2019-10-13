#----------------------------------------------------------------------
# plotmoreresults.R
#----------------------------------------------------------------------
PlotMoreResults <- function(# Plot more results.
  ### Plot HIV-removed results on log scale and HIV-removed results with bias-adjusted data.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store trace plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  estimates.file = NULL, ##<< UN IGME estimates file (median estimates only). If \code{NULL}, not plotted. 
  weight.alpha.select = 0.5, ##<< Result type: Value of \code{weight.alpha} for Bayesian melding.
  year.start = NULL, ##<< Start year of estimates to plot. If \code{NULL}, earliest year of estimates available is used.
  year.end = NULL, ##<< End year of estimates to plot. If \code{NULL}, latest year of estimates available is used.
  plot.on.log.scale = TRUE, ##<< Plot country data and fits on log scale?
  plot.bias.adjusted.data = TRUE, ##<< Plot bias-adjusted data and fits?
  percentiles = c(0.05, 0.5, 0.95) ##<< Percentiles.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "mcmc.array.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  hiv.file <- mcmc.meta$files$hiv.file
  # if (is.null(hiv.file))
  #   hiv.file <- file.path("input", paste0("dataUNAIDS_", indicator.type, ".csv"))
  # if (is.null(estimates.file))
  #   estimates.file <- file.path("input", paste0(indicator.type, "_un.csv"))
  load(file.path(output.dir, "iso.c.rda"))
  load(file.path(output.dir, "year.t.rda"))
  #----------------------------------------------------------------------
  # get results
  if (is.validation) {
    Ytr.c <- GetLastObservationYearInValidationTrainingSet(mcmc.meta = mcmc.meta)
  } else {
    Ytr.c <- NULL
  }
  load(file = file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
  load(file = file.path(output.dir, "res.logscale.hivremoved.cqt.Lw.rda"))
  if (!is.null(year.start)) {
    if (year.start > min(year.t)) {
      res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]][, , year.t < year.start] <- NA
      res.logscale.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]][, , year.t < year.start] <- NA
    }
  }
  if (!is.null(year.end)) { 
    if (year.end < max(year.t)) {
      res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]][, , year.t > year.end] <- NA
      res.logscale.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]][, , year.t > year.end] <- NA
    }
  }  
  # get estimates for comparison
  if (!is.null(estimates.file)) {
    load(file = file.path(output.dir, "igme.hivremoved.rda"))
    igme.logscale.hivremoved <- GetIGME(country.codes = mcmc.meta$data$iso.c,
                                        is.hiv.country = mcmc.meta$data$hiv.c,  
                                        estimates.file = estimates.file,
                                        hiv.file = hiv.file,
                                        get.HIV.removed.data = TRUE,
                                        get.log.scale.data = TRUE)
    save(igme.logscale.hivremoved, file = file.path(output.dir, "igme.logscale.hivremoved.rda"))
    cat("Data and estimates loaded.\n")
  } else {
    igme.hivremoved <- igme.logscale.hivremoved <- NULL
  }
  #----------------------------------------------------------------------
  ##details<< Plot country data and fits on log scale using \code{\link{PlotDataAndEstimates}}.
  if (plot.on.log.scale) {
    if (!is.validation) {
      load(file = file.path(output.dir, "ypredict.hivremoved.ciq.rda"))
    } else {
      ypredict.hivremoved.ciq <- NULL
    }
    # plot
    pdf(file = file.path(fig.dir, paste(runname, "Results plot (on log scale, HIV-free).pdf")), 
        width = 21, height = 7)
    for (c in 1:mcmc.meta$data$C) {
      PlotDataAndEstimates(data = mcmc.meta$data.logscale.hivremoved,
                           c = c, 
                           est.years = year.t,
                           ylab = paste0("log(", indicator.type, ")"),
                           plot.se = TRUE, 
                           CIs.cqt = res.logscale.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]],
                           newobsPIs.ciq = ypredict.hivremoved.ciq,
                           Ytr.c = Ytr.c,
                           igme = igme.logscale.hivremoved)
    }
    dev.off()
  }
  cat(paste("Country results (HIV-removed if applicable) on log scale plotted.\n"))
  #----------------------------------------------------------------------
  ##details<< Get bias-adjusted data and plot country HIV-removed data and fits 
  ## the bias-adjusted data using \code{\link{GetBiasAdjustedData}} and \code{\link{PlotDataAndEstimates}}.
  if (plot.bias.adjusted.data) {
    if (run.type == "global") {
      if (!file.exists(file.path(output.dir, "data.hivremoved.biasadjusted.rda"))) {
        data.hivremoved.biasadjusted <- GetBiasAdjustedData(data.hivremoved = mcmc.meta$data.hivremoved, 
                                                            mcmc.array = mcmc.array,
                                                            percentiles = percentiles)
        save(data.hivremoved.biasadjusted, file = file.path(output.dir, "data.hivremoved.biasadjusted.rda"))
      } else {
        load(file = file.path(output.dir, "data.hivremoved.biasadjusted.rda"))
      }
      pdf(file = file.path(fig.dir, paste(runname, "Results plot (HIV-free, bias-adjusted).pdf")), 
          width = 21, height = 7)
      for (c in 1:mcmc.meta$data$C) {
        PlotDataAndEstimates(data = mcmc.meta$data.hivremoved,
                             c = c,
                             est.years = year.t,
                             ylab = indicator.type,
                             plot.se = TRUE,
                             CIs.cqt = res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]],
                             plot.biasadjobs = TRUE,
                             plot.b1adjobs = FALSE,
                             data.hivremoved.biasadjusted = data.hivremoved.biasadjusted,
                             igme = igme.hivremoved)
      }
      dev.off()
      cat("Country results (HIV-removed if applicable) with bias-adjusted data plotted.\n")
    }
  }
  ##value<< NULL
  return(invisible())
}
