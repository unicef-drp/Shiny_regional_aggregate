#----------------------------------------------------------------------
# plotresultsforimr.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
PlotResultsForIMR <- function(# Plot results for IMR
  ## Differs from \code{PlotResults} in that \code{mcmc.meta$data} is \code{NULL} and \code{mcmc.array}
  ## is not available.
  runname = "test", ##<< Run name.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  fig.dir = NULL, ##<< Directory to store plots. If \code{NULL}, defaults to folder \code{fig} in current working directory.
  estimates.file = NULL, ##<< UN IGME estimates file (median estimates only). If \code{NULL}, not plotted.
  weight.alpha.select = 0.5, ##<< Result type: Value of \code{weight.alpha} for Bayesian melding.
  year.start = NULL, ##<< Start year of estimates to plot. If \code{NULL}, earliest year of estimates available is used.
  year.end = NULL, ##<< End year of estimates to plot. If \code{NULL}, latest year of estimates available is used.
  percentiles = c(0.05,0.5,0.95), ##<< Percentiles.
  fig.dir.alt = NULL, ##<< Alternative directory for plots with different types plots in different folders.
  ## (Used for combined country-specific runs.)
  separate.plots.by.country = FALSE ##<< Separate plots produced by country?
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig")
  dir.create(fig.dir, showWarnings = FALSE)
  if (!is.null(fig.dir.alt)) {
    dir.create(fig.dir.alt, showWarnings = FALSE)
    dir.create(file.path(fig.dir.alt, "Results"), showWarnings = FALSE)
    dir.create(file.path(fig.dir.alt, "Results (HIV-removed)"), showWarnings = FALSE)
  }
  
  load(file.path(output.dir, "mcmc.meta.rda"))
  list2env(mcmc.meta$settings, envir = environment())
  hiv.file <- mcmc.meta$settings$hiv.file
  # if (is.null(hiv.file))
  #   hiv.file <- file.path("input", paste0("dataUNAIDS_", indicator.type, ".csv"))
  # if (is.null(estimates.file))
  #   estimates.file <- file.path("input", paste0(indicator.type, "_un.csv"))
  
  load(file.path(output.dir, "iso.c.rda"))
  load(file.path(output.dir, "year.t.rda"))
  year.t.temp <- year.t
  load(file.path(output.dir, "res.cqt.Lw.rda"))
  res.cqt <- res.cqt.Lw[[paste0(weight.alpha.select)]]
  if (!is.null(year.start)) {
    if (year.start > min(year.t)) {
      res.cqt[, , year.t < year.start] <- NA
    }
  }
  if (!is.null(year.end)) {
    if (year.end < max(year.t)) {
      res.cqt[, , year.t > year.end] <- NA
    }
  }
  
  if (!is.null(estimates.file)) {
    igme <- GetIGME(country.codes = mcmc.meta$data.all$iso.c,
                    estimates.file = estimates.file,
                    hiv.file = hiv.file)
    save(igme, file = file.path(output.dir, "igme.rda"))
    cat("IGME estimates read in.\n")
  } else {
    igme <- NULL
  }
  
  if (!separate.plots.by.country) {
    if (is.null(fig.dir.alt)) {
      pdf(file = file.path(fig.dir, paste(runname, "Results.pdf")), width = 21, height = 7)
    } else {
      pdf(file = file.path(fig.dir.alt, "Results", paste(runname, "Results.pdf")), width = 21, height = 7)
    }
  }
  for (c in 1:mcmc.meta$data.all$C) {
    if (separate.plots.by.country)
      pdf(file = file.path(fig.dir, paste0(runname, " Results_", mcmc.meta$data.all$iso.c[c], ".pdf")), width = 21, height = 7)
    PlotDataAndEstimates(data = mcmc.meta$data,
                         data.all = mcmc.meta$data.all,
                         c = c,
                         est.years = year.t,
                         ylab = indicator.type,
                         plot.se = TRUE,
                         legendfull = mcmc.meta$data.all$imrmethod.c[c],
                         CIs.cqt = res.cqt, 
                         igme = igme)
   if (separate.plots.by.country)
      dev.off()
  }
  if (!separate.plots.by.country)
    dev.off()
  cat("Country results plotted.\n")
  #----------------------------------------------------------------------
  ##details<< Plot country HIV-removed data and fits using \code{\link{PlotDataAndEstimates}}.
  # get results
  load(file = file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
  if (!is.null(estimates.file)) {
    igme.hivremoved <- GetIGME(country.codes = mcmc.meta$data.all$iso.c,
                               is.hiv.country = mcmc.meta$data.all$hiv.c,  
                               estimates.file = estimates.file,
                               hiv.file = hiv.file,
                               get.HIV.removed.data = TRUE)
    save(igme.hivremoved, file = file.path(output.dir, "igme.hivremoved.rda"))
  } else {
    igme.hivremoved <- NULL
  }
  if (sum(mcmc.meta$data.all$hiv.c) > 0) { # for HIV countries only
    if (is.null(fig.dir.alt)) {
      pdf(file = file.path(fig.dir, paste(runname, "Results plot (HIV-removed).pdf")), width = 21, height = 7)
    } else {
      pdf(file = file.path(fig.dir.alt, "Results (HIV-removed)", paste(runname, "Results plot (HIV-removed).pdf")), width = 21, height = 7)
    }
    for (c in (1:mcmc.meta$data.all$C)[mcmc.meta$data.all$hiv.c]){
      PlotDataAndEstimates(data = mcmc.meta$data.hivremoved,
                           data.all = mcmc.meta$data.hivremoved.all,
                           c = c,
                           est.years = year.t,
                           ylab = indicator.type,
                           plot.se = TRUE,
                           legendfull = mcmc.meta$data.all$imrmethod.c[c],
                           CIs.cqt = res.hivremoved.cqt.Lw[[paste0(weight.alpha.select)]],
                           igme = igme.hivremoved)
    }
    dev.off()
    cat("HIV-removed country results plotted.\n")
  }
}
