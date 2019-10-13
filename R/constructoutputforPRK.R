#----------------------------------------------------------------------
# constructoutputforPRK.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
DoExpertUIs <- function(
  u5.est.t,
  nsim,
  mu.d = 0,
  sd.d = 0.15
) {
  D <- exp(rnorm(nsim, mu.d, sd.d))
  u5.tj <- t(sapply(u5.est.t, "*", D))
  return(u5.tj)
}
#----------------------------------------------------------------------
ConstructOutputForPRK <- function( # Construct output for Korea DPR
  ## Construct output for Korea DPR: Country trajectories and estimates.
  runname = "test", ##<< Run name.
  runname.IMR = NULL, ##<< Run name if IMR output is desired.
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output are stored and new objects will be added.
  ## If \code{NULL}, defaults to \code{output/runname}, which is the default from \code{RunMCMC}).
  est.PRK.file = "input/MedianEstimate_PRK.csv", ##<< File path to estimates for Korea DPR.
  runname.global, ##<< Run name of global run to get settings.
  nsim = 8000, ##<< Number of trajectories to sample (should be the same as number for trajectories for other countries).
  year.start = NULL, ##<< Start year of estimates to output in .txt file. If \code{NULL}, defaults to first observation year for each country.
  year.end = NULL, ##<< End year of estimates to output in .txt file. If \code{NULL}, defaults to \code{mcmc.meta$settings$year.lastestimate}.
  year1 = 1990.5, ##<< First year used for ARR calculation.
  year2 = 2000.5, ##<< Second year used for ARR calculation.
  year3 = 2005.5, ##<< Third year used for ARR calculation, usually last year of estimation for validation exercise.
  year4 = NULL, ##<< Last year used for ARR calculation. If \code{NULL}, defaults to \code{mcmc.meta$settings$year.lastestimatepublished} or \code{NULL} if validation run and
  ## if \code{mcmc.meta$settings$year.lastestimatepublished} > \code{mcmc.meta$settings$year.cutoff}.
  percentiles = c(0.05, 0.5, 0.95), ##<< Percentiles for 90% UIs.
  weights.alpha = seq(0.1, 0.6, 0.1), ##<< Vector of weights assigned to global gamma distribution.
  weight.alpha.publish = 0.5, ##<< \code{weight.alpha} to use for Results.csv.
  quick.plot.check = TRUE ##<< Logical value to indicate if a quick plot should be made for checking.
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", runname, "/")
  
  # get mcmc.meta
  if (!file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat("Starting RunMCMC to get mcmc.meta for all data.\n")
    # first load some contry-specific run's mcmc.meta to get data.cmeinfo.file
    #load(file.path("output", paste0(runname.global, "_AFG"), "mcmc.meta.rda"))
    #data.cmeinfo.file <- mcmc.meta$files$data.cmeinfo.file
    #rm(mcmc.meta)
    # first load global run's mcmc.meta to get settings
    load(file = file.path("output", paste0(runname.global,"_all"),"mcmc.meta.rda"))
    RunMCMC(runname = runname,
            iso.select = "PRK",
            run.jags = FALSE,
            indicator.type = mcmc.meta$settings$indicator.type,
            run.type = "country",
            runname.global = runname.global,
            run.for.IMR.MLT = ifelse(mcmc.meta$settings$indicator.type == "IMR", TRUE, FALSE),
            output.dir = output.dir,
            data.cmeinfo.file = data.cmeinfo.file,
            country.info.file = mcmc.meta$files$country.info.file,
            country.B3info.file = mcmc.meta$files$country.B3info.file, 
            hiv.file = mcmc.meta$files$hiv.file, 
            adj.file = mcmc.meta$files$adj.file,
            livebirths.file = mcmc.meta$files$livebirths.file,
            year.current = mcmc.meta$settings$year.current,
            year.lastestimatepublished = mcmc.meta$settings$year.lastestimatepublished,
            year.lastestimate = mcmc.meta$settings$year.lastestimate,
            year.cutoff = mcmc.meta$settings$year.cutoff,
            runname.U5MR = mcmc.meta$settings$runname.U5MR,
            runname.igme = mcmc.meta$settings$runname.igme,
            se.censusindirect.missing = mcmc.meta$settings$se.censusindirect.missing, 
            se.othernonvr.missing = mcmc.meta$settings$se.othernonvr.missing,
            se.vr.min = mcmc.meta$settings$se.vr.min,
            se.vr.missing = mcmc.meta$settings$se.vr.missing,
            recall.mid = mcmc.meta$settings$recall.mid,
            dhsdirect.prior.mu.mubeta1 = mcmc.meta$settings$dhsdirect.prior.mu.mubeta1,
            dhsdirect.prior.sigma.mubeta1 = mcmc.meta$settings$dhsdirect.prior.sigma.mubeta1,
            is.validation = mcmc.meta$settings$is.validation,
            fit.B2.model = mcmc.meta$settings$fit.B2.model,
            input.vr.se = mcmc.meta$settings$input.vr.se,
            include.HIV.countries = mcmc.meta$settings$include.HIV.countries,
            add.dhsdirect.bias = mcmc.meta$settings$add.dhsdirect.bias,
            set.dhsdirect.prior = mcmc.meta$settings$set.dhsdirect.prior,
            use.constant.sigma.u = mcmc.meta$settings$use.constant.sigma.u,
            use.country.variance.multipliers = mcmc.meta$settings$use.country.variance.multipliers,
            I = mcmc.meta$settings$I)
    closeAllConnections()
    rm(mcmc.meta)
    cat("mcmc.meta for all data saved.\n")
  } 
  load(file.path(output.dir, "mcmc.meta.rda"))
  cat("mcmc.meta for all data loaded.\n")
  
  if (is.null(year.end))
    year.end <- mcmc.meta$settings$year.lastestimate
  if (is.null(year4)) {
    if (mcmc.meta$settings$is.validation & 
          mcmc.meta$settings$year.lastestimatepublished > mcmc.meta$settings$year.cutoff) {
      year4 <- NULL # validation
    } else {
      year4 <- mcmc.meta$settings$year.lastestimatepublished
    }
  }
  
  year.i <- c(unlist(mcmc.meta$data$year.Lcs.j[[1]]), unlist(mcmc.meta$data$yearvr.Lc.j[[1]]))
  
  cat(paste("Constructing output for Korea DPR.\n"))
  set.seed(mcmc.meta$general$seed.MCMC*mcmc.meta$data.all$uncode.c[1]) # change JR, 20140508
  est.PRK <- read.csv(est.PRK.file, header = T, stringsAsFactors = F)
  year.t <- est.PRK$Year[est.PRK$Indicator == "Under-five Mortality Rate" &
                              est.PRK$Subgroup == "Total"]
  nyears <- length(year.t)
  u5.est.t <- est.PRK$Estimate[est.PRK$Indicator == "Under-five Mortality Rate" &
                                 est.PRK$Subgroup == "Total"]  
  ##details<< Outputs lists of results for weights w (and year combinations y for ARR).
  nweights <- length(weights.alpha)
  weights.alpha.plusdefault <- c(0, weights.alpha)
  nweightsplus1 <- length(weights.alpha.plusdefault)
  res.cqt.Lw <- resall.cqt.Lw <- resmean.ct.Lw <- resARR.cq.Lwy <- list()
  for (w in 1:nweightsplus1) {
    res.cqt.Lw[[w]] <- resall.cqt.Lw[[w]] <- array(NA, c(1, length(percentiles), nyears))
    resmean.ct.Lw[[w]] <- array(NA, c(1, nyears))
    dimnames(res.cqt.Lw[[w]]) <- dimnames(resall.cqt.Lw[[w]]) <- 
      list("PRK", percentiles, year.t)
    dimnames(resmean.ct.Lw[[w]]) <- list("PRK", year.t)
    resARR.cq.Lwy[[w]] <- list()
    for (y in 1:4) {
      resARR.cq.Lwy[[w]][[y]] <- array(NA, c(1, length(percentiles)))
      dimnames(resARR.cq.Lwy[[w]][[y]]) <- list("PRK", percentiles)
    }
    names(resARR.cq.Lwy[[w]]) <- c(paste0(year1, "-", year3), paste0(year1, "-", year2), 
                                   paste0(year2, "-", year4), paste0(year1, "-", year4))
  }
  names(res.cqt.Lw) <- names(resall.cqt.Lw) <- names(resmean.ct.Lw) <- names(resARR.cq.Lwy) <- 
    weights.alpha.plusdefault
  
  u5.ctj <- array(NA, c(1, nyears, nsim))
  u5.ctj[1, , ] <- DoExpertUIs(u5.est.t = u5.est.t, nsim = nsim)
  res <- CalculateQuantities(u5temp.tj = u5.ctj[1, , ],
                             iso = "PRK",
                             indicator.type = "U5MR", 
                             runname.U5MR = runname.U5MR,
                             hiv = FALSE, hiv.file = mcmc.meta$files$hiv.file, # change JR, 20140508
                             crisisadj = FALSE, adj.file = mcmc.meta$files$adj.file, # change JR, 20140508
                             year.t = year.t, year.i = year.t,
                             estyear.min = 1990.5,
                             year1 = year1, year2 = year2, year3 = year3, year4 = year4,
                             percentiles = percentiles)
  u5.ctj[1, , ] <- res$u5.tj
  save(res, file = "res.rda")
  for (w in 1:nweightsplus1) {
    eval(parse(text = paste0("u5new", w-1, ".ctj", " <- u5.ctj")))
    resall.cqt.Lw[[w]][1, , ] <- res$resall.qt
    res.cqt.Lw[[w]][1, , ] <- res$res.qt
    resmean.ct.Lw[[w]][1, ] <- res$resmean.t
    for (y in 1:4) {
      resARR.cq.Lwy[[w]][[y]][1, ] <- unlist(res$resARR.q.Ly[[y]])
    }
  }
  if (quick.plot.check) {
    u.i <- c(unlist(mcmc.meta$data$u.Lcs.j[[1]]), unlist(mcmc.meta$data$uvr.Lc.j[[1]]))
    plot(u.i ~ year.i, main = mcmc.meta$data$name.c[1], 
         ylim = c(0, min(max(res.cqt.Lw[[paste0(weights.alpha[1])]][1, , ], u.i, na.rm = T))), 
         xlim = c(min(year.i, na.rm = T), max(year.i, year.t, na.rm = T)))
    for (q in 1:length(percentiles))
      lines(res.cqt.Lw[[paste0(weight.alpha.publish)]][1, q, ] ~ year.t)
  } # end quick.plot.check
  # output results
  # change JR, 20150602: optimised code by replacing with OutputResultsWide function
  OutputResultsWide(res.cqt = res.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = "U5MR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results")
  # save all results
  iso.c <- mcmc.meta$data$iso.c
  name.c <- mcmc.meta$data$name.c
  save(iso.c, file = file.path(output.dir, "iso.c.rda"))
  save(name.c, file = file.path(output.dir, "name.c.rda"))
  save(year.t, file = file.path(output.dir, "year.t.rda"))
  save(u5.ctj, file = file.path(output.dir, "u5.ctj.rda"))
  save(res.cqt.Lw, file = file.path(output.dir, "res.cqt.Lw.rda"))
  save(resall.cqt.Lw, file = file.path(output.dir, "resall.cqt.Lw.rda"))
  save(resmean.ct.Lw, file = file.path(output.dir, "resmean.ct.Lw.rda"))
  save(resARR.cq.Lwy, file = file.path(output.dir, "resARR.cq.Lwy.rda"))
  for (w in 1:nweights) {
    eval(parse(text = paste0("save(u5new", w, ".ctj, file = file.path(output.dir, \"u5new", 
                             w, ".ctj.rda", "\"))")))
  }
  res.hivremoved.cqt.Lw <- res.crisisremoved.cqt.Lw <- res.crisisandhivremoved.cqt.Lw <- 
    res.logscale.hivremoved.cqt.Lw <- res.cqt.Lw
  for (w in 1:nweightsplus1) {
    res.logscale.hivremoved.cqt.Lw[[paste0(weights.alpha.plusdefault[w])]] <- 
      log(res.hivremoved.cqt.Lw[[paste0(weights.alpha.plusdefault[w])]])
  }
  save(res.hivremoved.cqt.Lw, file = file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
  save(res.crisisremoved.cqt.Lw, file = file.path(output.dir, "res.crisisremoved.cqt.Lw.rda"))
  save(res.crisisandhivremoved.cqt.Lw, file = file.path(output.dir, "res.crisisandhivremoved.cqt.Lw.rda"))
  save(res.logscale.hivremoved.cqt.Lw, file = file.path(output.dir, "res.logscale.hivremoved.cqt.Lw.rda"))
  OutputResultsWide(res.cqt = res.hivremoved.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = "U5MR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (HIV-free)")
  OutputResultsWide(res.cqt = res.crisisremoved.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = "U5MR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (crisis-free)")
  OutputResultsWide(res.cqt = res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha.publish)]],
                    name.c = mcmc.meta$data$name.c,
                    iso.c = mcmc.meta$data$iso.c,
                    year.t = year.t,
                    indicator.type = "U5MR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (crisis-and-HIV-free)")
  #----------------------------------------------------------------------
  # output year.t and res.cqt.Lw.rda for IMR
  if (!is.null(runname.IMR)) {
    dir.create(file.path(getwd(), "output", runname.IMR), showWarnings = F)
    u1.est.t <- est.PRK$Estimate[est.PRK$Indicator == "Infant Mortality Rate" &
                                   est.PRK$Subgroup == "Total"]
    for (w in 1:nweightsplus1) {
      res.cqt.Lw[[w]][] <- NA
      res.cqt.Lw[[w]][1, 2, ] <- u1.est.t
    }
    save(year.t, file = file.path("output", runname.IMR, "year.t.rda"))
    save(res.cqt.Lw, file = file.path("output", runname.IMR, "res.cqt.Lw.rda"))
  }
  ##value<< \code{NULL}; Saves all results to \code{output.dir}. 
  #return(invisible())
  return(results)
}
