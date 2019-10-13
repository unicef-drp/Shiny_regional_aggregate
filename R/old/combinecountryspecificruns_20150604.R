#----------------------------------------------------------------------
# combinecountryspecificruns.R
#----------------------------------------------------------------------
CombineCountrySpecificRuns <- function(# Combine results of country-specific runs for all/some countries and
  ## generate mcmc.meta for combined data of the country-specific runs.
  runname.prefix.countryspecific, ##<< Character specifying runname prefix of country-specific runs.
  runname.global = NULL, ##<< Character specifying runname.global of country-specific runs.
  indicator.type, ##<< Indicator type.
  iso.all = NULL, ##<< ISO country codes of country-specific runs to combine. If \code{NULL}, runs
  ## are combined for all 194 countries.
  data.cmeinfo.file, ##<< File path to complete database used for country-specific runs.
  get.combinedresults = TRUE, ##<< Get combined results? 
  get.trajectories = TRUE, ##<< Get combined trajectories?
  get.PPD = TRUE, ##<< Get posterior predictive distribution and related quantities?
  output.dir.countryspecific = NULL, ##<< Directory where country output folders are located. If \code{NULL},
  ## defaults to \code{output}.
  output.dir = NULL ##<< Directory to saved combined results to. If \code{NULL}, defaults to
  ## \code{output/runname.global_all}. 
) {
  if (is.null(iso.all)) {
    load(file.path("input", "iso.c.rda"))
    iso.all <- iso.c
  }
  if (is.null(output.dir.countryspecific))
    output.dir.countryspecific <- file.path(getwd(), "output")
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", paste0(runname.prefix.countryspecific, "_all/"))
  dir.create(output.dir, showWarnings = F)
  
  # get mcmc.meta if it does not yet exist
  if (!file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat("Starting RunMCMC to get mcmc.meta for all data.\n")
    # first load some country-specific run's mcmc.meta to get settings
    load(file.path(output.dir.countryspecific, 
		     paste0(runname.prefix.countryspecific, "_AUS"), "mcmc.meta.rda"))
    mcmc.meta.cc <- mcmc.meta
    rm(mcmc.meta)
    # then load global run's mcmc.meta to get settings if runname.global is not NULL
    if (is.null(runname.global)) {
      mcmc.meta.global <- mcmc.meta.cc
    } else {
      load(file = file.path("output", runname.global, "mcmc.meta.rda"))
      mcmc.meta.global <- mcmc.meta
      rm(mcmc.meta)
    }
    RunMCMC(runname = paste0(runname.prefix.countryspecific, "_all/"),
            iso.select = iso.all,
            run.jags = FALSE,
            indicator.type = mcmc.meta.global$settings$indicator.type,
            run.type = "combined",
            runname.global = mcmc.meta.cc$settings$runname.global,
            run.for.IMR.MLT = ifelse(mcmc.meta.global$settings$indicator.type == "IMR", TRUE, FALSE),
            output.dir = output.dir,
            data.cmeinfo.file = data.cmeinfo.file,
            country.info.file = mcmc.meta.global$files$country.info.file,
            country.B3info.file = mcmc.meta.global$files$country.B3info.file, 
            hiv.file = mcmc.meta.global$files$hiv.file, 
            adj.file = mcmc.meta.global$files$adj.file,
            livebirths.file = mcmc.meta.global$files$livebirths.file,
            year.current = mcmc.meta.global$settings$year.current,
            year.lastestimatepublished = mcmc.meta.global$settings$year.lastestimatepublished,
            year.lastestimate = mcmc.meta.global$settings$year.lastestimate,
            year.cutoff = mcmc.meta.global$settings$year.cutoff,
            runname.U5MR = mcmc.meta.global$settings$runname.U5MR,
            runname.igme = mcmc.meta.global$settings$runname.igme,
            se.censusindirect.missing = mcmc.meta.global$settings$se.censusindirect.missing, 
            se.othernonvr.missing = mcmc.meta.global$settings$se.othernonvr.missing,
            se.vr.min = mcmc.meta.global$settings$se.vr.min,
            se.vr.missing = mcmc.meta.global$settings$se.vr.missing,
            recall.mid = mcmc.meta.global$settings$recall.mid,
            dhsdirect.prior.mu.mubeta1 = mcmc.meta.global$settings$dhsdirect.prior.mu.mubeta1,
            dhsdirect.prior.sigma.mubeta1 = mcmc.meta.global$settings$dhsdirect.prior.sigma.mubeta1,
            is.validation = mcmc.meta.global$settings$is.validation,
            fit.B2.model = mcmc.meta.global$settings$fit.B2.model,
            input.vr.se = mcmc.meta.global$settings$input.vr.se,
            include.HIV.countries = mcmc.meta.global$settings$include.HIV.countries,
            add.dhsdirect.bias = mcmc.meta.global$settings$add.dhsdirect.bias,
            set.dhsdirect.prior = mcmc.meta.global$settings$set.dhsdirect.prior,
            use.constant.sigma.u = mcmc.meta.global$settings$use.constant.sigma.u,
            use.country.variance.multipliers = mcmc.meta.global$settings$use.country.variance.multipliers,
            I = mcmc.meta.global$settings$I,
            global.gamma.median = mcmc.meta.cc$settings$global.gamma.median,
            global.gamma.sd = mcmc.meta.cc$settings$global.gamma.sd)
    closeAllConnections()
    cat("mcmc.meta for all data saved.\n")
  } 
  load(file.path(output.dir, "mcmc.meta.rda"))
  cat("mcmc.meta for all data loaded.\n")
  
  if (indicator.type == "U5MR") {
    iso.all <- mcmc.meta$data$iso.c
  } else {
    iso.all <- mcmc.meta$data.all$iso.c
  }
  
  if (get.combinedresults) {
    # combine output of country-specific runs
    C <- length(iso.all)
    minyear.c <- maxyear.c <- n.c <- rep(NA, C)
    # check that output for all selected countries are available
    exclude <- NULL
    for (c in 1:C) {
      if (!file.exists(file.path(output.dir.countryspecific,
                                 paste0(runname.prefix.countryspecific, "_", iso.all[c]),
                                 "res.cqt.Lw.rda")))
        exclude <- c(exclude, c)
    }
    
    if (!is.null(exclude)) {
      cat(paste0("Warning: No output available for the countries with codes: ", paste(iso.all[exclude], collapse = ", "), "\n"))
      iso.all <- iso.all[-exclude]
      C <- length(iso.all)
      cat("Proceeding to combine output for all other selected countries.\n")
    }
    exclude.for.PPD <- NULL
    for (c in 1:C) {
      load(file = file.path(output.dir.countryspecific, 
                            paste0(runname.prefix.countryspecific, "_", iso.all[c]),
                            "year.t.rda"))
      minyear.c[c] <- min(year.t, na.rm = T)
      maxyear.c[c] <- max(year.t, na.rm = T)
      if (indicator.type == "U5MR" & get.PPD) {
        file.y.ci <- file.path(output.dir.countryspecific,
                              paste0(runname.prefix.countryspecific, "_", iso.all[c]),
                              "y.ci.rda")
        if (file.exists(file.y.ci)) {
          load(file.y.ci)
          n.c[c] <- dim(y.ci)[2]
        } else {
          exclude.for.PPD <- c(exclude.for.PPD, c)
        }
      }
    }
    if (indicator.type == "U5MR" & get.PPD) {
      if (length(exclude.for.PPD) > 0)
        cat(paste0("Warning: No residuals available for the countries with codes: ", paste(iso.all[exclude.for.PPD], collapse = ", "), "\n"))
    }
    minyear <- min(minyear.c, na.rm = T)
    maxyear <- max(maxyear.c, na.rm = T)
    if (indicator.type == "U5MR" & get.PPD)
      nmax <- max(n.c, na.rm = T)
    year.all <- minyear:maxyear
    nyears <- length(year.all)
    # load one result to get nsim, Q and weights.alpha
    if (get.trajectories) {
      load(file = file.path(output.dir.countryspecific,
                            paste0(runname.prefix.countryspecific, "_", iso.all[1]),
                            "u5.ctj.rda"))
      nsim <- dim(u5.ctj)[3]; rm(u5.ctj)
    }
    if (get.PPD) {
      load(file = file.path(output.dir.countryspecific,
                            paste0(runname.prefix.countryspecific, "_", iso.all[1]),
                            "ypredict.cij.rda"))
      nsim <- dim(ypredict.cij)[3]; rm(ypredict.cij)
    }
    load(file = file.path(output.dir.countryspecific, 
                          paste0(runname.prefix.countryspecific, "_", iso.all[1]),
                          "resARR.cq.Lwy.rda"))
    # if (indicator.type == "U5MR") { ### adhoc change JR, 20150518
      percentiles <- dimnames(resARR.cq.Lwy[[1]][[1]])[[2]]
    # } else {
      percentiles <- as.character(c(0.05, 0.5, 0.95))
    # }
    Q <- length(percentiles)
    weights.alpha.plusdefault <- as.numeric(names(resARR.cq.Lwy))
    nweightsplus1 <- length(weights.alpha.plusdefault)
    weights.alpha <- weights.alpha.plusdefault[-1]
    nweights <- length(weights.alpha)
    ARR.years <- names(resARR.cq.Lwy[[1]])
    
    # declare variables
    if (get.trajectories) {
      u5.ctj.all <- array(NA, c(C, nyears, nsim))
      dimnames(u5.ctj.all) <- list(iso.all, year.all, NULL)
    }
    if (mcmc.meta$settings$is.validation) {
      u5full.ctj.all <- array(NA, c(C, nyears, nsim))
      dimnames(u5full.ctj.all) <- list(iso.all, year.all, NULL)
    }
    res.cqt.Lw.all <- resall.cqt.Lw.all <- resmean.ct.Lw.all <- resARR.cq.Lwy.all <- list()
    if (indicator.type == "U5MR" & get.PPD) {
      ypredict.cij.all <- array(NA, c(C, nmax, nsim))
      ypredict.hivremoved.ciq.all <- array(NA, c(C, nmax, Q)) 
      y.ci.all <- bias.ci.all <- stbias.ci.all <- q.ci.all <- matrix(NA, C, nmax)
    }
    mcmc.array.all <- resproject.list.c.all <- NULL
    for (w in 1:nweightsplus1) {
      if (get.trajectories) {
        eval(parse(text = paste0("u5new", w-1, ".ctj.all", " <- array(NA, c(C, nyears, nsim))")))
        eval(parse(text = paste0("dimnames(u5new", w-1, ".ctj.all)", " <- list(iso.all, year.all, NULL)")))
      }
      res.cqt.Lw.all[[w]] <- resall.cqt.Lw.all[[w]] <-
        array(NA, c(C, Q, nyears))
      dimnames(res.cqt.Lw.all[[w]]) <- dimnames(resall.cqt.Lw.all[[w]]) <- 
        list(iso.all, percentiles, year.all)
      resmean.ct.Lw.all[[w]] <- array(NA, c(C, nyears))
      dimnames(resmean.ct.Lw.all[[w]]) <- list(iso.all, year.all)
      resARR.cq.Lwy.all[[w]] <- list()
      for (y in 1:length(ARR.years)) {
        resARR.cq.Lwy.all[[w]][[y]] <- array(NA, c(C, Q))
        dimnames(resARR.cq.Lwy.all[[w]][[y]]) <- list(iso.all, percentiles)
      }
      names(resARR.cq.Lwy.all[[w]]) <- ARR.years
    }
    names(res.cqt.Lw.all) <- names(resall.cqt.Lw.all) <- names(resmean.ct.Lw.all) <- names(resARR.cq.Lwy.all) <- 
      weights.alpha.plusdefault
    res.hivremoved.cqt.Lw <- res.crisisremoved.cqt.Lw <- 
      res.crisisandhivremoved.cqt.Lw <- res.logscale.hivremoved.cqt.Lw <- res.cqt.Lw
    
    # load and combine country-specific results
    files <- c("year.t", "res.cqt.Lw", "resall.cqt.Lw", "resmean.ct.Lw", "resARR.cq.Lwy",
               "res.hivremoved.cqt.Lw", "res.logscale.hivremoved.cqt.Lw")
    if (get.trajectories)
      files <- c(files, "u5.ctj", paste0("u5new", 1:nweights, ".ctj"))
    if (mcmc.meta$settings$is.validation)
      files <- c(files, "u5full.ctj")
    if (indicator.type == "U5MR") {
      files <- c(files, "mcmc.array", "resproject.list.c")
      if (get.PPD) {
        files.PPD <- c("ypredict.cij", "ypredict.hivremoved.ciq", "y.ci", "bias.ci", "stbias.ci", "q.ci")
        files.PPD.to.load <- paste0(files.PPD, ".rda")
      }
    }
    files.to.load <- paste0(files, ".rda")
    for (c in 1:C) {
      cat(paste0("Combining country-specific output for country ", c, " (", iso.all[c], ") of ", C, " countries.\n"))
      sapply(files.to.load, LoadFile, 
             output.dir = file.path(output.dir.countryspecific, 
                                    paste0(runname.prefix.countryspecific, "_", iso.all[c])), 
             envir = environment())
      if (indicator.type == "U5MR" & get.PPD & !(c %in% exclude.for.PPD))
        sapply(files.PPD.to.load, LoadFile, 
               output.dir = file.path(output.dir.countryspecific, 
                                      paste0(runname.prefix.countryspecific, "_", iso.all[c])), 
               envir = environment())
      if (get.trajectories)
        u5.ctj.all[c, is.element(year.all, year.t), ] <- u5.ctj[1, , ]
      if (mcmc.meta$settings$is.validation)
        u5full.ctj.all[c, is.element(year.all, year.t), ] <- u5full.ctj[1, , ]
      for (w in 1:nweightsplus1) {
        if (get.trajectories & w > 1)
          eval(parse(text = paste0("u5new", w-1, ".ctj.all[c, is.element(year.all, year.t), ]", 
                                   " <- u5new", w-1, ".ctj[1, , ]"))) 
        res.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- res.cqt.Lw[[w]][1, , ]
        resall.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- resall.cqt.Lw[[w]][1, , ]
        res.hivremoved.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- res.hivremoved.cqt.Lw[[w]][1, , ]
        res.logscale.hivremoved.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- 
          res.logscale.hivremoved.cqt.Lw[[w]][1, , ]
        resmean.ct.Lw.all[[w]][c, is.element(year.all, year.t)] <- unlist(resmean.ct.Lw[[w]][1, ])
        for (y in 1:length(ARR.years)) {
          resARR.cq.Lwy.all[[w]][[y]][c, ] <- unlist(resARR.cq.Lwy[[w]][[y]][1, ])
        }
      }
      if (indicator.type == "U5MR") {
        # combine mcmc.array
        mcmc.array.names <- dimnames(mcmc.array)[[3]]
        # change names of country-specific parameters
        mcmc.array.names <- gsub("\\[1", paste0("[", c), mcmc.array.names)
        # change names of parameter a
        mcmc.array.names[mcmc.array.names == "a"] <- paste0("a.c[", c, "]")
        # inpute back mcmc.array names
        dimnames(mcmc.array)[[3]] <- mcmc.array.names
        # standardise dimensions of mcmc.array
        if (is.null(mcmc.array.all)) {
          mcmc.array.add <- mcmc.array
        } else {
          if (all(dim(mcmc.array)[1:2] == dim(mcmc.array.all)[1:2])) {
            mcmc.array.add <- mcmc.array
          } else {
            mcmc.array.add <- array(NA, c(dim(mcmc.array.all)[1:2], dim(mcmc.array)[3]))
            dimnames(mcmc.array.add)[[3]] <- mcmc.array.names
            for (par in mcmc.array.names)
              mcmc.array.add[, , par] <- matrix(c(mcmc.array[, , par]), 
                                                dim(mcmc.array.add)[1], dim(mcmc.array.add)[2])
          }
        }
        # drop deviance parameter
        mcmc.array.all <- abind(mcmc.array.all, mcmc.array.add[, , !is.element(mcmc.array.names, "deviance")])
        # combine resproject.list.c # change JR, 20140423
        if (file.exists(file.path(output.dir.countryspecific, 
                                  paste0(runname.prefix.countryspecific, "_", iso.all[c]), "resproject.list.c.rda"))) {
          # standardise nrow of B.tk in resproject.list.c
          B.tk.all <- matrix(NA, length(year.all), ncol(resproject.list.c[[1]]$B.tk))
          B.tk.all[is.element(year.all, year.t), ] <- resproject.list.c[[1]]$B.tk
          resproject.list.c[[1]]$B.tk <- B.tk.all
          resproject.list.c.all <- c(resproject.list.c.all, resproject.list.c)
        } else {
          resproject.list.c.all <- c(resproject.list.c.all, list(NULL))
        }
        if (get.PPD & !(c %in% exclude.for.PPD)) {
          ypredict.cij.all[c, 1:n.c[c], ] <- ypredict.cij[1, , ]
          ypredict.hivremoved.ciq.all[c, 1:n.c[c], ] <- ypredict.hivremoved.ciq[1, , ]
          y.ci.all[c, 1:n.c[c]] <- y.ci[1, ]
          bias.ci.all[c, 1:n.c[c]] <- bias.ci[1, ]
          stbias.ci.all[c, 1:n.c[c]] <- stbias.ci[1, ]
          q.ci.all[c, 1:n.c[c]] <- q.ci[1, ]
        }
      }
    } # end country loop  
    # rename all combined files
    if (indicator.type == "U5MR") {
      name.c <- mcmc.meta$data$name.c
    } else {
      name.c <- mcmc.meta$data.all$name.c
    }
    iso.c <- iso.all
    year.t <- year.all
    res.cqt.Lw <- res.cqt.Lw.all
    resall.cqt.Lw <- resall.cqt.Lw.all
    resmean.ct.Lw <- resmean.ct.Lw.all
    resARR.cq.Lwy <- resARR.cq.Lwy.all
    res.hivremoved.cqt.Lw <- res.hivremoved.cqt.Lw.all
    res.logscale.hivremoved.cqt.Lw <- res.logscale.hivremoved.cqt.Lw.all
    if (get.trajectories) { # change JR, 1 Jul
      u5.ctj <- u5.ctj.all
      if (mcmc.meta$settings$is.validation)
        u5full.ctj <- u5full.ctj.all
      for (w in 1:nweights) { 
        eval(parse(text = paste0("u5new", w, ".ctj", " <- u5new", w, ".ctj.all")))
      }
    }
    if (indicator.type == "U5MR") {
      mcmc.array <- mcmc.array.all
      resproject.list.c <- resproject.list.c.all
      if (get.PPD) {
        ypredict.cij <- ypredict.cij.all
        ypredict.hivremoved.ciq <- ypredict.hivremoved.ciq.all
        y.ci <- y.ci.all
        bias.ci <- bias.ci.all
        stbias.ci <- stbias.ci.all
        q.ci <- q.ci.all
      }
    }
    # save all combined files
    save(name.c, file = file.path(output.dir, "name.c.rda"))
    save(iso.c, file = file.path(output.dir, "iso.c.rda"))
    save(year.t, file = file.path(output.dir, "year.t.rda"))
    save(res.cqt.Lw, file = file.path(output.dir, "res.cqt.Lw.rda"))
    save(resall.cqt.Lw, file = file.path(output.dir, "resall.cqt.Lw.rda"))
    save(resmean.ct.Lw, file = file.path(output.dir, "resmean.ct.Lw.rda"))
    save(resARR.cq.Lwy, file = file.path(output.dir, "resARR.cq.Lwy.rda"))
    save(res.hivremoved.cqt.Lw, file = file.path(output.dir, "res.hivremoved.cqt.Lw.rda"))
    save(res.logscale.hivremoved.cqt.Lw, 
         file = file.path(output.dir, "res.logscale.hivremoved.cqt.Lw.rda"))
    if (get.trajectories) {
      save(u5.ctj, file = file.path(output.dir, "u5.ctj.rda"))
      if (mcmc.meta$settings$is.validation)
        save(u5full.ctj, file = file.path(output.dir, "u5full.ctj.rda"))
      for (w in 1:nweights) {
        eval(parse(text = paste0("save(u5new", w, ".ctj, file = file.path(output.dir, \"u5new", 
                                 w, ".ctj.rda", "\"))")))
      }
    }
    if (indicator.type == "U5MR") {
      save(mcmc.array, file = file.path(output.dir, "mcmc.array.rda"))
      save(resproject.list.c, file = file.path(output.dir, "resproject.list.c.rda"))
      if (get.PPD) {
        save(ypredict.cij, file = file.path(output.dir, "ypredict.cij.rda"))
        save(ypredict.hivremoved.ciq, file = file.path(output.dir, "ypredict.hivremoved.ciq.rda"))
        save(y.ci, file = file.path(output.dir, "y.ci.rda"))
        save(bias.ci, file = file.path(output.dir, "bias.ci.rda"))
        save(stbias.ci, file = file.path(output.dir, "stbias.ci.rda"))
        save(q.ci, file = file.path(output.dir, "q.ci.rda"))
      }
    }
    # for is.validation.up.to.2000 files
    if (file.exists(file.path(output.dir.countryspecific, 
                              paste0(runname.prefix.countryspecific, "_", iso.all[1]),
                              "res.cqt.Lw(upto2000).rda"))) {
      res.cqt.Lw.all <- resall.cqt.Lw.all <- resmean.ct.Lw.all <- resARR.cq.Lwy.all <- 
        res.hivremoved.cqt.Lw.all <- res.logscale.hivremoved.cqt.Lw.all <- list()
      for (w in 1:nweightsplus1) {
        res.cqt.Lw.all[[w]] <- resall.cqt.Lw.all[[w]] <- 
          res.hivremoved.cqt.Lw.all[[w]] <- res.logscale.hivremoved.cqt.Lw.all[[w]] <- 
          array(NA, c(C, Q, nyears))
        resmean.ct.Lw.all[[w]] <- array(NA, c(C, nyears))
        resARR.cq.Lwy.all[[w]] <- list()
        for (y in 1:length(ARR.years)) {
          resARR.cq.Lwy.all[[w]][[y]] <- array(NA, c(C, Q))
        }
        names(resARR.cq.Lwy.all[[w]]) <- ARR.years
      }
      names(res.cqt.Lw.all) <- names(resall.cqt.Lw.all) <- names(resmean.ct.Lw.all) <- names(resARR.cq.Lwy.all) <- 
        names(res.hivremoved.cqt.Lw.all) <- names(res.logscale.hivremoved.cqt.Lw.all) <-
        weights.alpha.plusdefault
      files <- c("res.cqt.Lw", "resall.cqt.Lw", "resmean.ct.Lw", "resARR.cq.Lwy",
                 "res.hivremoved.cqt.Lw", "res.logscale.hivremoved.cqt.Lw")
      files.to.load <- c("year.t.rda", paste0(files, "(upto2000).rda"))
      for (c in 1:C) {
        cat(paste0("Combining country-specific output for country ", c, " (", iso.all[c], ") of ", C, " countries.\n"))
        sapply(files.to.load, LoadFile, 
               output.dir = file.path(output.dir.countryspecific, 
                                      paste0(runname.prefix.countryspecific, "_", iso.all[c])), 
               envir = environment())    
        for (w in 1:nweightsplus1) {
          res.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- res.cqt.Lw[[w]][1, , ]
          resall.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- resall.cqt.Lw[[w]][1, , ]
          res.hivremoved.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- res.hivremoved.cqt.Lw[[w]][1, , ]
          res.logscale.hivremoved.cqt.Lw.all[[w]][c, , is.element(year.all, year.t)] <- 
            res.logscale.hivremoved.cqt.Lw[[w]][1, , ]
          resmean.ct.Lw.all[[w]][c, is.element(year.all, year.t)] <- unlist(resmean.ct.Lw[[w]][1, ])
          for (y in 1:length(ARR.years)) {
            resARR.cq.Lwy.all[[w]][[y]][c, ] <- unlist(resARR.cq.Lwy[[w]][[y]][1, ])
          }
        }
      } # end country loop
      res.cqt.Lw <- res.cqt.Lw.all
      resall.cqt.Lw <- resall.cqt.Lw.all
      resmean.ct.Lw <- resmean.ct.Lw.all
      resARR.cq.Lwy <- resARR.cq.Lwy.all
      res.hivremoved.cqt.Lw <- res.hivremoved.cqt.Lw.all
      res.logscale.hivremoved.cqt.Lw <- res.logscale.hivremoved.cqt.Lw.all
      save(res.cqt.Lw, file = file.path(output.dir, "res.cqt.Lw(upto2000).rda"))
      save(resall.cqt.Lw, file = file.path(output.dir, "resall.cqt.Lw(upto2000).rda"))
      save(resmean.ct.Lw, file = file.path(output.dir, "resmean.ct.Lw(upto2000).rda"))
      save(resARR.cq.Lwy, file = file.path(output.dir, "resARR.cq.Lwy(upto2000).rda"))
      save(res.hivremoved.cqt.Lw, file = file.path(output.dir, "res.hivremoved.cqt.Lw(upto2000).rda"))
      save(res.logscale.hivremoved.cqt.Lw, 
           file = file.path(output.dir, "res.logscale.hivremoved.cqt.Lw(upto2000).rda"))
    }
    cat(paste0("Results combined and saved to ", output.dir, "\n"))
  }
  ##value<< \code{NULL}.
  return(invisible())
}
