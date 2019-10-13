#----------------------------------------------------------------------
# deriveimrestimates.R
# Jin Rou New, Jun 2013-2014
#----------------------------------------------------------------------
DeriveIMREstimatesFromU5MR <- function( # Derive IMR estimates from U5MR using model life table or Sahel equation
  iso.select, ##<< 3-character ISO country code of selected country.
  runname = NULL, ##<< If \code{NULL}, \code{IMRrunname.U5MR_iso.select} is used.
  runname.U5MR,
  runname.global.U5MR,
  runname.IMR = NULL, ##<< Required if a relative adjustment is used, i.e. Ratio of median(IMR)/median(U5MR) 
  ## is used to adjust and get IMR trajectories.
  runname.global.IMR = NULL, ##<< Required if a relative adjustment is used, i.e. Ratio of median(IMR)/median(U5MR) 
  ## is used to adjust and get IMR trajectories.
  weight.alpha.select = 0.5, ##<< If \code{NULL}, results generated for all weights.alpha that are available.
  data.cmeinfo.file,
  hiv.file = NULL,
  adj.file = NULL,
  lifetable.file = "input/data_lt.csv",
  weight.alpha.publish = 0.5, ##<< \code{weight.alpha} to use for Results.csv.
  year.start = NULL, ##<< Start year of estimates to output in .txt file. If \code{NULL}, defaults to first observation year for each country.
  year.end = NULL, ##<< End year of estimates to output in .txt file. If \code{NULL}, defaults to \code{mcmc.meta$settings$year.lastestimate}.
  year1 = 1990.5, 
  year2 = 2000.5, 
  year3 = 2005.5, 
  year4 = NULL, ##<< Last year used for ARR calculation. If \code{NULL}, defaults to \code{mcmc.meta$settings$year.lastestimatepublished} or \code{NULL} if validation run and
  ## if \code{mcmc.meta$settings$year.lastestimatepublished} > \code{mcmc.meta$settings$year.cutoff}.
  percentiles = c(0.05, 0.5, 0.95),
  output.dir = NULL, ##<< Directory where results output will be stored or if \code{NULL}, 
  ## directory \code{output/runname} is created in current working 
  ... ##<< Additional arguments for \code{RunMCMC}
) {
  if (is.null(runname))
    runname <- paste0("IMR", runname.U5MR, "_", iso.select)
  if (is.null(output.dir)) {
    dir.create(file.path(getwd(), "output"), showWarnings = FALSE) 
    dir.create(file.path(getwd(), "output", runname), showWarnings = FALSE) 
    output.dir <- paste0(getwd(), "/output/", runname, "/")
  }
  if (is.null(hiv.file))
    hiv.file <- file.path("input", "dataUNAIDS_IMR.csv")
  if (is.null(adj.file))
    adj.file <- file.path("input", "dataPostAdj_IMR.csv")
  if (is.null(runname.IMR) | iso.select == "PRK") {
    global.gamma.median <- NA
    global.gamma.sd<- NA
  } else {
    load(file.path("output", runname.IMR, "mcmc.meta.rda"))
    global.gamma.median <- mcmc.meta$settings$global.gamma.median
    global.gamma.sd<- mcmc.meta$settings$global.gamma.sd
    rm(mcmc.meta)
  }
  load(file.path("output", runname.U5MR, "mcmc.meta.rda"))
  hiv.file.U5MR <- mcmc.meta$files$hiv.file
  adj.file.U5MR <- mcmc.meta$files$adj.file
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
  # get mcmc.meta for IMR run
  if (!file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat(paste0("Starting RunMCMC to get mcmc.meta for ", runname, " IMR data.\n"))
    RunMCMC(runname = runname,
            iso.select = iso.select,
            run.type = "country",
            indicator.type = "IMR",
            runname.global = runname.global.IMR,
            runname.U5MR = paste0(runname.global.U5MR, "_all"),
            run.for.IMR.MLT = TRUE,
            data.cmeinfo.file = data.cmeinfo.file,
            country.info.file = mcmc.meta$files$country.info.file,
            country.B3info.file = mcmc.meta$files$country.B3info.file, 
            hiv.file = hiv.file,
            adj.file = adj.file,
            livebirths.file = mcmc.meta$files$livebirths.file,
            year.current = mcmc.meta$settings$year.current,
            year.lastestimatepublished = mcmc.meta$settings$year.lastestimatepublished,
            year.lastestimate = mcmc.meta$settings$year.lastestimate,
            year.cutoff = mcmc.meta$settings$year.cutoff,
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
            I = mcmc.meta$settings$I,
            global.gamma.median = global.gamma.median,
            global.gamma.sd = global.gamma.sd,
            ...)
    closeAllConnections()
    cat(paste0("mcmc.meta for ", runname, " saved to ", output.dir, ".\n"))
  }
  rm(mcmc.meta) # remove mcmc.meta for runname.U5MR
  load(file.path(output.dir, "mcmc.meta.rda")) # load mcmc.meta for runname
  
  if (!is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard"))) {
    if (sum(!file.exists(file.path("output", runname.U5MR, 
                                   c("year.t.rda", "res.cqt.Lw.rda", "u5.ctj.rda")))) > 0) {
      cat("Error: Results year.t.rda and/or res.cqt.Lw.rda and/or u5.ctj.rda is/are not found for ", 
          runname.U5MR, "!",  "\n")
      return()
    }
  } else {
    if (sum(c(!file.exists(file.path("output", runname.U5MR, 
                                     c("year.t.rda", "res.cqt.Lw.rda", "u5.ctj.rda"))),
              !file.exists(file.path("output", runname.IMR,
                                     c("year.t.rda", "res.cqt.Lw.rda"))))) > 0) {
      cat("Error: Results year.t.rda and/or res.cqt.Lw.rda and/or u5.ctj.rda is/are not found for ", 
          runname.U5MR, " and/or ", runname.IMR, "!",  "\n")
      return()
    }
  }
  
  if (is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard"))) {
    load(file.path("output", runname.IMR, "year.t.rda"))
    load(file.path("output", runname.IMR, "res.cqt.Lw.rda"))
    yearIMR.t <- year.t
    resIMR.cqt.Lw <- res.cqt.Lw
  }
  load(file.path("output", runname.U5MR, "year.t.rda"))
  load(file.path("output", runname.U5MR, "res.cqt.Lw.rda"))
  load(file.path("output", runname.U5MR, "u5.ctj.rda"))
  
  nyears <- length(year.t)
  nsim <- dim(u5.ctj)[3]; rm(u5.ctj)
  weights.alpha.plusdefault <- names(res.cqt.Lw)
  nweightsplus1 <- length(weights.alpha.plusdefault)
  weights.alpha <- weights.alpha.plusdefault[weights.alpha.plusdefault != '0']
  nweights <- length(weights.alpha)
  res.cqt.Lw <- resall.cqt.Lw <- resmean.ct.Lw <- resARR.cq.Lwy <- list()
  # declare result variables to be used
  for (w in 1:nweightsplus1) {
    res.cqt.Lw[[w]] <- resall.cqt.Lw[[w]] <- array(NA, c(1, length(percentiles), nyears))
    resmean.ct.Lw[[w]] <- array(NA, c(1, nyears))
    dimnames(res.cqt.Lw[[w]]) <- dimnames(resall.cqt.Lw[[w]]) <- 
      list(mcmc.meta$data.all$iso.c, percentiles, year.t)
    dimnames(resmean.ct.Lw[[w]]) <- list(mcmc.meta$data.all$iso.c, year.t)
    resARR.cq.Lwy[[w]] <- list()  
    for (y in 1:4) {
      resARR.cq.Lwy[[w]][[y]] <- array(NA, c(1, length(percentiles)))
      dimnames(resARR.cq.Lwy[[w]][[y]]) <- list(mcmc.meta$data.all$iso.c, percentiles)
    }
    names(resARR.cq.Lwy[[w]]) <- c(paste0(year1, "-", year3), paste0(year1, "-", year2), 
                                   paste0(year2, "-", year4), paste0(year1, "-", year4))
  }
  names(res.cqt.Lw) <- names(resall.cqt.Lw) <- names(resmean.ct.Lw) <- names(resARR.cq.Lwy) <- 
    weights.alpha.plusdefault
  res.hivremoved.cqt.Lw <- res.crisisremoved.cqt.Lw <- 
    res.crisisandhivremoved.cqt.Lw <- res.logscale.hivremoved.cqt.Lw <- res.cqt.Lw
  u1.ctj <- array(NA, c(1, nyears, nsim))
  # for other weights.alpha
  for (w in 1:nweights)
    eval(parse(text = paste0("u1new", w, ".ctj", " <- array(NA, c(1, nyears, nsim))")))
  if (!is.null(weight.alpha.select)) {
    weights.alpha.not.select <- weights.alpha.plusdefault[weights.alpha.plusdefault != weight.alpha.select]
  } else {
    weights.alpha.not.select <- NULL
  }
  # load U5MR estimates
  for (w in 1:nweightsplus1) {
    if (!is.element(weights.alpha.plusdefault[w], weights.alpha.not.select)) {
      weight.alpha <- weights.alpha.plusdefault[w]
      cat(paste0("Deriving for weight.alpha = ", weight.alpha, "\n"))
      if (w == 1) {
        load(file.path("output", runname.U5MR, "u5.ctj.rda"))
        u5.tj <- u5.ctj[1, , ]
      } else {
        load(file.path("output", runname.U5MR, paste0("u5new", w-1, ".ctj.rda")))   
        eval(parse(text = paste0("u5.tj <- u5new", w-1, ".ctj[1, , ]")))
      }
      #----------------------------------------------------------------------
      # obtain crisis-and-HIV-free U5MR estimates
      if (!is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard"))) {
        # undo crisis post-adjustment for crisisadj countries
        if (mcmc.meta$data.all$crisisadj.c) {
          u.median.t <- apply(u5.tj, 1, median)
          propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median.t, year.t = year.t, 
                                                  iso = mcmc.meta$data.all$iso.c, operation = "-",
                                                  adj.file = adj.file.U5MR)$propadj.t
          u5.crisisfree.tj <- apply(u5.tj, 2, "*", propadj.t)
        } else {
          u5.crisisfree.tj <- u5.tj
        }
        # undo HIV post-adjustment for countries with high HIV prevalence  
        if (mcmc.meta$data.all$hiv.c) {
          u.median2.t <- apply(u5.crisisfree.tj, 1, median)
          propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median2.t, year.t = year.t,
                                                  iso = mcmc.meta$data.all$iso.c, operation = "-",
                                                  hiv.file = hiv.file.U5MR)$propadjhiv.t      
          u5.hivcrisisfree.tj <- apply(u5.crisisfree.tj, 2, "*", propadjhiv.t)
        } else {
          u5.hivcrisisfree.tj <- u5.crisisfree.tj
        }
        # save for checking
        save(u5.hivcrisisfree.tj, file = file.path(output.dir, paste0("u5.hivcrisisfree.tj_w", w, ".rda")))
      } else {
        u5.hivcrisisfree.tj <- u5.tj # no change, final estimates used!
        # save for checking
        save(u5.tj, file = file.path(output.dir, paste0("u5.tj_w", w, ".rda")))
      } # end not B3/Nonstandard loop
      #----------------------------------------------------------------------
      # derive IMR estimates
      if (is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard"))) {
        cat(paste0("Obtaining estimates with relative adjustment from B3 U5MR and IMR estimates for ", 
                   mcmc.meta$data.all$name.c, ".\n"))
        u5.median.t <- apply(u5.hivcrisisfree.tj, 1, median)[is.element(year.t, yearIMR.t)]
        u1.median.t <- resIMR.cqt.Lw[[which(names(resIMR.cqt.Lw) == paste0(weight.alpha))]][1, 2, ]
        ratio.u1u5.t <- u1.median.t/u5.median.t
        u1.hivcrisisfree.tj <- apply(u5.hivcrisisfree.tj[is.element(year.t, yearIMR.t), ], 
                                     2, "*", ratio.u1u5.t) # note: final IMR estimates    
        year.t.final <- yearIMR.t        
      } else if (grepl("MLT", mcmc.meta$data.all$imrmethod.c)) { # by MLT
        cat(paste0("Obtaining estimates via model life table method for ", mcmc.meta$data.all$name.c, ".\n"))
        u1.hivcrisisfree.tj <- apply(u5.hivcrisisfree.tj, 2, GetEstimateViaMLT,  
                                     result = "IMR", 
                                     lifetable = sub("MLT ", "", mcmc.meta$data.all$imrmethod.c),
                                     lifetable.file = lifetable.file)
        # check for extreme U5MR values which lie outside MLT range
        select.nonNA <- apply(u1.hivcrisisfree.tj, 1, function(x) sum(is.na(x)) == 0)
        ntrajs.NA <- apply(u1.hivcrisisfree.tj, 1, function(x) sum(is.na(x)))
        select.trajs.NA <- apply(u1.hivcrisisfree.tj, 2, function(x) sum(is.na(x)) > 0)
        if (sum(!select.nonNA) > 0) {
          cat(paste0("Warning: ", mcmc.meta$data.all$name.c, " ", mcmc.meta$data.all$iso.c, " - NA obtained for the years: ", 
                     paste(year.t[!select.nonNA], collapse = ", "), ".\n"))
          if (length(year.t[year.t >= 1990.5 & ntrajs.NA > 0]) > 0) {
            cat(paste0("Note: ", mcmc.meta$data.all$name.c, " ", mcmc.meta$data.all$iso.c, " - The years (only 1990.5 onwards shown) ", 
                       paste(year.t[year.t >= 1990.5 & ntrajs.NA > 0], collapse = ", "),
                       " have ",
                       paste(ntrajs.NA[year.t >= 1990.5 & ntrajs.NA > 0], collapse = ", "),
                       " trajectory(ies) with NA values respectively.\n"))
            # replace NA with min or max value
            for (t.select in (1:nrow(u1.hivcrisisfree.tj))[!select.nonNA]) {
              u1.hivcrisisfree.tj[t.select, select.trajs.NA] <- 
                GetEstimateViaMLT(x.obs = u5.hivcrisisfree.tj[t.select, select.trajs.NA],
                                  result = "IMR", allow.na = FALSE,
                                  lifetable = sub("MLT ", "", mcmc.meta$data.all$imrmethod.c),
                                  lifetable.file = lifetable.file)
            }
          }
          # note: we need estimates from at least 1990!
          year.cutoff <- ifelse(length(year.t[!select.nonNA & year.t < 1990.5]) == 0, 
                                min(year.t)-1, max(year.t[!select.nonNA & year.t < 1990.5]))
          year.t.final <- year.t[year.t > year.cutoff] 
          u1.hivcrisisfree.tj <- u1.hivcrisisfree.tj[is.element(year.t, year.t.final), ]
        } else {
          year.t.final <- year.t
        } # end select.nonNA loop
      } else if (mcmc.meta$data.all$imrmethod.c == "Sahel") { # by Sahel equation
        cat(paste0("Obtaining estimates via Sahel equation for ", mcmc.meta$data.all$name.c, ".\n"))
        u5.hivcrisisfree.tj[u5.hivcrisisfree.tj > 1000] <- 1000 # U5MR cannot be above 1000
        u1.hivcrisisfree.tj <- GetIMRFromSahelEquation(u5mr = u5.hivcrisisfree.tj,
                                                       sahelvalue = as.numeric(mcmc.meta$data.all$sahel.c))
        year.t.final <- year.t
      } # end derive IMR estimates loop
      # save for checking
      if (!is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard"))) {
        u1.hivcrisisfree.tj[u1.hivcrisisfree.tj > 1000] <- 1000
        save(u1.hivcrisisfree.tj, file = file.path(output.dir, paste0("u1.hivcrisisfree.tj_w", w, ".rda")))
      } else {
        u1.hivcrisisfree.tj[u1.hivcrisisfree.tj > 1000] <- 1000
        u1.tj <- u1.hivcrisisfree.tj
        save(u1.tj, file = file.path(output.dir, paste0("u1.tj_w", w, ".rda")))
      }
      save(year.t.final, file = file.path(output.dir, paste0("year.t.final_w", w, ".rda")))
      # get results 
      res <- CalculateQuantities(u5temp.tj = u1.hivcrisisfree.tj,
                                 iso = mcmc.meta$data.all$iso.c,
                                 indicator.type = indicator.type,
                                 # note: no further adjustments needed if relative adjustment is used, estimates are final 
                                 hiv = ifelse(!is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard")), 
                                              mcmc.meta$data.all$hiv.c, FALSE),
                                 hiv.file = hiv.file,
                                 # note: no further adjustments needed if relative adjustment is used, estimates are final 
                                 crisisadj = ifelse(!is.element(mcmc.meta$data.all$imrmethod.c, c("B3", "Nonstandard")), 
                                                    mcmc.meta$data.all$crisisadj.c, FALSE),
                                 adj.file = adj.file,
                                 year.t = year.t.final, year.i = year.t.final,
                                 estyear.min = ifelse(mcmc.meta$data.all$iso.c == "RUS", 1970.5, 1990.5),
                                 year1 = year1, year2 = year2, year3 = year3, 
                                 year4 = year4,
                                 percentiles = percentiles)
      # store results
      if (w == 1) {
        u1.ctj[1, is.element(year.t, year.t.final), ] <- res$u5.tj
      } else {
        eval(parse(text = paste0("u1new", w-1, ".ctj[1, is.element(year.t, year.t.final), ] <- res$u5.tj")))
      }
      resall.cqt.Lw[[paste(weight.alpha)]][1, , is.element(year.t, year.t.final)] <- res$resall.qt
      res.cqt.Lw[[paste(weight.alpha)]][1, , is.element(year.t, year.t.final)] <- res$res.qt
      resmean.ct.Lw[[paste(weight.alpha)]][1, is.element(year.t, year.t.final)] <- res$resmean.t
      for (y in 1:4) {
        resARR.cq.Lwy[[paste(weight.alpha)]][[y]][1, ] <- unlist(res$resARR.q.Ly[[y]])
      }
      
      # get HIV-removed and HIV-removed & log scale results
      # change JR, 20150602: get crisis-removed and crisis-and-HIV-removed results
      # start with final results
      res.hivremoved.cqt.Lw[[paste(weight.alpha)]] <- res.crisisremoved.cqt.Lw[[paste(weight.alpha)]] <- 
        res.crisisandhivremoved.cqt.Lw[[paste(weight.alpha)]] <- 
        res.logscale.hivremoved.cqt.Lw[[paste(weight.alpha)]] <- 
        res.cqt.Lw[[paste(weight.alpha)]]
      # undo crisis post-adjustment for crisisadj countries
      u.median.t <- res.cqt.Lw[[paste0(weight.alpha)]][1, percentiles == 0.5, 
                                                       is.element(year.t, year.t.final)]
      if (mcmc.meta$data.all$crisisadj.c) {
        # get crisis-free results that include only HIV adjustments
        propadj.t <- GetCrisisAdjustedEstimates(u.t = u.median.t, year.t = year.t.final, 
                                                iso = mcmc.meta$data.all$iso.c,
                                                operation = "-",
                                                adj.file = adj.file)$propadj.t
        res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)] <- 
          res.crisisremoved.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)] <- 
          t(apply(res.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)], 
                  1, "*", propadj.t))
      }
      # undo HIV post-adjustment for countries with high HIV prevalence  
      if (mcmc.meta$data.all$hiv.c) {
        # change JR, 20150602: fixed bug: assume relative uncertainty in 
        # unadjusted U5MR equal to relative uncertainty in adjusted U5MR.
        # relative adjustment was not correctly carried out before this date for res files.
        # get HIV-free results that include only crisis adjustments
        propadjhiv.t <- GetHIVAdjustedEstimates(u.t = u.median.t, year.t = year.t.final, 
                                                iso = mcmc.meta$data.all$iso.c,
                                                operation = "-",
                                                hiv.file = hiv.file)$propadjhiv.t
        res.hivremoved.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)] <- 
          t(apply(res.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)], 
                  1, "*", propadjhiv.t))
        # get HIV and crisis-free results
        u.median2.t <- res.crisisremoved.cqt.Lw[[paste0(weight.alpha)]][
          1, percentiles == 0.5, is.element(year.t, year.t.final)]
        propadjhiv2.t <- GetHIVAdjustedEstimates(u.t = u.median2.t, year.t = year.t.final, 
                                                 iso = mcmc.meta$data.all$iso.c,
                                                 operation = "-",
                                                 hiv.file = hiv.file)$propadjhiv.t
        res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)] <-
          t(apply(res.crisisremoved.cqt.Lw[[paste0(weight.alpha)]][1, , is.element(year.t, year.t.final)], 
                  1, "*", propadjhiv2.t))
      }
      res.logscale.hivremoved.cqt.Lw[[paste0(weight.alpha)]] <- 
        log(res.hivremoved.cqt.Lw[[paste0(weight.alpha)]])
    } # end weights.alpha.not.select loop
  } # end weights.alpha loop
  
  # output results
  load(file.path(output.dir, paste0("year.t.final_w", 
                                    which(weights.alpha.plusdefault == weight.alpha.publish), 
                                    ".rda")))
  # change JR, 20150602: optimised code by replacing with OutputResultsWide function
  OutputResultsWide(res.cqt = res.cqt.Lw[[paste0(weight.alpha.publish)]][
    , , is.element(year.t, year.t.final), drop = FALSE],
                    name.c = mcmc.meta$data.all$name.c,
                    iso.c = mcmc.meta$data.all$iso.c,
                    year.t = year.t.final,
                    indicator.type = "IMR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results")
  # save all quantities
  iso.c <- mcmc.meta$data.all$iso.c
  name.c <- mcmc.meta$data.all$name.c
  save(iso.c, file = file.path(output.dir, "iso.c.rda"))
  save(name.c, file = file.path(output.dir, "name.c.rda"))
  save(year.t, file = file.path(output.dir, "year.t.rda"))
  save(year.t.final, file = file.path(output.dir, "year.t.final.rda")) # corresponds to weight.alpha.publish
  save(res.cqt.Lw, file = file.path(output.dir, paste0("res.cqt.Lw.rda")))
  save(resall.cqt.Lw, file = file.path(output.dir, paste0("resall.cqt.Lw.rda")))
  save(resmean.ct.Lw, file = file.path(output.dir, paste0("resmean.ct.Lw.rda")))
  save(resARR.cq.Lwy, file = file.path(output.dir, paste0("resARR.cq.Lwy.rda")))
  if (exists("u1.ctj", envir = environment())) {
    u5.ctj <- u1.ctj
    save(u5.ctj, file = file.path(output.dir, paste0("u5.ctj.rda")))
  }
  for (w in 1:nweights) {
    if (exists(paste0("u1new", w, ".ctj"), envir = environment())) { 
      eval(parse(text = paste0("u5new", w, ".ctj <- ", "u1new", w, ".ctj")))
      eval(parse(text = paste0("save(u5new", w, ".ctj, file = file.path(output.dir, \"u5new", 
                               w, ".ctj.rda", "\"))")))
    }
  }
  save(res.hivremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.hivremoved.cqt.Lw.rda")))
  save(res.crisisremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.crisisremoved.cqt.Lw.rda")))
  save(res.crisisandhivremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.crisisandhivremoved.cqt.Lw.rda")))
  save(res.logscale.hivremoved.cqt.Lw, 
       file = file.path(output.dir, paste0("res.logscale.hivremoved.cqt.Lw.rda")))
  OutputResultsWide(res.cqt = res.hivremoved.cqt.Lw[[paste0(weight.alpha.publish)]][
    , , is.element(year.t, year.t.final), drop = FALSE],
                    name.c = mcmc.meta$data.all$name.c,
                    iso.c = mcmc.meta$data.all$iso.c,
                    year.t = year.t.final,
                    indicator.type = "IMR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (HIV-free)")
  OutputResultsWide(res.cqt = res.crisisremoved.cqt.Lw[[paste0(weight.alpha.publish)]][
    , , is.element(year.t, year.t.final), drop = FALSE],
                    name.c = mcmc.meta$data.all$name.c,
                    iso.c = mcmc.meta$data.all$iso.c,
                    year.t = year.t.final,
                    indicator.type = "IMR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (crisis-free)")
  OutputResultsWide(res.cqt = res.crisisandhivremoved.cqt.Lw[[paste0(weight.alpha.publish)]][
    , , is.element(year.t, year.t.final), drop = FALSE],
                    name.c = mcmc.meta$data.all$name.c,
                    iso.c = mcmc.meta$data.all$iso.c,
                    year.t = year.t.final,
                    indicator.type = "IMR",
                    year.start = 1931.5,
                    year.end = year.end,
                    output.dir = output.dir,
                    file.name = "Results (crisis-and-HIV-free)")
  cat(paste0("IMR results obtained for ", mcmc.meta$data.all$name.c, 
             " (", mcmc.meta$data.all$iso.c, ") and saved in ", output.dir, "\n"))
}
