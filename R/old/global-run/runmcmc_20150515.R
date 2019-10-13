#----------------------------------------------------------------------
# runmcmc.R
#----------------------------------------------------------------------
RunMCMC <- function(# Run MCMC sampling
  ### Run MCMC sampling for the B3 model and save mcmc.meta and JAGS objects to \code{output.dir}.
  runname = "test", ##<< Run name, used to create a directory \code{output/runname}
  ## with JAGS output (after MCMC sampling), and estimates (in next steps).
  iso.select = NULL, ##<< If \code{NULL}, a global run will be initiated. 
  isos.to.exclude.for.global.run = c("LIE", "PRK", "SPS", "MCO"), ##<< Vector of 3-character ISO country codes
  ## of country data to exclude for global run. Default: Liechtenstein, Korea DPR & Sudan pre-secession (as estimates 
  ## are no longer produced with B3 for these countries); Andorra & Monaco (as neighbouring regions' data are used)
  output.dir = NULL, ##<< Directory where mcmc.meta and raw MCMC output will be stored
  ## either in an existing directory, or if \code{NULL}, directory \code{output/runname} is created
  ## in current working directory
  seed.MCMC = 1, ##<< Seed for initializing MCMC, defaults to 1.
  chain.ids = seq(1, 6), ##<< IDs of chains to run in series 
  ## (the IDs need to be numeric because they are used to set the seed).
  nsteps = ifelse(run.type == "global", 10, 5), ##<< For each \code{niterperstep} iterations, the iterations will be saved. 
  nthin = ifelse(run.type == "global", 20, 30), ##<< Thinning factor.
  nburnin = 25000, ##<< Burn-in (excluded samples at start of chain). # change JR, 20140602: from 10000 to 25000
  niterperstep = ifelse(run.type == "global", 5000, 7500), ##<< Number of iterations, NOT including burn-in.
  data.cmeinfo.file = NULL, ##<< File path of data from CME Info. If \code{NULL}, child mortality data included in package is used. 
  data.file = NULL, ##<< If \code{NULL}, data processed from data.cmeinfo.file is used. 
  data.all.file = NULL, ##<< If \code{NULL}, excluded data processed from data.cmeinfo.file is used.
  country.info.file = NULL, ##<< If \code{NULL}, country info included in package is used.
  country.B3info.file = NULL, ##<< If \code{NULL}, country B3 info included in package is used. 
  hiv.file = NULL, ##<< If \code{NULL}, UNAIDS HIV mortality adjustment data included in package is used. 
  adj.file = NULL, ##<< If \code{NULL}, WHO crisis mortality adjustment data included in package is used. 
  livebirths.file = NULL, ##<< If \code{NULL}, WPP live birth data included in package is used.
  run.on.server = FALSE, ##<< Logical value indicating whether or not to run on the server.
  run.jags = TRUE, ##<< Logical values indicating whether or not to run JAGS.
  run.for.IMR.MLT = FALSE, ##<< Logical values indicating whether or not to run to get data only for IMR MLT estimates.
  indicator.type = "U5MR", ##<< Takes two possible values, "U5MR" or "IMR" for desired indicator to estimate.
  year.current = 2015.5, ##<< Current year. No series date can be after this date.
  year.lastestimatepublished = year.current-1, ##<< Year of last estimate published.
  year.lastestimate = 2015.5, ##<< Year of last estimate.
  runname.U5MR = NULL, ##<< Character indicating runname of B3 U5MR run required for estimating IMR. 
  ## If \code{NULL}, B3 U5MR estimates included in package is used.
  runname.igme = paste0("IGME", floor(year.current)-1),  ##<< Runname of UN IGME run of the previous year. The folder
  ## \code{output/runname.igme} should contain the U5MR estimates in an \code{res.U5MR.rda} file.
  se.censusindirect.missing = 0.025, ##<< Inputed value of relative non-VR SE for Census Indirect observations if missing (NA). Do not change.
  se.othernonvr.missing = 0.1, ##<< Inputed value of relative non-VR SE for all other non-VR observations (not Census Indirect) if missing (NA). Do not change.
  se.vr.min = 0.025, ##<< Minimum relative VR SE (no relative VR SE is lower than this value). Do not change.
  se.vr.missing = 0.1, ##<< Inputed value of relative VR SE if missing (NA). Do not change.
  recall.mid = 10, ##<< Mean of retrospective period (set at 10 years), used to centre retrospective period variable in model # change JR, 20140501
  dhsdirect.prior.mu.mubeta1 = ifelse(indicator.type == "U5MR", -0.0123, -0.0259900303333333), ##<< Prior value for the mean DHS Direct bias level (based on UN IGME 2012). Do not change.
  dhsdirect.prior.sigma.mubeta1 = ifelse(indicator.type == "U5MR", 0.00555581516674627, 0.0071070833850106), ##<< Prior value for the sd of DHS Direct bias level (based on UN IGME 2012) Do not change.
  is.validation = FALSE, ##<< Logical value indicating whether or not to do a validation run.
  year.cutoff = 2006, ##<< Used only if \code{is.validation} = \code{TRUE}):
  ## All data with series year after and IN year.cutoff are excluded.
  fit.B2.model = FALSE, ##<< Logical value indicating whether or not to fit B2 model (without data model).
  input.vr.se = TRUE, ##<< Logical value indicating whether or not to read in/input VR SE or 
  ## estimate country-specific VR SE. Set to \code{FALSE} to fit B2 model.
  include.HIV.countries = TRUE, ##<< Logical value indicating whether or not to include HIV countries in global run.
  apply.rules = TRUE, ##<< Logical value indicating whether to apply global smoothing or 
  ## to fix series level bias for early series for countries that fall under the set rules for doing so.
  add.dhsdirect.bias = FALSE, ##<< Logical value indicating whether or not to add DHS Direct bias.
  set.dhsdirect.prior = TRUE, ##<< Logical value indicating whether or not to set posterior median estimates
  ## from multilevel model for DHS Direct prior.
  use.constant.sigma.u = FALSE, ##<< Logical value indicating whether or not sigma.u is constant in a country.
  use.country.variance.multipliers = FALSE, ##<< Logical value indicating whether or not country variance 
  ## multipliers should be used.
  run.type = "global", ##<< Takes the values "global" (global run), "country" (country-specific run) or
  ## "combined" (combined run).
  runname.global = NULL, ##<< Character specifying runname of global run. If \code{NULL},
  ## global run results included in package are used. 
  I = 2.5, ##<< Interval length between two knots during observation period.
  periods.unsmooth.list = NULL, ##<< (Country tweak) List of vectors of length 2 with start and end years of periods to unsmooth spline fit.
  periods.smooth.list = NULL, ##<< (Country tweak) List of vectors of length 2 with start and end years of periods to smooth spline fit.
  periods.constant.list = NULL, ##<< (Country tweak) List of vectors of length 2 with start and end years of periods without decline/with constant fit. # change JR, 24 Jun
  get.jags.predictions = FALSE, ##<< Logical value indicating whether or not to get predictions directly from JAGS. ### not working
  global.gamma.median = NULL, ##<< Bayesian melding global gamma median parameter.
  global.gamma.sd = NULL ##<< Bayesian melding global gamma sd parameter.
) {
  chain.ids <- unique(chain.ids)
  ##details<< All output is written to folder output.dir (you wil get a message if it already exists).
  ## JAGS objects are written to output.dir/temp.JAGSobjects.
  if (is.null(output.dir)) {
    dir.create(file.path(getwd(), "output"), showWarnings = FALSE) 
    output.dir <- paste0(getwd(), "/output/", runname, "/")
  }
  if (file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat(paste("The output directory", output.dir, "already contains an mcmc.meta (and probably an older MCMC run).\n"))
    cat(paste("Delete files in this directory or choose a different runname/output directory.\n"))
    return(invisible())
  }
  dir.create(output.dir, showWarnings = FALSE)
  if (!run.for.IMR.MLT)
    dir.create(file.path(output.dir, "temp.JAGSobjects"), showWarnings = FALSE) 
  
  filename <- paste0(output.dir, "logfile.txt")
  cat(paste0("Seed and data files used are written to logfile ", filename, "."), "\n")
  fileout <- file(filename, open = "wt")
  sink(fileout, split = T)
  
  if (!run.for.IMR.MLT)
    cat(paste0("seed.MCMC is ", seed.MCMC, "."), "\n")
  
  ##details<< Object \code{val.info} is created, which is \code{NULL} if this run is not a validation exercise.
  ## Observations are left out if their series year is in or after \code{year.cutoff}.
  #  ##details<< If it is a validation exercise, \code{val.info} includes:
  #  ##describe<< 
  if (is.validation) {
    val.info <- list(year.cutoff = year.cutoff)# ##<< From arguments
    # ##end<<
  } else {
    val.info <- NULL
  }
  #----------------------------------------------------------------------
  # File paths and settings
  if (is.null(country.B3info.file))
    country.B3info.file <- file.path("input", "infoUNinclHIV.csv")
  if (is.null(data.cmeinfo.file))
    data.cmeinfo.file <- file.path("input", paste0("data_", indicator.type, "_CMEInfo.csv"))
  if (is.null(country.info.file))
    country.info.file <- file.path("input", "country.info.CME.csv")
  if (is.null(livebirths.file))
    livebirths.file <- file.path("input", "data_livebirths.csv")
  if (indicator.type == "IMR") { # load U5MR results if doing IMR run
    runname.U5MR <- ifelse(is.null(runname.U5MR), "IGME2013", runname.U5MR)
    if (!file.exists(file.path("output", runname.U5MR, "res.U5MR.rda"))) {
      cat(paste0("Error: No res.U5MR file in output/", runname.U5MR, ", get U5MR results first!"))
      return(invisible())
    }
  }
  if (is.null(hiv.file))
    hiv.file <- file.path("input", paste0("dataUNAIDS_", indicator.type, ".csv"))
  if (is.null(adj.file))
    adj.file <- file.path("input", paste0("dataPostAdj_", indicator.type, ".csv"))
  
  data.cmeinfo.temp <- read.csv(data.cmeinfo.file, header = T, stringsAsFactors = F, strip.white = T)
  if (!is.null(iso.select))
    data.cmeinfo.temp <- data.cmeinfo.temp[data.cmeinfo.temp$Country.Code %in% iso.select, ]
  # Check for country tweaks to apply # change JR, 20140501
  if (apply.rules) {
    isos.for.global.smoothing <- ApplyRules(run.type = run.type, # change JR, 20140606
                                            data.cmeinfo = data.cmeinfo.temp, 
                                            country.B3info.file = country.B3info.file, 
                                            country.info.file = country.info.file,
                                            livebirths.file = livebirths.file, 
                                            year.lastestimatepublished = year.lastestimatepublished, 
                                            isos.to.exclude = isos.to.exclude.for.global.run[!(
                                              isos.to.exclude.for.global.run %in% iso.select)],
                                            output.dir = output.dir)
  } else {
    isos.for.global.smoothing <- ""
  }
  data.processed.cmeinfo.file <- file.path(output.dir, "data_CMEInfo.csv")
  if (!run.for.IMR.MLT)
    CleanDataFromCMEInfo(data.cmeinfo.file = data.processed.cmeinfo.file,
                         country.B3info.file = country.B3info.file,
                         hiv.file = hiv.file,
                         runname = runname, iso.select = iso.select, output.dir = output.dir,
                         indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                         is.validation = is.validation, fit.B2.model = fit.B2.model,
                         year.current = year.current)
  # get data including excluded observations for plotting
  CleanDataFromCMEInfo(data.cmeinfo.file = data.processed.cmeinfo.file,
                       country.B3info.file = country.B3info.file,
                       hiv.file = hiv.file,
                       runname = runname, iso.select = iso.select, output.dir = output.dir,
                       indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                       is.validation = is.validation, fit.B2.model = fit.B2.model,
                       includeExcluded = TRUE,
                       year.current = year.current)
  if (is.null(data.file))
    data.file <- file.path(output.dir, paste0("data", indicator.type, "_clean_inclHIV.csv"))
  if (is.null(data.all.file))
    data.all.file <- file.path(output.dir, paste0("data", indicator.type, "_clean_inclHIV_inclExcluded.csv"))
  if (run.type != "global" & !run.for.IMR.MLT) { # load global run data if running country-specific run
    if (is.null(runname.global)) {
      runname.global <- "GR20140530notweaks"
      if (!file.exists(file.path("input/data.global.rda"))) {
        cat(paste0("Error: No default data.global file in input folder. Run global run first or specify runname.global!\n"))
        return(invisible())
      } else {
        load(file = file.path("input/data.global.rda"))
        cat(paste0("Default global run loaded.\n"))
      }
    } else {
      if (!file.exists(file.path("output", runname.global, "data.global.rda")))
        SummariseGlobalRun(runname.global = runname.global)
      load(file = file.path("output", runname.global, "data.global.rda"))
      cat(paste0("Default global run loaded.\n"))
    }
  } else {
    data.global <- NULL
  }
  cat(paste0("External inputs read in.\n"))
  if (is.null(global.gamma.median)) {
    if (run.type == "combined") {
      stop("Please input a value for global.gamma.median!")
    } else if (run.type == "country") {
      if (iso.select != "PRK") {
        global.gamma.median <- data.global$global.gamma.median
      } else {
        global.gamma.median <- NULL
      }
    }
  }
  if (is.null(global.gamma.sd)) {
    if (run.type == "combined") {
      stop("Please input a value for global.gamma.sd!")
    } else if (run.type == "country") {
      if (iso.select != "PRK") {
        global.gamma.sd <- data.global$global.gamma.sd
      } else {
        global.gamma.sd <- NULL
      }
    }
  }

  ##details<< Object \code{files} is created, with includes the following file paths:
  #  ##describe<< 
  files <- list(data.cmeinfo.file = data.cmeinfo.file, 
                data.file = data.file, 
                data.all.file = data.all.file,
                country.info.file = country.info.file,
                country.B3info.file = country.B3info.file, 
                hiv.file = hiv.file, 
                adj.file = adj.file,
                livebirths.file = livebirths.file)
  ##details<< Object \code{settings} is created, with includes the following settings:
  #  ##describe<< 
  settings <- list(year.current = year.current,# ##<< From arguments
                   year.lastestimatepublished = year.lastestimatepublished,# ##<< From arguments
                   year.lastestimate = year.lastestimate,# ##<< From arguments
                   year.cutoff = year.cutoff,# ##<< From arguments
                   indicator.type = indicator.type,# ##<< From arguments
                   runname.U5MR = runname.U5MR, # ##<< From arguments
                   runname.igme = runname.igme,# ##<< From arguments
                   se.censusindirect.missing = se.censusindirect.missing, # ##<< From arguments
                   se.othernonvr.missing = se.othernonvr.missing, # ##<< From arguments
                   se.vr.min = se.vr.min, # ##<< From arguments
                   se.vr.missing = se.vr.missing, # ##<< From arguments
                   recall.mid = recall.mid, # ##<< From arguments
                   dhsdirect.prior.mu.mubeta1 = dhsdirect.prior.mu.mubeta1, # ##<< From arguments
                   dhsdirect.prior.sigma.mubeta1 = dhsdirect.prior.sigma.mubeta1, # ##<< From arguments
                   is.validation = is.validation,# ##<< From arguments
                   fit.B2.model = fit.B2.model,# ##<< From arguments
                   input.vr.se = input.vr.se, # ##<< From arguments
                   include.HIV.countries = include.HIV.countries, # ##<< From arguments
                   add.dhsdirect.bias = add.dhsdirect.bias, # ##<< From arguments
                   set.dhsdirect.prior = set.dhsdirect.prior,# ##<< From arguments
                   use.constant.sigma.u = use.constant.sigma.u,# ##<< From arguments
                   use.country.variance.multipliers = use.country.variance.multipliers,# ##<< From arguments
                   run.type = run.type,# ##<< From arguments
                   runname.global = runname.global,# ##<< From arguments
                   I = I,# ##<< From arguments
                   isos.for.global.smoothing = isos.for.global.smoothing, # change JR, 20140502
                   periods.unsmooth.list = periods.unsmooth.list,# ##<< From arguments
                   periods.smooth.list = periods.smooth.list,# ##<< From arguments
                   periods.constant.list = periods.constant.list,# ##<< From arguments
                   get.jags.predictions = get.jags.predictions,# ##<< From arguments
                   global.gamma.median = global.gamma.median,# ##<< From \code{data.global$global.gamma.median} if \code{run.type} is "country", else \code{NULL}
                   global.gamma.sd = global.gamma.sd)# ##<< From \code{data.global$global.gamma.sd} if \code{run.type} is "country", else \code{NULL} 
  # ##end<<
  # write settings to tweaks.txt
  if (!run.for.IMR.MLT) {
    filename <- paste0(output.dir, "tweaks.txt")
    cat(paste0("Information about run is written to ", filename, "."), "\n")
    fileout <- file(filename, open = "wt")
    sink(fileout, split = T)
    cat(paste0("runname is ", runname, "\n"))
    cat(paste0("data.cmeinfo.file used is ", data.cmeinfo.file, "\n"))
    if (is.null(periods.smooth.list)) {
      cat("Periods to smooth are: NA\n")
    } else {
      cat("Periods to smooth are:\n")
      for (p in 1:length(periods.smooth.list)) {
        cat(paste0(periods.smooth.list[[p]][1], "-",
                   periods.smooth.list[[p]][2], "\n"))
      }
    }
    if (is.null(periods.unsmooth.list)) {
      cat("Periods to unsmooth are: NA\n")
    } else {
      cat("Periods to unsmooth are:\n")
      for (p in 1:length(periods.unsmooth.list)) {
        cat(paste0(periods.unsmooth.list[[p]][1], "-",
                   periods.unsmooth.list[[p]][2], "\n"))
      }
    }
    if (is.null(periods.constant.list)) {
      cat("Periods without decline are: NA\n")
    } else {
      cat("Periods without decline are:\n")
      for (p in 1:length(periods.constant.list)) {
        cat(paste0(periods.constant.list[[p]][1], "-",
                   periods.constant.list[[p]][2], "\n"))
      }
    }
    sink()
  }
  cat(paste0("Settings saved.\n"))
  #---------------------------------------------------------------------- 
  # Data
  if (!run.for.IMR.MLT) {
    data <- ReadData(country.codes = iso.select,
                     country.B3info.file = country.B3info.file,
                     data.file = data.file,
                     hiv.file = hiv.file,
                     adj.file = adj.file,
                     isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                     settings = settings)
    # Data HIV-removed
    data.hivremoved <- ReadData(country.codes = iso.select,
                                get.HIV.removed.data = TRUE,
                                country.B3info.file = country.B3info.file,
                                data.file = data.file,
                                hiv.file = hiv.file,
                                adj.file = adj.file,
                                isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                                settings = settings,
                                print.messages = FALSE)
    # Data on log scale, HIV-removed
    data.logscale.hivremoved <- ReadData(country.codes = iso.select,
                                         get.HIV.removed.data = TRUE,
                                         get.log.scale.data = TRUE,
                                         country.B3info.file = country.B3info.file,
                                         data.file = data.file,
                                         hiv.file = hiv.file,
                                         adj.file = adj.file,
                                         isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                                         settings = settings,
                                         print.messages = FALSE)
  } else {
    data <- data.hivremoved <- data.logscale.hivremoved <- NULL
  }
  # Data for validation exercise
  if (is.validation) {
    data.val <- GetValidationData(data = data, year.cutoff = year.cutoff)
    # Run again to include only countries with at least 2 and 1 obs in training and test data respectively 
    include.c <- ifelse(data.val$ntrain.c < 2 | data.val$ntest.c == 0, FALSE, TRUE)
    if (sum(include.c) > 0) {
      iso.select <- data$iso.c[include.c] # iso.select becomes subset of ISOs for validation
      data <- ReadData(country.codes = iso.select,
                       country.B3info.file = country.B3info.file,
                       data.file = data.file,
                       hiv.file = hiv.file,
                       adj.file = adj.file,
                       isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                       settings = settings,
                       print.messages = FALSE)
      data.hivremoved <- ReadData(country.codes = iso.select,
                                  get.HIV.removed.data = TRUE,
                                  country.B3info.file = country.B3info.file,
                                  data.file = data.file,
                                  hiv.file = hiv.file,
                                  adj.file = adj.file,
                                  isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                                  settings = settings,
                                  print.messages = FALSE)
      data.logscale.hivremoved <- ReadData(country.codes = iso.select,
                                           get.HIV.removed.data = TRUE,
                                           get.log.scale.data = TRUE,
                                           country.B3info.file = country.B3info.file,
                                           data.file = data.file,
                                           hiv.file = hiv.file,
                                           adj.file = adj.file,
                                           isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                                           settings = settings,
                                           print.messages = FALSE)
      data.val <- GetValidationData(data = data, year.cutoff = year.cutoff)
    } else {
      cat("No data available before validation cut-off year.\n")
      cat("---------- End RunMCMC ----------\n")
      return(invisible())
    }
  } else {
    data.val <- NULL
  } # end isValidation
  # Data (including excluded)
  data.all <- ReadData(country.codes = iso.select,
                       country.B3info.file = country.B3info.file,
                       data.file = data.all.file,
                       hiv.file = hiv.file,
                       adj.file = adj.file,
                       isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                       settings = settings,
                       print.messages = FALSE)
  # Data HIV-removed (including excluded)
  data.hivremoved.all <- ReadData(country.codes = iso.select,
                                  get.HIV.removed.data = TRUE,
                                  country.B3info.file = country.B3info.file,
                                  data.file = data.all.file,
                                  hiv.file = hiv.file,
                                  adj.file = adj.file,
                                  isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                                  settings = settings,
                                  print.messages = FALSE)
  # Data on log scale, HIV-removed (including excluded)
  data.logscale.hivremoved.all <- ReadData(country.codes = iso.select,
                                           get.HIV.removed.data = TRUE,
                                           get.log.scale.data = TRUE,
                                           country.B3info.file = country.B3info.file,
                                           data.file = data.all.file,
                                           hiv.file = hiv.file,
                                           adj.file = adj.file,
                                           isos.to.exclude.for.global.run = isos.to.exclude.for.global.run, # change JR, 20140501
                                           settings = settings,
                                           print.messages = FALSE)
  ### deprecated as of 5 Jul 2013
  # if (fit.B2.model) {
  #   data <- ProcessDataToSetAllDataAsVR(data = data, hiv.file = hiv.file, adj.file = adj.file)
  #   cat("All data set as VR data to run B2 model.\n")
  # }
  cat("Database read in.\n")
  # save(data, file = file.path(output.dir, "data.rda")) ### adhoc change JR for testing
  # save(settings, file = file.path(output.dir, "settings.rda")) ### adhoc change JR for testing
  #----------------------------------------------------------------------
  # JAGS components
  if (!run.for.IMR.MLT) {
    # data
    jags.data.temp <- GetJAGSDataAll(data = data,
                                     data.val = data.val,
                                     data.global = data.global,
                                     hiv.file = hiv.file,
                                     adj.file = adj.file,
                                     settings = settings)
    jags.data <- jags.data.temp$jags.data
    jags.data.for.inits <- jags.data.temp$jags.data.for.inits
    data <- jags.data.temp$data
    cat(paste0("JAGS data obtained.", "\n"))
    # parameter names # change JR, 20140507
    jags.par.temp <- GetJAGSParameters(jags.data = jags.data,
                                       data.val = data.val,
                                       settings = settings)
    jags.par.all <- jags.par.temp$jags.par.all
    jags.par <- jags.par.temp$jags.par
    cat(paste0("JAGS parameter names obtained.", "\n"))
    # inits (used for checking only; will differ from actual inits used in JAGS)
    jags.inits <- GetJAGSInits(data = data,
                               jags.data = jags.data,
                               jags.data.for.inits = jags.data.for.inits,
                               settings = settings)
    cat(paste0("JAGS inits obtained.", "\n"))
  } else {
    jags.data <- jags.data.for.inits <- jags.par.all <- jags.par <- jags.inits <- NULL
  }
  ##details<<
  ##describe<< Object mcmc.meta is saved, which is a list with
  mcmc.meta <- list(
    general = ##<< General information:
      ##describe<<
      list(chain.ids = chain.ids, ##<< From arguments
           nchains = length(chain.ids), ##<< Number of chains
           nsteps = nsteps, ##<< From arguments
           nthin = nthin, ##<< From arguments
           nburnin = nburnin, ##<< From arguments
           niterperstep = niterperstep, ##<< From arguments
           output.dir = output.dir, ##<< From arguments
           seed.MCMC = seed.MCMC ##<< From arguments
      ),
    ##end<<
    settings = settings, ##<< List of settings
    files = files, ##<< List of file paths
    data = data, ##<< Object from \code{\link{ReadData}}
    data.hivremoved = data.hivremoved, ##<< Object from \code{\link{ReadData}}
    data.logscale.hivremoved = data.logscale.hivremoved, ##<< Object from \code{\link{ReadData}}
    data.all = data.all, ##<< Object from \code{\link{ReadData}}
    data.hivremoved.all = data.hivremoved.all, ##<< Object from \code{\link{ReadData}}
    data.logscale.hivremoved.all = data.logscale.hivremoved.all, ##<< Object from \code{\link{ReadData}}
    data.val = data.val, ##<< Object from \code{\link{GetValidationData}}
    data.global = data.global, ##<< Object from \code{\link{SummariseGlobalRun}}
    jags.data = jags.data, ##<< Object from \code{\link{GetJAGSDataAll}}
    jags.par.all = jags.par.all, ##<< Output from \code{\link{GetJAGSParameters}}, list with all parameter names that are saved in JAGS
    jags.par = jags.par, ##<< Output from \code{\link{GetJAGSParameters}}, list with all parameter names that are sampled in JAGS
    jags.data.for.inits = jags.data.for.inits, ##<< Output from \code{\link{GetJAGSDataAll}
    jags.inits = jags.inits, ##<< Object from \code{\link{GetJAGSInits}}
    val.info = val.info ##<< Details about the validation exercise
  )
  ##end<<
  save(mcmc.meta, file = file.path(output.dir, "mcmc.meta.rda"))
  cat(paste0("mcmc.meta saved.", "\n"))
  
  if (!run.jags | run.for.IMR.MLT) # end here if run.jags = FALSE # change JR, 20 May
    return(invisible()) 
  
  #Details<< JAGS model is stored in \code{output.dir} using \code{WriteModel}
  WriteModel(mcmc.meta = mcmc.meta, output.dir = output.dir)
  cat(paste0("Model written.", "\n"))
  #file.show(file.path(output.dir, "model.txt"))
  cat(paste0("MCMC run started. ", "\n"))
  sink()
  closeAllConnections()
  
  if (run.on.server) {
    # registerDoMC()
    registerDoMC(cores = detectCores()) # change JR, 20140509
    print(paste0("Running in parallel? ",
                 ifelse(getDoParWorkers() == 1,  
                        "No.", paste0("Yes, with ", getDoParWorkers(), " cores."))))
    foreach (chain.id = chain.ids) %dopar% {
      cat(paste0("Start chain ID ", chain.id, ".\n"))
      InternalRunOneChain(chain.id = chain.id, mcmc.meta = mcmc.meta)
    } # end chain.ids
  } else {
    for (chain.id in chain.ids) {
      cat(paste0("Start chain ID ", chain.id, ".\n"))
      InternalRunOneChain(chain.id = chain.id, mcmc.meta = mcmc.meta)
    }
  }
  cat("All chains have finished!\n")
  cat("---------- End RunMCMC ----------\n")
  ##value<< NULL (mcmc.meta is saved to output.dir, and JAGS objects are saved in their own directory).
  return(invisible())
} # end RunMCMC function

#----------------------------------------------------------------------
InternalRunOneChain <- function(# Do MCMC sampling
  ## Do MCMC sampling for one chain
  chain.id, ##<< Chain ID
  mcmc.meta ##<< List, described in \code{\link{RunMCMC}}
) {
  # set seed before sampling the initial values
  set.seed.chain <- chain.id*mcmc.meta$general$seed.MCMC*min(as.numeric(mcmc.meta$data$uncode.c), na.rm = T) # change JR, 20140508
  # in JAGS version (July 21, 2012) jags.seed doesn't work, and inits need to be provided as a function
  # note that even with same JAGS seed, as long as inits from R are different, any non-initialized pars will have different starting values
  # and seed in JAGS is consistent
  mcmc.info <- list(set.seed.chain = set.seed.chain, chain.id = chain.id)
  mcmc.info.file <- file.path(mcmc.meta$general$output.dir, paste0("mcmc.info.", chain.id, ".rda"))
  if (file.exists(mcmc.info.file)) {
    cat(paste0("The output directory ", mcmc.meta$general$output.dir, " already contains info on chain ", chain.id, ".\n"))
    cat(paste("No new samples are added.\n"))
    return(invisible())
  }
  save(mcmc.info, file = mcmc.info.file)
  cat("JAGS is called to obtain posterior samples, and there will be some info about the model and steps written to file.", "\n")
  cat("Just wait for statement that MCMC run has finished", "\n")    
  jags.dir <- file.path(mcmc.meta$general$output.dir, "temp.JAGSobjects/")
  set.seed(set.seed.chain) # note: seed only useful if inits function didn't change!
  # need to sample something in R first
  temp <- rnorm(1)
  GetJAGSInitsWrapper <- function() {
    return(
      # c(
        GetJAGSInits(data = mcmc.meta$data,
                     jags.data = mcmc.meta$jags.data,
                     jags.data.for.inits = mcmc.meta$jags.data.for.inits,
                     settings = mcmc.meta$settings)
        # , list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = set.seed.chain)) # change JR, 20140627
    )
  }
  mod <- jags(data = mcmc.meta$jags.data, 
              inits = GetJAGSInitsWrapper,
              parameters.to.save = unlist(mcmc.meta$jags.par.all),
              model.file = file.path(mcmc.meta$general$output.dir, "model.txt"),
              n.chains = 1, # change JR, 20140626
              n.iter = mcmc.meta$general$niterperstep + mcmc.meta$general$nburnin, 
              n.burnin = mcmc.meta$general$nburnin, 
              n.thin = mcmc.meta$general$nthin,
              jags.seed = set.seed.chain, # 123, # change JR, 20140521: country- and chain-specific seed set
              # debug = FALSE,
              working.directory = mcmc.meta$general$output.dir)
  i = 1 # index for which update
  mod.upd <- mod
  save(mod.upd, file = paste0(mcmc.meta$general$output.dir, "/temp.JAGSobjects/jags_mod", chain.id, "update_", i, ".Rdata"))
  cat(paste0("MCMC results step ", 1, " for chain ", chain.id, " written to folder temp.JAGSobjects in ", mcmc.meta$general$output.dir, "."), "\n")
  
  #----- update MCMC -----#
  if (mcmc.meta$general$nsteps > 1) {
    for (i in 2:(mcmc.meta$general$nsteps)) {
      mod.upd <- update(mod.upd, parameters.to.save = unlist(mcmc.meta$jags.par.all), 
                        n.iter = mcmc.meta$general$niterperstep, 
                        n.thin = mcmc.meta$general$nthin)
      save(mod.upd, file = paste0(mcmc.meta$general$output.dir, "/temp.JAGSobjects/jags_mod", chain.id, "update_", i, ".Rdata"))
      cat(paste0("MCMC results step ", i, " for chain ", chain.id, " written to folder temp.JAGSobjects in ", mcmc.meta$general$output.dir, "."), "\n")
    }
  }  
  cat(paste0("Hoorah, chain ", chain.id, " has finished!"), "\n")
  ##note<< Called from \code{\link{RunMCMC}}.
  ## This function can give errors and warnings when JAGS output is read into R,
  ## no worries about that here, convergence will be checked later.
  ##value<< NULL
  return(invisible())
}
