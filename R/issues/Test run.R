rm(list = ls()) # Clear workspace
run.on.server <- F # Indicate if run is on the server
get.notifications <- F # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
workdir <- "C:/Users/lhug/Dropbox/IGME Data/2018 Round Estimation/Code" # Give work directory file path if not running things on server
if (run.on.server) {
  package.dir <- workdir <- getwd()
} else {
  package.dir <- workdir
}
setwd(workdir)





#########################################step 1######################################################
##############################input the arguments from runmcmc.R#####################################
#####################################################################################################
iso.select = NULL ##<< If \code{NULL}, a global run will be initiated. 
isos.to.exclude.for.global.run = c("LIE", "PRK", "SPS", "MCO") ##<< Vector of 3-character ISO country codes
## of country data to exclude for global run. Default: Liechtenstein, Korea DPR & Sudan pre-secession (as estimates 
## are no longer produced with B3 for these countries); Andorra & Monaco (as neighbouring regions' data are used)
output.dir = NULL ##<< Directory where mcmc.meta and raw MCMC output will be stored
## either in an existing directory, or if \code{NULL}, directory \code{output/runname} is created
## in current working directory
seed.MCMC = 1 ##<< Seed for initializing MCMC, defaults to 1.
chain.ids = seq(1, 6) ##<< IDs of chains to run in series 
## (the IDs need to be numeric because they are used to set the seed).
data.cmeinfo.file = NULL ##<< File path of data from CME Info. If \code{NULL}, child mortality data included in package is used. 
data.file = NULL ##<< If \code{NULL}, data processed from data.cmeinfo.file is used. 
data.all.file = NULL ##<< If \code{NULL}, excluded data processed from data.cmeinfo.file is used.
country.info.file = NULL ##<< If \code{NULL}, country info included in package is used.
country.B3info.file = NULL ##<< If \code{NULL}, country B3 info included in package is used. 
hiv.file = NULL ##<< If \code{NULL}, UNAIDS HIV mortality adjustment data included in package is used. 
adj.file = NULL ##<< If \code{NULL}, WHO crisis mortality adjustment data included in package is used. 
livebirths.file = NULL ##<< If \code{NULL}, WPP live birth data included in package is used.
run.on.server = FALSE ##<< Logical value indicating whether or not to run on the server.
run.jags = TRUE ##<< Logical values indicating whether or not to run JAGS.
run.for.IMR.MLT = FALSE ##<< Logical values indicating whether or not to run to get data only for IMR MLT estimates.
indicator.type = "U5MR" ##<< Takes two possible values, "U5MR" or "IMR" for desired indicator to estimate.
year.current = 2017.5 ##<< Current year. No series date can be after this date.
year.lastestimatepublished = year.current-1 ##<< Year of last estimate published.
year.lastestimate = 2016.5 ##<< Year of last estimate.
runname.U5MR = NULL ##<< Character indicating runname of B3 U5MR run required for estimating IMR. 
## If \code{NULL}, B3 U5MR estimates included in package is used.
runname.igme = paste0("IGME", floor(year.current)-1)  ##<< Runname of UN IGME run of the previous year. The folder
## \code{output/runname.igme} should contain the U5MR estimates in an \code{res.U5MR.rda} file.
se.censusindirect.missing = 0.025 ##<< Inputed value of relative non-VR SE for Census Indirect observations if missing (NA). Do not change.
se.othernonvr.missing = 0.1 ##<< Inputed value of relative non-VR SE for all other non-VR observations (not Census Indirect) if missing (NA). Do not change.
se.vr.min = 0.025 ##<< Minimum relative VR SE (no relative VR SE is lower than this value). Do not change.
se.vr.missing = 0.1 ##<< Inputed value of relative VR SE if missing (NA). Do not change.
recall.mid = 10 ##<< Mean of retrospective period (set at 10 years), used to centre retrospective period variable in model # change JR, 20140501
dhsdirect.prior.mu.mubeta1 = ifelse(indicator.type == "U5MR", -0.0123, -0.0259900303333333) ##<< Prior value for the mean DHS Direct bias level (based on UN IGME 2012). Do not change.
dhsdirect.prior.sigma.mubeta1 = ifelse(indicator.type == "U5MR", 0.00555581516674627, 0.0071070833850106) ##<< Prior value for the sd of DHS Direct bias level (based on UN IGME 2012) Do not change.
is.validation = FALSE ##<< Logical value indicating whether or not to do a validation run.
year.cutoff = 2006 ##<< Used only if \code{is.validation} = \code{TRUE}):
## All data with series year after and IN year.cutoff are excluded.
fit.B2.model = FALSE ##<< Logical value indicating whether or not to fit B2 model (without data model).
input.vr.se = TRUE ##<< Logical value indicating whether or not to read in/input VR SE or 
## estimate country-specific VR SE. Set to \code{FALSE} to fit B2 model.
include.HIV.countries = TRUE ##<< Logical value indicating whether or not to include HIV countries in global run.
apply.rules = TRUE ##<< Logical value indicating whether to apply global smoothing or 
## to fix series level bias for early series for countries that fall under the set rules for doing so.
add.dhsdirect.bias = FALSE ##<< Logical value indicating whether or not to add DHS Direct bias.
set.dhsdirect.prior = TRUE ##<< Logical value indicating whether or not to set posterior median estimates
## from multilevel model for DHS Direct prior.
use.constant.sigma.u = FALSE ##<< Logical value indicating whether or not sigma.u is constant in a country.
use.country.variance.multipliers = FALSE ##<< Logical value indicating whether or not country variance 
## multipliers should be used.
run.type = "country" ##<< Takes the values "global" (global run), "country" (country-specific run) or
## "combined" (combined run).
runname.global = NULL##<< Character specifying runname of global run. If \code{NULL},
## global run results included in package are used. 
I = 2.5##<< Interval length between two knots during observation period.
periods.unsmooth.list = NULL ##<< (Country tweak) List of vectors of length 2 with start and end years of periods to unsmooth spline fit.
periods.smooth.list = NULL ##<< (Country tweak) List of vectors of length 2 with start and end years of periods to smooth spline fit.
periods.constant.list = NULL ##<< (Country tweak) List of vectors of length 2 with start and end years of periods without decline/with constant fit. # change JR, 24 Jun
get.jags.predictions = FALSE ##<< Logical value indicating whether or not to get predictions directly from JAGS. ### not working
global.gamma.median = NULL ##<< Bayesian melding global gamma median parameter.
global.gamma.sd = NULL ##<< Bayesian melding global gamma sd parameter.
special.constant.list = NULL
#######################################################################################################




#########################################step 2######################################################
#########################input the arguments from 2U5MR_onecountry.R#################################
#####################################################################################################
if (run.on.server) {
  package.dir <- workdir <- getwd()
} else {
  package.dir <- workdir
}
setwd(workdir)
#----------------------------------------------------------------------
# 2. Load libraries and codes 
#----------------------------------------------------------------------
source(file.path(package.dir, "R/loadlibrariesandcodes.R"))
# NOTE: if you run this for the first time, use do.install = TRUE to install all packages needed 
# LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir, do.install = TRUE)

LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir)
if (get.notifications) {
  #library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost("note", "Error!", geterrmessage(), recipients = 2)#c(1, 2))
  })
}
# There will be a lot of output, to ignore as long as it does not contain R erro
runname.global <- "GR20180210" ##<< Global run to use. Do not change



#######################################Use San Marino data###########################################
data.cmeinfo.file <- "input/data_U5MR_SMR.csv" ##<< File path of .CSV file in CME Info database format for one country if default database is not used
iso.select <- "SMR" ##<< 3-character ISO country code of country to run for
#####################################################################################################
runname <- paste0(runname.global, "_", iso.select) ##<< Run name
runname2 <- "GR20170401_all"# "IGME2014" ##<< Run name of another run for comparison, use "IGME2013" to compare with previous year's run
year.current <- 2016.5

# if (run.on.server) {
#   registerDoMC(cores = detectCores())
#   print(paste0("Running in parallel? ",
#                ifelse(getDoParWorkers() == 1,  
#                       "No.", paste0("Yes, with ", getDoParWorkers(), " cores."))))
# }

  data.cmeinfo <- read.csv(file = data.cmeinfo.file, 
                           header = T, stringsAsFactors = F, strip.white = F, encoding = "latin1")
  use.one.chain <- any(!is.na(data.cmeinfo$Set.As.Minimum[data.cmeinfo$Country.Code == iso.select]) &
                         data.cmeinfo$Set.As.Minimum[data.cmeinfo$Country.Code == iso.select] == 1)
  # settings for tweaks
  periods.smooth.list <- NULL
  # periods.smooth.list <- list(c(yyyy1,yyyy2))
  periods.unsmooth.list <- NULL 
  #periods.unsmooth.list <- list(c(yyyy1,yyyy2))
  if (iso.select == "SOM") {
    periods.constant.list <- list(c(1991.5, 2008.5))
  } else {
    periods.constant.list <- NULL
  }
  if (iso.select == "YEM"){ # add by YS
    special.constant.list <- list (c(2011.5,2030.5))
  } else if (iso.select == "SYR") {
    special.constant.list <- list(c(2010.5, 2030.5))
  } else if (iso.select == "VEN") { # add by DJS 2018-03-16
    special.constant.list <- list(c(2015.5, 2030.5))
  } else {
    special.constant.list <- NULL
  }
  # End of user input
  #----------------------------------------------------------------------
  tweaks <- paste0(ifelse(!is.null(data.cmeinfo.file), data.cmeinfo.file, ""),
                   ifelse(!is.null(periods.unsmooth.list), "_unsmooth", ""),
                   ifelse(!is.null(periods.smooth.list), "_smooth", ""))
  print(paste(runname, ":", tweaks))
  if (use.one.chain) {
    nsteps <- 32 # 30
    chain.ids <- 1
  } else {
    nsteps <- 4 # 5
    chain.ids <- 1:8 # 1:6
  }



  
  
  
##########################################Step 3##########################################
############################Use the settings in RunMCMC function##########################
runname = runname
  # test:
  #nsteps = 1, chain.ids = c(1,2), nthin = 1, nburnin = 5, niterperstep = 10,
nsteps = nsteps
chain.ids = chain.ids
iso.select = iso.select
  
##### use country run ########
run.type = "country"
run.on.server = run.on.server
runname.global = runname.global
data.cmeinfo.file = data.cmeinfo.file
year.current = year.current
year.lastestimatepublished = year.current ### Typically year.current-1
year.lastestimate = year.current
periods.unsmooth.list = periods.unsmooth.list
periods.smooth.list = periods.smooth.list
periods.constant.list = periods.constant.list
special.constant.list = special.constant.list
########################################################################################






##########################################Step 4######################################################
############################Generate 'data' argument from RunMCMC.R###################################
#######################the following code is copied from Line 76 to 412###############################

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

data.cmeinfo.temp <- read.csv(data.cmeinfo.file, header = T, stringsAsFactors = F, strip.white = T, encoding = "latin1")
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
                       adj.file = adj.file,
                       runname = runname, iso.select = iso.select, output.dir = output.dir,
                       indicator.type = indicator.type, runname.U5MR = runname.U5MR,
                       is.validation = is.validation, fit.B2.model = fit.B2.model,
                       year.current = year.current)
# get data including excluded observations for plotting
CleanDataFromCMEInfo(data.cmeinfo.file = data.processed.cmeinfo.file,
                     country.B3info.file = country.B3info.file,
                     hiv.file = hiv.file,
                     adj.file = adj.file,
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
                 global.gamma.sd = global.gamma.sd,# ##<< From \code{data.global$global.gamma.sd} if \code{run.type} is "country", else \code{NULL} 
                 special.constant.list = special.constant.list) # by YS
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


################################################'data' argument now has been created##################################











###################################################Step 5#############################################
#######################Run Getjagssubfuntions.R to get lme regression result##########################
#######################################From Line 5 to Line 136########################################

########Arguments part########
data
data.val
indicator.type
runname.U5MR

run.type
year.lastestimate
periods.unsmooth.list = NULL
periods.smooth.list = NULL
periods.constant.list = NULL
hiv.file
adj.file
runname.igme
##<< Runname of UN IGME run of the previous year, 
## with \code{output/runname.igme} containing U5MR estimates stored as a \code{res.U5MR.rda}.
mean.b0 = ifelse(indicator.type == "U5MR", 4.02, 2.19) ##<< Mean of b0 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.b1 = ifelse(indicator.type == "U5MR", -0.0889, 0.0516) ##<< Mean of b1 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.u = ifelse(indicator.type == "U5MR", -0.000446, 0.0271) ##<< Mean of u estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.var.b0 = ifelse(indicator.type == "U5MR", 0.00129, 0.490) ##<< Mean variance of b0 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.var.b1 = ifelse(indicator.type == "U5MR", 0.000110, 0.0342) ##<< Mean variance of b1 estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.
mean.sigma.u = ifelse(indicator.type == "U5MR", 0.0424, 0.206) ##<< Mean sd of u estimates from lmer across countries for countries where lmer was fitted; the inputed value for countries where lmer could not be fitted.

##############lme previous part############
year.t <- seq(floor(min(ifelse(is.element("RUS", data$iso.c), 1971, 1986), 
                        data$minyear.c, na.rm = T))-0.5, year.lastestimate) # last year needs to be a future projection!
# to make sure first and last observation year is within year.t for all countries
# use max length of observation period to find max number of splines
maxobsperiod <- max(data$maxyear.c - min(data$minyear.c, year.t, na.rm = T), na.rm = T) 
# find max number of splines
# add 2 at start and 2+1 at end, so +5 in total
# +1 so that Q > qplus1.c (for write_model.R)
K <- ceiling(maxobsperiod/I)+5+1
M <- 2  # number of b's (difference degree, also called d)
Q <- K - M
# make dimensions equal to C+1 to avoid problems when running it for one country
year.ci <- y.ci <- q5hat.ci <- y.inits.ci <- matrix(NA, data$C+1, data$nmax)
BG.cim <- array(NA, c(data$C+1, data$nmax, M))
Z.ciq <- array(0, c(data$C+1, data$nmax, Q))
q.c <- k.c <- sigma0.u.c <- rep(NA, data$C+1)
u0.cq <- matrix(NA, data$C+1, Q)
b0.cm <- matrix(NA, data$C+1, 2)
Sigma0.b.Lc <- T0.b.Lc <- replicate(data$C+1, matrix(NA, 2, 2), simplify = FALSE)
u.cq <- matrix(NA, data$C+1, Q)
b.cm <- matrix(NA, data$C+1, 2)
IMR_larger_than_U5MR <- vector(length = data$C) # indicating if empirical IMR is larger than estimated U5MR

############Changes made here############
################data$C==1################
#######for (c in 1:data$C) {#############
c=1         #####
  year.i <- c(unlist(data$year.Lcs.j[[c]]), unlist(data$yearvr.Lc.j[[c]]))
  year.min <- floor(min(ifelse(data$iso.c[c] == "RUS", 1971, 1986), 
                        data$minyear.c[c], na.rm = T))-0.5 
  year.ci[c, 1:data$n.c[c]] <- year.i
  u.i <- c(unlist(data$u.Lcs.j[[c]]), unlist(data$uvr.Lc.j[[c]]))
  includeIncompleteVRAny <- any(data$isincompletevr.Lc.j[[c]] == 1)
  if (includeIncompleteVRAny) {
    # for incomplete VR data, get inits data based on max of previous year's estimates and 1.1*obs
    u.igme.i <- Getq5Estimates(iso = data$iso.c[c], years = year.i, runname = runname.igme) 
    is.vrincompleteany.i <- c(rep(FALSE, data$nnonvr.c[c]), data$isincompletevr.Lc.j[[c]] == 1) # note: (data$isincompletevr.Lc.j[[c]] == 1) is NULL if all are non-VR data
    u.inits.i <- ifelse(is.vrincompleteany.i, mapply(max, u.igme.i, u.i*1.1), u.igme.i)
  }
  # get crisis-free data for crisisadjfordata.c countries
  if (data$crisisadjfordata.c[c]) {
    u.i <- GetCrisisSubtractedSeries(u.i = u.i, year.i = year.i, iso = data$iso.c[c],
                                     adj.file = adj.file)
    if (includeIncompleteVRAny)
      u.inits.i <- GetCrisisSubtractedSeries(u.i = u.inits.i, year.i = year.i, iso = data$iso.c[c],
                                             adj.file = adj.file)
  }
  if (any(u.i <= 0))
    stop(paste0("Zero or negative observation (u.i) found for ", data$name.c[c], 
                " (, ", data$iso.c[c], ")"))
  # get HIV-free data for HIV countries
  if (data$hiv.c[c]) {
    y.i <- log(GetHIVSubtractedSeries(u.i = u.i, year.i = year.i, iso = data$iso.c[c],
                                      hiv.file = hiv.file))
    if (includeIncompleteVRAny)
      y.inits.i <- log(GetHIVSubtractedSeries(u.i = u.inits.i, year.i = year.i, iso = data$iso.c[c],
                                              hiv.file = hiv.file))
  } else {
    y.i <- log(u.i)
    if (includeIncompleteVRAny)
      y.inits.i <- log(u.inits.i)
  }
  y.ci[c, 1:data$n.c[c]] <- y.i
  if (includeIncompleteVRAny)
    y.inits.ci[c, 1:data$n.c[c]] <- y.inits.i
  if (indicator.type == "IMR") {
    q5hat.i <- Getq5Estimates(iso = data$iso.c[c], years = year.i, runname = runname.U5MR)
    # get HIV-free q5 estimates for HIV countries
    if (data$hiv.c[c])
      q5hat.i <- GetHIVSubtractedSeries(u.i = q5hat.i, year.i = year.i, iso = data$iso.c[c], 
                                        hiv.file = hiv.file)
    q5hat.ci[c, 1:data$n.c[c]] <- q5hat.i 
    l.i <- logit(ifelse(exp(y.i) < q5hat.i, exp(y.i)/q5hat.i, 0.9999999999)) # logit cannot take value of >= 1!
    
    IMR_larger_than_U5MR [c] = ifelse(any(exp(y.i) > q5hat.i),TRUE,FALSE) # add by YS
  } else {
    IMR_larger_than_U5MR [c] = FALSE}
  res <- GetSplines(years.t = c(year.min, year.i), I = I,
                    years.combine = periods.constant.list) 
  ktemp <- length(res$alphayears.k)
  B.ik <- res$B.tk[-1, ] # change JR, 17 Jun: remove the first (extra) column corresponding to year.min
  ## note: year.min was added to years.t vector in GetSplines() so that start year of splines would be at least
  ## 1970.5 for Russia or 1990.5 otherwise.
  D2 <- diff(diag(ktemp), diff = 2)
  G <- cbind(rep(1, ktemp), seq(1, ktemp)-ktemp/2) # centre second column of G
  Dcomb <- t(D2)%*%solve(D2%*%t(D2))
  Z.iq <- B.ik%*%Dcomb
  q.c[c] <- dim(Z.iq)[2]
  Z.ciq[c, 1:data$n.c[c], 1:q.c[c]] <- Z.iq
  BG.im <- B.ik%*%G  
  BG.cim[c, 1:data$n.c[c],] <- BG.im
  
  
  
  one <- rep(1, nrow(Z.iq))
  
  
  
#################################lme part##################################
  if (length(y.i) > 2) {
    if (indicator.type == "U5MR") {
      if (!includeIncompleteVRAny) {
        mod <- lme(fixed = y.i ~ -1 + BG.im, random = list(one = pdIdent(~ Z.iq - 1)),control = lmeControl(opt = "optim"))
      } else {
        mod <- lme(fixed = y.inits.i ~ -1 + BG.im, random = list(one = pdIdent(~ Z.iq - 1)))
      }
    } else if (indicator.type == "IMR") {
      mod <- lme(fixed = l.i ~ -1 + BG.im, random = list(one = pdBlocked(pdIdent(~ Z.iq - 1))))
    }
    u.q <- summary(mod)$coefficients$random$one
    b.m <- summary(mod)$coefficients$fixed  
    u.cq[c, 1:length(u.q)] <- u.q
    b.cm[c, ] <- b.m
  } else {
    # change JR, 3 Apr 2013: error with lme if less than 3 observations available,
    # so set as mean of values from all other countries
    u.q <- rep(mean.u, q.c[c]) # -0.001080 
    b.m <- c(mean.b0, mean.b1) # c(3.939, -0.09051)
  }
###############################length(y.i)==2################################
  
  
  lme(fixed = y.i ~ -1 + BG.im, random = list(one = pdIdent(~ Z.iq - 1)))
  
