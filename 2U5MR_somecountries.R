#----------------------------------------------------------------------
# 2U5MR_somecountries.R
# Leontine Alkema & Jin Rou New, 2012-2015
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# NOTES
#----------------------------------------------------------------------
# NOTES: 
## Check that the right database (called "data_U5MR_CMEInfo.csv") is in input folder!

#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- FALSE # Indicate if run is on the server
get.notifications <- FALSE # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
output.run.times <- FALSE # Output run times of each country-specific run?
workdir <- "C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code" # Give work directory file path if not running things on server

# Define working directory
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
  library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost("note", "Error!", geterrmessage(), recipients = 2)#c(1, 2))
  })
}
# There will be a lot of output, to ignore as long as it does not contain R errors
#----------------------------------------------------------------------
# 3. Get all required components and start MCMC country-specific run
#    for all countries
#----------------------------------------------------------------------
runname.global <- "GR20180210"
# File path of .CSV file in CME Info database format for one country if default database is not used
data.cmeinfo.file <- "input/data_U5MR_20180716.csv"
year.current <- 2030.5
is.validation <- FALSE
info <- read.csv("input/infoUNinclHIV.csv", header = T, stringsAsFactors = F)
# iso.all <- info$iso.c[info$iso.c != "LIE" & info$method.c == "B3"]
iso.all <- c("KEN")
# iso.all <- c("DZA", "ROU", "BLR", "BGR",
#              "ZWE", "BWA", "CMR", "CAF", "CIV", "GAB", "KEN", "LSO", 
#              "MWI", "MOZ", "NAM", "RWA", "ZAF", "SWZ", "UGA", "TZA", "ZMB")
C.all <- length(iso.all)
print(C.all)

nruns <- 0
# Start a file to output run times for each country
if (output.run.times)
  cat(paste("ISO", paste(names(proc.time()), collapse = ","), sep = ","), 
      file = file.path("output", paste0(runname.global, "_onecountryruntimes",
                                        ifelse(is.validation, "val", ""), "_", Sys.Date(), ".txt")), 
      fill = T, append = F)
if (run.on.server) {
  registerDoMC(cores = detectCores())
  print(paste0("Running in parallel? ",
               ifelse(getDoParWorkers() == 1,  
                      "No.", paste0("Yes, with ", getDoParWorkers(), " cores."))))
}
# foreach(c.all=1:C.all) %dopar% {
for (c.all in 1:C.all) {
  iso.select <- iso.all[c.all]
  runname <- paste0(runname.global, "_", iso.select, "_TestAdjustmentsLocal") ##<< Change runname
  data.cmeinfo <- read.csv(file = data.cmeinfo.file, 
                           header = T, stringsAsFactors = F, strip.white = F, encoding = "latin1")
  use.one.chain <- any(!is.na(data.cmeinfo$Set.As.Minimum[data.cmeinfo$Country.Code == iso.select]) &
                         data.cmeinfo$Set.As.Minimum[data.cmeinfo$Country.Code == iso.select] == 1)
  if (!file.exists(file.path("output", runname, "mcmc.meta.rda"))) {
    # settings for tweaks
    periods.smooth.list <- NULL
    # periods.smooth.list <- list(c(yyyy1,yyyy2))
    periods.unsmooth.list <- NULL 
    # periods.unsmooth.list <- list(c(yyyy1,yyyy2))
    if (iso.select == "SOM") {
      periods.constant.list <- list(c(1991.5, 2008.5))
    } else {
      periods.constant.list <- NULL
    }
    if (iso.select == "YEM"){ # add by YS
      special.constant.list <- list (c(2011.5,2030.5))
    } else if (iso.select == "SYR") {
      special.constant.list <- list(c(2010.5, 2030.5))
    # } else if (iso.select == "VEN") { # add by DJS 2018-03-16
    #   special.constant.list <- list(c(2015.5, 2030.5))
    } else if (iso.select == "SSD") { # add by DJS 2018-07-05
      special.constant.list <- list(c(2013.5, 2030.5))
    } else {
      special.constant.list <- NULL
    }
    # End of user input
    #----------------------------------------------------------------------
    # NOTE: "tweaks" will be combined with country+global run info to give runname (output folder name)
    # you can change the "tweaks" name or runname if several tweaks are tried with nonunique names
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
    RunMCMC(runname = runname,
            # test:
            # nsteps = 1, chain.ids = c(1,2), nthin = 1, nburnin = 5, niterperstep = 10,
            nsteps = nsteps, chain.ids = chain.ids,
            iso.select = iso.select,
            run.type = "country",
            run.on.server = run.on.server,
            runname.global = runname.global,
            data.cmeinfo.file = data.cmeinfo.file,
            year.current = year.current,
            year.lastestimatepublished = year.current, ### Typically year.current-1
            year.lastestimate = year.current,
            periods.unsmooth.list = periods.unsmooth.list,
            periods.smooth.list = periods.smooth.list,
            periods.constant.list = periods.constant.list)
    if (output.run.times)
      cat(paste(iso.select, paste(proc.time(), collapse = ","), sep = ","), 
          file = file.path("output", paste0(runname.global, "_onecountryruntimes",
                                            ifelse(is.validation, "val", ""), "_", Sys.Date(), ".txt")),
          fill = T, append = T)
    closeAllConnections()
    #----------------------------------------------------------------------
    # 4. Read MCMC output and construct mcmc.array
    #----------------------------------------------------------------------
    ReadMCMCOutput(runname = runname)
    #----------------------------------------------------------------------
    # 5. Construct output and output estimates in results.csv
    #----------------------------------------------------------------------
    ConstructOutput(runname = runname)
    #----------------------------------------------------------------------
    # 6. Make plots
    #----------------------------------------------------------------------
    #PlotResults(runname = runname)
    #----------------------------------------------------------------------
    # 7. Do a comparison plot with another run
    #----------------------------------------------------------------------
    PlotComparison(runname1 = runname, 
                   runname2 = "GR20180210_all",
                   runname3 = "GR20170401_all",
                  legend1 = "test adjustments.R",
                  legend2 = "UN IGME 2018", 
                  legend3 = "UN IGME 2017",
                  year.end = 2017.5,
                  plot.igme = F)
    #----------------------------------------------------------------------
    # 8. Check convergence
    #----------------------------------------------------------------------
    #CheckConvergence(runname = runname, check.convergence = FALSE)
  }
  nruns <- nruns + 1
  if (get.notifications) {
    if (nruns %% 10 == 0) {
      pbPost(type = "note", 
             title = paste0("2U5MR_somecountries.R"), 
             body = paste0(nruns, " U5MR CS runs done!"),
             recipients = c(1, 2))
    }
  }
}
if (get.notifications)
  pbPost(type = "note", 
         title = paste0("2U5MR_somecountries.R"), 
         body = paste0("U5MR CS runs done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# Fin.
