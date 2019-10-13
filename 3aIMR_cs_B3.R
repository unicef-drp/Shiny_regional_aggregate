#----------------------------------------------------------------------
# 3aIMR_cc.R
# Leontine Alkema & Jin Rou New, 2012-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- FALSE # Indicate if run is on the server
get.notifications <- FALSE # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
output.run.times <- FALSE # Output run times of each country-specific run?
workdir <- "C:/Users/lhug/Dropbox/IGME Data/2019 Round Estimation/Code" # Give work directory file path if not running things on server

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
runname.global <- "IGME2013IMR"
runname.countryspecific.prefix <- "IMR20190218"
runname.U5MR <- "GR20190218_all"
# File path of .CSV file in CME Info database format for one country if default database is not used
data.cmeinfo.file <- "input/data_IMR_20190228.csv"
is.validation <- FALSE
year.current <- 2030.5
info <- read.csv("input/infoUNinclHIV.csv", header = T, stringsAsFactors = F)
iso.all <- info$iso.c[info$iso.c != "LIE" & info$imrmethod.c == "B3"]

### To update
# iso.all2 <- c("BOL", "BDI", "CHN", "COD", "EGY", "GEO", "GNB", "MDG", "MMR", "MRT", "NGA", "NPL", "SLE",
#               "ATG", "TUR",
#               "BWA", "CMR", "CAF", "CIV", # "GAB", "MOZ",
#               "KEN", "LSO", "MWI", "NAM", "RWA", "ZAF", "SWZ", "UGA", "TZA", "ZMB", "ZWE")
iso.all2='AND'
iso.all <- intersect(iso.all, iso.all2)

C.all <- length(iso.all)
print(C.all)

nruns <- 0
# Start a file to output run times for each country
if (output.run.times)
  cat(paste("ISO", paste(names(proc.time()), collapse = ","), sep = ","), 
      file = file.path("output", paste0(runname.countryspecific.prefix, "_onecountryruntimes",
                                        ifelse(is.validation, "val", ""), ".txt")), 
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
  runname <- paste0(runname.countryspecific.prefix, "_", iso.select)
  if (!file.exists(file.path("output", runname, "mcmc.meta.rda"))) {
    # settings for tweaks
    periods.smooth.list <- NULL
    # periods.smooth.list <- list(c(yyyy1,yyyy2))
    periods.unsmooth.list <- NULL 
    #periods.unsmooth.list <- list(c(yyyy1,yyyy2))
    periods.constant.list <- NULL
    # End of user input
    #----------------------------------------------------------------------
    tweaks <- paste0(ifelse(!is.null(data.cmeinfo.file), data.cmeinfo.file, ""),
                     ifelse(!is.null(periods.unsmooth.list), "_unsmooth", ""),
                     ifelse(!is.null(periods.smooth.list), "_smooth", ""))
    print(paste(runname, ":", tweaks))
    nsteps <- 4 # 5
    chain.ids <- 1:8 # 1:6
    RunMCMC(runname = runname,
            # test:
            nsteps = 1, chain.ids = c(1,2), nthin = 1, nburnin = 5, niterperstep = 10,
            # nsteps = nsteps, chain.ids = chain.ids,
            iso.select = iso.select,
            runname.global = runname.global,
            runname.U5MR = runname.U5MR,
            indicator.type = "IMR",
            run.type = "country",
            run.on.server = run.on.server,
            data.cmeinfo.file = data.cmeinfo.file,
            year.current = year.current,
            year.lastestimatepublished = year.current, ### Typically year.current-1
            year.lastestimate = year.current,
            periods.unsmooth.list = periods.unsmooth.list,
            periods.smooth.list = periods.smooth.list)
    if (output.run.times)
      cat(paste(iso.select, paste(proc.time(), collapse = ","), sep = ","), 
          file = file.path("output", paste0(runname.global, "_onecountryruntimes",
                                            ifelse(is.validation, "val", ""), ".txt")),
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
    PlotResults(runname = runname)
    #----------------------------------------------------------------------
    # 7. Do a comparison plot with another run
    #----------------------------------------------------------------------
    PlotComparison(runname1 = runname,
                   runname2 = 'IGME2016',#"IGME2014",
                   legend1 = "Test for MCO",
                   legend2 = "IGME 2016 GR",
                   plot.igme = F)
    #----------------------------------------------------------------------
    # 8. Check convergence
    #----------------------------------------------------------------------
    CheckConvergence(runname = runname, check.convergence = TRUE)
  }
  nruns <- nruns + 1
  if (get.notifications) {
    if (nruns %% 10 == 0) {
      pbPost(type = "note", 
             title = paste0("3aIMR_cs_B3.R"), 
             body = paste0(nruns, " IMR B3 CS runs done!"),
             recipients = c(1, 2))
    }
  }
} 
if (get.notifications)
  pbPost(type = "note", 
         title = paste0("3aIMR_cs_B3.R"), 
         body = paste0("All IMR B3 CS runs done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# Fin.
