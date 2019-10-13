#----------------------------------------------------------------------
# 1U5MRIMR_global.R
# Leontine Alkema & Jin Rou New, 2012-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- F # Indicate if run is on the server
get.notifications <- F # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
workdir <- "C:/Users/lhug/Dropbox/IGME Data/2018 Round Estimation/Code" # Give work directory file path if not running things on server

# Define working directory
if (run.on.server) {
  package.dir <- workdir <- getwd()
} else {
  package.dir <- workdir
}
setwd(workdir)
#----------------------------------------------------------------------
# 2. Load/get all required components
#----------------------------------------------------------------------
source(file.path(package.dir, "R/loadlibrariesandcodes.R"))
# NOTE: if you run this for the first time, use do.install = TRUE to install all packages needed 
#LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir, do.install = TRUE)

LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir)
if (get.notifications) {
  library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost("note", "Error!", geterrmessage(), recipients = 2)#c(1, 2))
  })
}
#----------------------------------------------------------------------
# 3. Get all required components and start MCMC global run
#----------------------------------------------------------------------
indicator.type <- "U5MR"
data.cmeinfo.file <- "input/data_U5MR_20180129.csv"
year.current <- 2017.5
##<< Specify a run name
if (indicator.type == "U5MR") {
  runname <- "GR20180130"
  runname.U5MR <- NULL
} else {
  runname <- "IMRGR20150511"
  runname.U5MR <- "GR20150511"
}
iso.select <- NULL ##<< \code{NULL} to read in all countries
# iso.select <- c("AFG", "ARM", "LKA", "SGP", "RWA") ##<< \code{NULL} to do a test run ####
is.validation <- FALSE ##<< Validation run?

RunMCMC(runname = runname,
        # test:
        #nsteps = 1, chain.ids = c(1,2), nthin = 1, nburnin = 5, niterperstep = 10, ######
        chain.ids = seq(1, 16), nsteps = 4,
        data.cmeinfo.file = data.cmeinfo.file,
        iso.select = iso.select,
        # isos.to.exclude.for.global.run = c("LIE", "PRK", "SPS", "MCO"),
        indicator.type = indicator.type,
        runname.U5MR = runname.U5MR,
        year.current = year.current,
        year.lastestimatepublished = year.current, ### Typically year.current-1
        year.lastestimate = year.current,
        is.validation = is.validation,
        run.on.server = run.on.server)
closeAllConnections()
print(warnings())
if (get.notifications)
  pbPost(type = "note", 
         title = paste0("1U5MRIMR_global.R"), 
         body = paste0("RunMCMC done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# 4. Read MCMC output and construct mcmc.array
#----------------------------------------------------------------------
ReadMCMCOutput(runname = runname, nsteps = NULL)
#----------------------------------------------------------------------
# 5. Read MCMC output and construct mcmc.array
#----------------------------------------------------------------------
GetGammaParameters(runname = runname)
#----------------------------------------------------------------------
# 6. Summarise results of global run into data.global
#----------------------------------------------------------------------
SummariseGlobalRun(runname.global = runname)
#----------------------------------------------------------------------
# 7. Construct output and output estimates in results.csv
#----------------------------------------------------------------------
ConstructOutput(runname = runname)  # make sure global.alpha.diffs.median and sd are changed!
if (get.notifications) 
  pbPost(type = "note", 
         title = paste0("1U5MRIMR_global.R"), 
         body = paste0("ConstructOutput done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# 8. Make plots
#----------------------------------------------------------------------
PlotResults(runname = runname, year.start = NULL, year.end = NULL)
PlotComparison(runname1 = runname, runname2 = "IGME2014",
               legend2 = "UN IGME 2014", plot.igme = F)
PlotMoreResults(runname = runname, year.start = NULL, year.end = NULL)
if (get.notifications) 
  pbPost(type = "note", 
         title = paste0("1U5MRIMR_global.R"), 
         body = paste0("Plots done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# 9. Diagnostics and plots of priors and posteriors
#----------------------------------------------------------------------
CheckConvergence(runname = runname)
if (get.notifications)
  pbPost(type = "note", 
         title = paste0("1U5MRIMR_global.R"), 
         body = paste0("CheckConvergence done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# Fin.
