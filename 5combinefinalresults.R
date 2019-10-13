#----------------------------------------------------------------------
# 5combineresults.R
# Leontine Alkema & Jin Rou New, 2012-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- TRUE # Indicate if run is on the server
workdir <- "~/Dropbox/Jin Rou/CME2015" # Give work directory file path if not running things on server

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
# There will be a lot of output, to ignore as long as it does not contain R errors
#----------------------------------------------------------------------
# 3. Produce inputs for sex-specific mortality and NMR models
#----------------------------------------------------------------------
runname.U5MR <- "GR20150511_all"
runname.IMR<- "IMR20150518_all"
# Produce inputs for NMR model
GetAdjustmentFreeTrajectories(runname = runname.U5MR)
GetAdjustmentFreeTrajectories(runname = runname.IMR)
# Produce inputs for sex-specific mortality model 
# (requires that the above code for NMR model is run first)
CombineFinalResults(runname.U5MR = runname.U5MR, runname.IMR = runname.IMR,
                    crisis.free = TRUE, hiv.free = FALSE)
# Note: As of UN IGME 2015, crisis-free but not HIV-free inputs used for
# sex-specific mortality model. Prior to this, non-crisis-free and non-HIV-free
# inputs used.
# Produce inputs for calculating aggregates
CombineFinalResults(runname.U5MR = runname.U5MR, runname.IMR = runname.IMR,
                    crisis.free = FALSE, hiv.free = FALSE)
