#----------------------------------------------------------------------
# 2and3PRK.R
# Jin Rou New, 2013-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- F # Indicate if run is on the server
workdir <- "/Users/lhug/Dropbox/UN IGME Data/2019 Round Estimation/Code" # Give work directory file path if not running things on server # Give work directory file path if not running things on server

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
# 3. Get results for PRK
#----------------------------------------------------------------------
# U5MR and IMR (part of IMR output)
# User input
runname.global.U5MR <- "GR20190311"
runname.global.IMR <- "IMR20190313"
runname.global.IMR2 <- "IMR20190314"
year.current <- 2018.5
iso.select <- "PRK"
runname.U5MR <- paste0(runname.global.U5MR, "_", iso.select)
runname.IMR <- paste0(runname.global.IMR, "_", iso.select)
runname.IMR2 <- paste0(runname.global.IMR2, "_", iso.select)
data.cmeinfo.file <- "input/data_U5MR_20190507.csv"
#----------------------------------------------------------------------
ConstructOutputForPRK(runname = runname.U5MR,
                      runname.IMR = runname.IMR,
                      est.PRK.file = "input/MedianEstimate_PRK.csv",
                      runname.global = runname.global.U5MR,
                      nsim = 8000)
# PlotResults(runname = runname.U5MR)
#----------------------------------------------------------------------
# IMR
# User input
data.cmeinfo.file <- "input/data_IMR_20190507.csv"
info <- read.csv("input/infoUNinclHIV.csv", header = T, stringsAsFactors = F)
# End user input
#----------------------------------------------------------------------
print(paste0(runname.IMR, ": ", info$imrmethod.c[info$iso.c == iso.select]))
DeriveIMREstimatesFromU5MR(iso.select = iso.select,
                           year.end=2030.5,
                           runname = runname.IMR2, 
                           runname.U5MR = runname.U5MR,
                           runname.IMR = runname.IMR,
                           runname.global.U5MR = runname.global.U5MR,
                           data.cmeinfo.file = data.cmeinfo.file,
                           weight.alpha.select = 0.5)
# PlotResultsForIMR(runname = runname.IMR)
#----------------------------------------------------------------------
# Fin.
