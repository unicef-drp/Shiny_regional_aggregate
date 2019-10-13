#----------------------------------------------------------------------
# 3bIMR_cc_derivefromU5MR.R
# Jin Rou New, 2013-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- F # Indicate if run is on the server
get.notifications <- F # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
workdir <- "C:/Users/lhug/Dropbox/IGME Data/2018 Round Estimation/Code"  # Give work directory file path if not running things on server

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
# 3. Get all required components and start 
#   (i) deriving IMR estimates from U5MR estimates via relative rescaling 
#       countries using B3 for IMR
#   (ii) deriving IMR estimates from U5MR estimates via MLT or Sahel eqn
#----------------------------------------------------------------------
runname.global <- "IMR20180213" # consolidated IMR runs (B3+others, except PRK)
runname.global.U5MR <- "GR20180210"
runname.global.IMR <- "IMR20180212" # country-specific IMR runs (B3 only)
runname.U5MR<-"GR20180210_ECU_update"# enter the runame used for U5MR update!
data.cmeinfo.file <- "input/Ecuador_IMR.csv"#data_IMR_20170809.csv"
info <- read.csv("input/infoUNinclHIV.csv", header = T, stringsAsFactors = F)
#iso.all <- info$iso.c[!is.element(info$iso.c, c("LIE", "PRK"))]

### To update
# data.hiv <- read.csv("input/dataUNAIDS_U5MR.csv", header = T, stringsAsFactors = F)
# iso.all <- unique(data.hiv$countrycode.hiv)
iso.all <- c("MMR", "BWA", "CMR", "CAF", "CIV", # "GAB", "MOZ",
             "KEN", "LSO", "MWI", "NAM", "RWA", "ZAF", "SWZ", "UGA", "TZA", "ZMB", "ZWE",
             "BOL", "BDI", "CHN", "COD", "EGY", "GEO", "GNB", "MDG", "MRT", "NGA", "NPL", "SLE",
             "ATG", "TUR")
iso.all <- "ECU"
C.all <- length(iso.all)
print(C.all)

nruns <- 0
if (run.on.server) {
  registerDoMC(cores = detectCores())
  print(paste0("Running in parallel? ",
               ifelse(getDoParWorkers() == 1,  
                      "No.", paste0("Yes, with ", getDoParWorkers(), " cores."))))
}
# foreach(c.all=1:C.all) %dopar% {
for (c.all in 1:C.all) {
  iso.select <- iso.all[c.all]
  runname <- paste0(runname.global, "_", iso.select)
  if (!file.exists(file.path("output", runname, "mcmc.meta.rda"))) {
    runname.U5MR <- paste0(runname.global.U5MR, "_", iso.select)
    if (info$imrmethod.c[info$iso.c == iso.select] == "B3") {
      runname.IMR <- paste0(runname.global.IMR, "_", iso.select)
    } else {
      runname.IMR <- NULL
    }
    print(paste0(runname, ": ", info$imrmethod.c[info$iso.c == iso.select]))
    DeriveIMREstimatesFromU5MR(iso.select = iso.select,
                               runname = runname, 
                               runname.U5MR = runname.U5MR,
                               runname.IMR = runname.IMR,
                               runname.global.U5MR = runname.global.U5MR,
                               runname.global.IMR = runname.global.IMR,
                               data.cmeinfo.file = data.cmeinfo.file)
    closeAllConnections()
    #PlotResultsForIMR(runname = runname)
  }
  nruns <- nruns + 1
  if (FALSE) { ################
    if (nruns %% 10 == 0)
      pbPost(type = "note", 
             title = paste0("3bIMR_cs_derivefromU5MR.R"), 
             body = paste0(nruns, " IMR CS runs done!"),
             recipients = c(1, 2))
  }

}
if (get.notifications)
  pbPost(type = "note", 
         title = paste0("3bIMR_cs_derivefromU5MR.R"), 
         body = paste0("All IMR B3 runs done!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# Fin.
