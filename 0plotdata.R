#----------------------------------------------------------------------
# 0plotdata.R
# Leontine Alkema & Jin Rou New, 2012-2014
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
# Clear workspace
rm(list = ls())
run.on.server <- FALSE # Indicate if run is on the server
workdir <- "/Users/dsharrow/Dropbox/UN IGME Data/2019 Round Estimation/Code" # Give work directory file path if not running things on server

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
# LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir, do.install = TRUE)

LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir)
#----------------------------------------------------------------------
# 3. Get all required components and start MCMC global run
#----------------------------------------------------------------------
indicator.type <- "IMR" #
year.current <- 2018.5

##<< Specify a run name
if (indicator.type == "U5MR") {
  # runname <- "GR20150511"
  runname <- "GR20190311_all_forplotting" #
  runname.U5MR <- "GR20190311_all_forplotting"#"GR20190311"
  data.cmeinfo.file <- "input/data_U5MR_20190722.csv"#
} else if (indicator.type == "IMR") {
  # runname <- "IMR2019" 
  runname <- "IMR20190314_all_forplotting"
  runname.U5MR <- "GR20190311_all_forplotting" # "GR20130512_all" # need a res.U5MR.rda file in IGME2017 and GrU5MR2019 to plot data
  # data.cmeinfo.file <- "input/data_IMR_20190310.csv"
  data.cmeinfo.file <- "input/data_IMR_20190718.csv"
}
iso.select <- NULL #"TTO" #<< \code{NULL} to read in all countries
# iso.select <- c("AFG", "ARM", "HTI", "SGP", "RWA") ##<< \code{NULL} to do a test run
is.validation <- FALSE ##<< Validation run?
RunMCMC(runname = runname,
        # test:
        # nsteps = 1, chain.ids = c(1,2), nthin = 1, nburnin = 5, niterperstep = 10,
        data.cmeinfo.file = data.cmeinfo.file,
        iso.select = iso.select,
        year.current = year.current,
        #year.lastestimatepublished = 2016.5,
        isos.to.exclude.for.global.run = c("LIE", "SPS"),
        indicator.type = indicator.type,
        runname.U5MR = runname.U5MR,
        is.validation = is.validation,
        run.jags = FALSE,
        run.for.IMR.MLT = ifelse(indicator.type == "U5MR", FALSE, TRUE),
        run.on.server = run.on.server)
closeAllConnections()
print(warnings())
#----------------------------------------------------------------------
load(file.path("output", runname, "mcmc.meta.rda"))
pdf(file = file.path(paste0("fig/", runname, "", indicator.type, " Data Plot ", 
                            Sys.Date(), ".pdf")), width = 21, height = 7)
if (indicator.type == "U5MR") {
  for (c in 1:mcmc.meta$data.all$C)
    PlotDataAndEstimates(data = mcmc.meta$data,
                         data.all = mcmc.meta$data.all,
                         c = c,
                         ylab = mcmc.meta$settings$indicator.type,
                         plot.se = TRUE)
} else if (indicator.type == "IMR") {
  for (c in 1:mcmc.meta$data.all$C)
    PlotDataAndEstimates(data = mcmc.meta$data,
                         data.all = mcmc.meta$data.all,
                         c = c,
                         ylab = mcmc.meta$settings$indicator.type,
                         legendfull = mcmc.meta$data.all$imrmethod.c[c],
                         plot.se = TRUE)
}
dev.off()
#----------------------------------------------------------------------
# Fin.
