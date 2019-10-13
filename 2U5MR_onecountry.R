#----------------------------------------------------------------------
# 2U5MR_onecountry.R
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
run.on.server <- F # Indicate if run is on the server
get.notifications <- F # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
workdir <- "C:/Users/dsharrow/Dropbox/UN IGME Data/2019 Round Estimation/Code" # Give work directory file path if not running things on server


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
  #library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost("note", "Error!", geterrmessage(), recipients = 2)#c(1, 2))
  })
}
# There will be a lot of output, to ignore as long as it does not contain R errors
#----------------------------------------------------------------------
# 3. Get all required components and start MCMC country-specific run
#    for all countries
#----------------------------------------------------------------------
runname.global <- "GR20190311" ##<< Global run to use. Do not change
data.cmeinfo.file <- "input/data_U5MR_20190507-test.csv" ##<< File path of .CSV file in CME Info database format for one country if default database is not used
iso.select <- 'PAK' ##<< 3-character ISO country code of country to run for
# runname <-  paste0(runname.global, "_", iso.select, "_PDSasSurveyAllYears") ##<< Run name
runname <- "GR20190311_PAK_test"
runname2 <-"GR20190311_PAK_5yrDHS" #C:\Users\lhug\Dropbox\IGME Data\2018 Round Estimation\Code\output\GR20180210_ECU_ENASNUT (B+D) and RHS"
runname3 <- "GR20180210_all"
# runname4 <- "GR20180210_all" # "IGME2014" ##<< Run name of another run for comparison, use "IGME2013" to compare with previous year's run
year.current <- 2030.5

# if (run.on.server) {
#   registerDoMC(cores = detectCores())
#   print(paste0("Running in parallel? ",
#                ifelse(getDoParWorkers() == 1,  
#                       "No.", paste0("Yes, with ", getDoParWorkers(), " cores."))))
# }
if (!file.exists(file.path("output", runname, "mcmc.meta.rda"))) {
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
  # } else if (iso.select == "SSD") {
  #   periods.constant.list <- list(c(2013.5, 2017.5))
  }  else {
    periods.constant.list <- NULL
  }
  if (iso.select == "YEM"){ # add by YS
    special.constant.list <- list (c(2011.5,year.current))
  } else if (iso.select == "SYR") {
   special.constant.list <- list(c(2010.5, year.current))
  } else if (iso.select == "VEN") { # add by DJS 2018-03-16
   special.constant.list <- list(c(2015.5, year.current))
  } else if (iso.select == "SSD") { # add by DJS 2018-07-05
    special.constant.list <- list(c(2013.5, year.current))
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
  RunMCMC(runname = runname,
          # test:
          nsteps = 1, chain.ids = c(1,2), nthin = 1, nburnin = 5, niterperstep = 10,
          # nsteps = nsteps, chain.ids = chain.ids,
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
          periods.constant.list = periods.constant.list,
          special.constant.list = special.constant.list)
  closeAllConnections()
  #----------------------------------------------------------------------
  # 4. Read MCMC output and construct mcmc.array
  #----------------------------------------------------------------------
  ReadMCMCOutput(runname = runname)
  #----------------------------------------------------------------------
  # 5. Construct output and output estimates in results.csv
  #----------------------------------------------------------------------
  ConstructOutput(runname = runname, year.start = 1985)
  #----------------------------------------------------------------------
  # 6. Make plots
  #----------------------------------------------------------------------
  # PlotResults(runname = runname, seriesnames.in.full = T,zoom=T)
  # # #----------------------------------------------------------------------
  # # # 7. Do a comparison plot with another run
  # # #----------------------------------------------------------------------
  # PlotComparison(runname1 = runname,
  #                runname2 = runname2,
  #                runname3 = runname3,
  #                # runname4 = runname4,
  #                legend1 = "Include only most recent DHS point",
  #                legend2 = "Include all DHS points", 
  #                legend3 = "UN IGME 2018 (no DHS 2017-18)",
  #                # legend4 = "UN IGME 2018 (no DHS)",
  #                year.end=2018.5,
  #                zoom.year.start=1990.5,
  #                zoom.year.end=2018.5,
  #                plot.igme = F)
}
if (get.notifications)
  pbPost(type = "note", 
         title = paste0("2U5MR_onecountry.R"), 
         body = paste0("U5MR one-country run done for ", iso.select, "!"),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# Fin.
