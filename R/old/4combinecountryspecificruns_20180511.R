#----------------------------------------------------------------------
# 4combinecountryspecificruns.R
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
# 2. Load libraries and codes 
#----------------------------------------------------------------------
source(file.path(package.dir, "R/loadlibrariesandcodes.R"))

# NOTE: if you run this for the first time, use do.install = TRUE to install all packages needed 
# LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir, do.install = TRUE)

LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir)
if (get.notifications) {
  library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost("note", "Error!", geterrmessage())
  })
}
# There will be a lot of output, to ignore as long as it does not contain R errors
#----------------------------------------------------------------------
# 3. Indicate run settings
#----------------------------------------------------------------------
indicator.type <- "U5MR" # "IMR"
if (indicator.type == "U5MR") {
  runname.global <- "GR20180210"
} else {
  runname.global <- "IMR20180213"
}
is.validation <- FALSE
year.lastestimatepublished <- 2030.5 # year to plot results until
data.cmeinfo.file <- ifelse(indicator.type == "U5MR",
                            "input/data_U5MR_20180302.csv",
                            "input/data_IMR_20180302.csv")
info <- read.csv("input/infoUNinclHIV.csv", header = T, stringsAsFactors = F)
iso.all <- info$iso.c[info$iso.c != "LIE"] # including "PRK"
#iso.all<-c("AFG","AND")
#----------------------------------------------------------------------
# 4. Get results file of all country estimates and UIs
#----------------------------------------------------------------------
dir.create(file.path(getwd(), "output", paste0(runname.global, "_all")), showWarnings = F)
for (file.name in c("Results.csv", "Results (crisis-free).csv", "Results (HIV-free).csv", 
                    "Results (crisis-and-HIV-free).csv")) {
  results.combined <- NULL
  for (c.all in 1:length(iso.all)) {
    iso.select <- iso.all[c.all]
    runname <- paste0(runname.global, "_", iso.select)
    results.country <- read.csv(file.path("output", runname, file.name), header = T, stringsAsFactors = F)
    results.combined <- rbind(results.combined, results.country)  
  }
  write.csv(results.combined, file = file.path("output", paste0(runname.global, "_all"), 
                                               file.name),
            row.names = F, na = "")
}

# For U5MR, summarise results in res.U5MR to be used as IMR/NMR run input
if (indicator.type == "U5MR") {
  SummariseU5MREstimates(results.file = file.path("output", paste0(runname.global, "_all"), "Results.csv"),
                         output.dir = file.path("output", paste0(runname.global, "_all")))
  SummariseU5MREstimates(results.file = file.path("output", paste0(runname.global, "_all"), "Results.csv"),
                         get.adjusted.estimates = FALSE,
                         output.dir = file.path("output", paste0(runname.global, "_all")))
}
#----------------------------------------------------------------------
# 5. Combine all country-specific runs
#----------------------------------------------------------------------
if (indicator.type == "U5MR") {
  runname.global.input <- runname.global
} else if (indicator.type == "IMR") {
  runname.global.input <- NULL
}
CombineCountrySpecificRuns(
  runname.prefix.countryspecific = runname.global,
  runname.global = runname.global.input,
  indicator.type = indicator.type,
  data.cmeinfo.file = data.cmeinfo.file,
  get.combinedresults = T, ##<< FALSE if only mcmc.meta is required
  get.trajectories = T,
  get.PPD = ifelse(indicator.type == "U5MR", T, F),
  iso.all = iso.all)
print(warnings())
#----------------------------------------------------------------------
# 6. Plot combined country-specific runs (various versions)
#----------------------------------------------------------------------
dir.create("fig", showWarnings = FALSE)
if (indicator.type == "U5MR") {
  fig.dir <-  file.path("fig", paste0(runname.global, "_all", " U5MR Plots ", Sys.Date()))
  fig.dir.cc <- file.path("fig", paste0(runname.global, "_all", " U5MR Country Plots ", Sys.Date()))
  PlotResults(runname = paste0(runname.global, "_all"),
              # year.end = year.lastestimatepublished,
              fig.dir = fig.dir,
              separate.plots.by.country = F,
              zoom = T,
              add.legend = T)
  # comparison of final run with previous year's run
  PlotComparison(runname1 = paste0(runname.global, "_all"), 
                 runname2 = "GR20170401_all", 
                 legend1 = "Draft UN IGME 2018",
                 legend2 = "UN IGME 2017",
                 # year.end = year.lastestimatepublished,
                 fig.dir = fig.dir,
                 zoom = F,
                 plot.igme = F,
                 plot.defaultloess = F,
                 year.end=2017)
  if (FALSE) {
    # comparison of final run with cc estimates
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "GR20140530notweaks_all_precc", 
                   legend1 = "UN IGME 2015",
                   legend2 = "Country consultation 2015",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
    # comparison of final run with previous year's run and cc run
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "GR20140530notweaks_all_precc", 
                   runname3 = "IGME2014", 
                   legend1 = "UN IGME 2015",
                   legend2 = "Country consultation 2015",
                   legend3 = "UN IGME 2014",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
  }
  if (FALSE) {
    # comparison of final run with IGME meeting estimates
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "GR20150511_all_preIGMEmeeting", 
                   legend1 = "Draft UN IGME 2015",
                   legend2 = "UN IGME Meeting 2015",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
    # comparison of final run with IGME meeting estimates and previous year's run
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "GR20150511_all_preIGMEmeeting", 
                   runname3 = "IGME2014", 
                   legend1 = "Draft UN IGME 2015",
                   legend2 = "UN IGME Meeting 2015",
                   legend3 = "UN IGME 2014",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
  }
  GetResidualsData(runname = paste0(runname.global, "_all"))
  PlotResiduals(runname = paste0(runname.global, "_all"))
  CheckConvergence(runname = paste0(runname.global, "_all"))
  # Individual country plots for country consultation, with results shown up to last year published
  if (TRUE)
    PlotResults(runname = paste0(runname.global, "_all"),
                year.end = year.lastestimatepublished,
                fig.dir = fig.dir.cc,
                plot.model.parameters = F,
                separate.plots.by.country = T,
                zoom = T,
                add.legend = T)
} else {
  fig.dir <- file.path("fig", paste0(runname.global, "_all", " IMR Plots ", Sys.Date()))
  fig.dir.cc <- file.path("fig", paste0(runname.global, "_all", " IMR Country Plots ", Sys.Date()))
  PlotResultsForIMR(runname = paste0(runname.global, "_all"),
                    # year.end = year.lastestimatepublished,
                    fig.dir = fig.dir,
                    separate.plots.by.country = F)
  # comparison of final run with previous year's run
  PlotComparison(runname1 = paste0(runname.global, "_all"), 
                 runname2 = "IGME2014IMR", 
                 legend1 = "Draft UN IGME 2015",
                 legend2 = "UN IGME 2014",
                 # year.end = year.lastestimatepublished,
                 fig.dir = fig.dir,
                 plot.igme = F,
                 plot.defaultloess = F)
  if (FALSE) {
    # comparison of final run with cc estimates
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "IMR20140607_all_precc", 
                   legend1 = "UN IGME 2015",
                   legend2 = "Country consultation 2015",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
    # comparison of final run with previous year's run and cc run
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "IMR20140607_all_precc", 
                   runname3 = "IGME2014IMR", 
                   legend1 = "UN IGME 2015",
                   legend2 = "Country consultation 2015",
                   legend3 = "UN IGME 2014",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
  }
  if (TRUE) {
    # comparison of final run with IGME meeting estimates
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "IMR20150518_all_preIGMEmeeting", 
                   legend1 = "Draft UN IGME 2015",
                   legend2 = "UN IGME Meeting 2015",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
    # comparison of final run with IGME meeting estimates and previous year's run
    PlotComparison(runname1 = paste0(runname.global, "_all"), 
                   runname2 = "IMR20150518_all_preIGMEmeeting", 
                   runname3 = "IGME2014IMR", 
                   legend1 = "Draft UN IGME 2015",
                   legend2 = "UN IGME Meeting 2015",
                   legend3 = "UN IGME 2014",
                   # year.end = year.lastestimatepublished,
                   fig.dir = fig.dir,
                   plot.igme = F,
                   plot.defaultloess = F)
  }
  # Individual country plots for country consultation, with results shown up to last year published
  if (TRUE) 
    PlotResultsForIMR(runname = paste0(runname.global, "_all"),
                      year.end = year.lastestimatepublished,
                      fig.dir = fig.dir.cc,
                      separate.plots.by.country = T)
}
if (get.notifications)
  pbPost(type = "note", 
         title = "4combinecountryspecificruns.R", 
         body = paste0(runname.global, "_all is complete."),
         recipients = c(1, 2))
#----------------------------------------------------------------------
# Fin
