rm(list = ls()) # Clear workspace
run.on.server <- F # Indicate if run is on the server
get.notifications <- F # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
username<-"dsharrow"     # Please enter your username in Dropbox folder       Change Kai 04/26/2018
workdir <- file.path(paste("C:/Users/",username,"/Dropbox/UN IGME Data/2019 Round Estimation/Code",sep="")) # Give work directory file path if not running things on server
#workdir <- "C:/Users/lhug/Dropbox/IGME Data/2018 Round Estimation/Code" # Give work directory file path if not running things on server

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

source(file.path(package.dir, "R/plot for profiles/plotdataandestimates.R"))      # Change Kai 05/11/2018
source(file.path(package.dir, "R/plot for profiles/plotresults.R"))               # used to plot U5MR     Change Kai 05/11/2018
source(file.path(package.dir, "R/plot for profiles/plotresultsforimr.R"))         # used to plot IMR      Change Kai 05/11/2018
source(file.path(package.dir, "R/plot for profiles/plotcomparison.R"))         # used to plot IMR      Change Kai 05/11/2018
source(file.path(package.dir, "R/plot for profiles/read_wpp_and_ihme.R"))       # used to plot wpp and GBD data     Change Kai 07/05/2018
source(file.path(package.dir, "R/plot for profiles/read_wpp_and_completeihme.R"))       # used to plot IHME data      Change Kai 07/05/2018
# There will be a lot of output, to ignore as long as it does not contain R errors
#----------------------------------------------------------------------
# 3. Indicate run settings
#----------------------------------------------------------------------
indicator.type <- "U5MR" # "IMR",#U5MR
if (indicator.type == "U5MR") {
  runname.global <- "GR20190311"
} else {
  runname.global <- "IMR20190314"
}
is.validation <- FALSE
year.lastestimatepublished <- 2018.5 # final year of plot results (max 2030.5)
data.cmeinfo.file <- ifelse(indicator.type == "U5MR",
                            "input/data_U5MR_20190507-test.csv",
                            "input/data_IMR_20190507.csv")              #####enter the most recent file of U5MR or IMR database


#----------------------------------------------------------------------
# 6. Plot combined country-specific runs (various versions)
#----------------------------------------------------------------------
if (indicator.type == "U5MR") {
#####Plot U5MR#####
fig.dir <- "C:/Users/dsharrow/Dropbox/UN IGME data/2019 Round Estimation/Consultation profiles/Results/Plots/U5MR Total"
#fig.dir <-  file.path("fig", paste0(runname.global, "_all", " U5MR Plots ", Sys.Date()))
file.format <- "png"     # Select "pdf" or "png" option  Change Kai 04/16/2018
wpp="input/wpp.csv"           ####add wpp 2017 data
ihme="output/IHME/IHME_GBD_2016_MORTALITY_1970_2016.XLSX"    #####add 2016 global burden of disease data
completeihme="output/IHME/IHME 5q0 Mortality_estimates 2018 latest version.xlsx"     #####add ihme data downloaded from 
library(readxl)   ###if you want to add ihme and wpp, you need this package to read data in excel file
library(tidyr)

PlotResults(runname = paste0(runname.global, "_PAK"),
            year.end = year.lastestimatepublished,
            fig.dir = fig.dir,
            separate.plots.by.country = F,
            hiv.removed = F,  #### show results with HIV deaths removed    Kai 09/26/2018
            zoom = T , # zoom in window 1990 to year.end, zoom and main.plot options, , one of them could set to be FALSE #Kai Zhong 05/14/2018
            main.plot = T,  # main plot window, zoom and main.plot options, one of them could set to be FALSE #Kai Zhong 05/14/2018
            officialname = T, # Use country official name  #kai added 04/16/2018
            add.legend = T,
            file.format=file.format,    # File format of plots option  #Kai Zhong 04/16/2018
            indirect_series_visibility=T,  ####If false, some indirect series will be invisible due to the existence of direct data series with the same name.   Kai Zhong 05/23/2018
            legendtext="Draft UN IGME 2019",     #####legend name of most recent u5mr curve in red color
            wpp=NULL,  ###wpp estimates, default value is NULL
            ihme=NULL,  ###global burden of disease estimates, default value is NULL
            completeihme=NULL,
            zoom.year.end = 2018.5)  ###ihme estimates, default value is NULL

PlotComparison(runname1 = paste0(runname.global, "_PAK"), 
               runname2 = paste0(runname.global, "_all_CC"), 
               legend1 = "Revised UN IGME 2019",          ####first legend name
               legend2 = "Draft country consultation",     ###second legend name
               # year.end = year.lastestimatepublished,
               fig.dir = fig.dir,
               main.plot = T,                 # main plot window, zoom and main.plot options, one of them could set to be FALSE #Kai Zhong 05/14/2018
               zoom = T,                      # zoom in window 1990 to year.end, zoom and main.plot options, , one of them could set to be FALSE #Kai Zhong 05/14/2018
               add.legend = T,
               plot.igme = F,
               filename=NULL,
               plot.defaultloess = F,
               file.format = file.format,   # File format of plots option  #Kai Zhong 04/16/2018
               year.end=2018.5,
               indirect_series_visibility=T,      ####If false, some indirect series will be invisible due to the existence of direct data series with the same name.   Kai Zhong 05/23/2018
               separate.plots.by.country = T)  ####If false, some indirect series will be invisible due to the existence of direct data series with the same name.   Kai Zhong 05/23/2018
}

if (indicator.type == "IMR") {
#####Plot IMR######
fig.dir <- "C:/Users/dsharrow/Dropbox/UN IGME data/2019 Round Estimation/Consultation profiles/Results/Plots/IMR Total"
#fig.dir <- file.path("fig", paste0(runname.global, "_all", " IMR Plots ", Sys.Date()))
file.format <- "png"     # Select "pdf" or "png" option  Change Kai 04/16/2018
wpp="input/wpp.csv"           ####add wpp 2017 data
ihme="output/IHME/IHME_GBD_2016_MORTALITY_1970_2016.XLSX"    #####add 2016 global burden of disease data
completeihme="output/IHME/IHME 5q0 Mortality_estimates 2018 latest version.xlsx"     #####add ihme data downloaded from 
library(readxl)   ###if you want to add ihme and wpp, you need this package to read data in excel file
library(tidyr)

PlotResultsForIMR(runname = paste0(runname.global, "_VEN"),
                  year.end = year.lastestimatepublished,
                  fig.dir = fig.dir,
                  hiv.removed = F,  #### show results with HIV deaths removed    Kai 09/26/2018
                  officialname = T, # kai added country official name parameter 04/16/2018
                  zoom = T,
                  main.plot = TRUE,  # kai added zoom and main.plot options, one of them could set to be FALSE 05/14/2018
                  add.legend = T,
                  separate.plots.by.country = F,
                  file.format = file.format,     ###kai added file.format option here 04/16/2018
                  indirect_series_visibility=T,
                  legendtext="Draft UN IGME 2019",
                  legend3=NULL,
                  legend4=NULL,
                  zoom.year.end = 2018.5)     ####If false, some indirect series will be invisible due to the existence of direct data series with the same name.   Kai Zhong 05/23/2018

PlotComparison(runname1 = paste0(runname.global, "_all"), 
               runname2 = paste0(runname.global, "_all_CC"),    ###change previous estimate file
               legend1 = "Revised UN IGME 2019",          ####first legend name
               legend2 = "Draft country consultation estimates",     ###second legend name
               # year.end = year.lastestimatepublished,
               fig.dir = fig.dir,
               plot.igme = F,
               filename=NULL,
               officialname = T,
               file.format = file.format,   # File format of plots option  #Kai Zhong 04/16/2018
               year.end=2018.5,
               plot.defaultloess = F,
               separate.plots.by.country = T)
}