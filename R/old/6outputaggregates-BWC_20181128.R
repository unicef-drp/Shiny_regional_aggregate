#----------------------------------------------------------------------
# 6outputaggregates.R
# Jin Rou New, 2013-2015
#----------------------------------------------------------------------
# NOTE: To get NMR deaths, need to set up NMR runname folder and add "YYYY-MM-DDres_nmr.csv" for median calculation and "finalresults.jtc.Rda" for UI
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------


rm(list = ls()) # Clear workspace
run.on.server <- F# Indicate if run is on the server
get.notifications <- F # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
user<-"lhug" # "dsharrow"
workdir <- paste0("C:/Users/",user,"/Dropbox/IGME Data/2018 Round Estimation/Code") # Give work directory file path if not running things on server

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
# source("calculateARRforotherperiods.R")
#----------------------------------------------------------------------
# 3. Get all required components and output death/aggregates estimates
#----------------------------------------------------------------------
source(file.path("R/outputaggregates-BWC.R"))
runname.U5MR <- "GR20180210_all"
runname.IMR <- "IMR20180213_all"
runname.NMR <- "NMR_forDeathCalculation"
year.lastestimatepublished <- 2017.5
date <- Sys.Date()
#date <- "2018-07-26"
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
file.dir.ui <- file.path(paste("Aggregate results (UIs)", date)) ##<< File directory to save UIs to
file.dir.output <- file.path(paste("Aggregate results (final)", date)) ##<< File directory to save final (combined) median estimates + UIs to

#Total (median)
source(file.path("R/outputaggregates-BWC.R"))
OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                 results.NMR.file = file.path("output", runname.NMR, "finalres_nmr_2018-07-30.csv"),
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median,
                 year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
                 regiontypes.select = c("M49","UNICEFReport", "UNICEFProg","WHO", "WB", "UNPD", " SDGSimple "))

# Projections
source(file.path("R/outputaggregates-BWC.R"))
#projection <- c("Constant2013")#, "Constant2016", "HighIncome", "SDG2016", "Highincome2016", "Constant2000")
projections <- c("AdjARR","HighIncome2030", "SDGtarget", "Constant Rate 2017")

projection <- "AdjARR2100"
                 
for(projection in projections){
file.dir.median <- file.path(paste0("Projections/",projection," Aggregate results (median) ", date))
year.lastestimatepublished <- 2017.5#2016.5
ifelse(projection=="AdjARR"|projection=="Constant Rate 2017"|projection=="HighIncome2030"|projection=="SDGtarget", est.years.proj <- seq(1950.5,2050.5,1), est.years.proj <- seq(1950.5,2050.5,1)) 
est.years.proj<-seq(1950.5,2099.5,1)

OutputAggregates(#results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                 #results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                 #results.NMR.file = file.path("output", runname.NMR, "2017-08-10res_nmr.csv"),
                 results.U5MR.file = paste0("Projections/Input results/",projection,"/",projection,"_Results_U5MR.csv"),
                 results.IMR.file = paste0("Projections/Input results/",projection,"/",projection,"_Results_IMR.csv"),
                 results.NMR.file = paste0("Projections/Input results/",projection,"/",projection,"_Results_NMR.csv"),
                 run.on.server = run.on.server,
                 year4 = 2017.5,
                 output.dir = file.dir.median,
                 year.target = 2017.5, est.years = est.years.proj,
                 regiontypes.select = c("UNICEFReport","WB"))#,,"SDGSimple""UNICEFReport","WB" "Countdown", "ECAAfrica", "AU",
#                                         "Fragile2013", "Fragile2014", 
#                                         "Fragile2015", "Fragile2017",
#                                         "USAID")) ## add livebirths.file?
} # loop for projections


# crisis-free (median)
# OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results (crisis-free).csv"),
#                  results.IMR.file = file.path("output", runname.IMR, "Results (crisis-free).csv"),
#                  results.NMR.file = file.path("output", runname.NMR, "2017-08-10res_nmrcf.csv"),
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  year.target = 2016.5, est.years = seq(1950.5,2016.5,1),
#                  regiontypes.select = c("NewUnicef")) ## add livebirths.file?


# Total (UIs)
# OutputAggregates(runname.U5MR = runname.U5MR,
#                   runname.IMR = runname.IMR,
#                   runname.NMR = runname.NMR,
#                   run.on.server = run.on.server,
#                   year4 = year.lastestimatepublished,
#                   output.dir = file.dir.ui,
#                   filename.NMR = "finalresults.jtc.Rda",
#                   year.target = 2016.5, est.years = seq(1950.5,2016.5,1),
#                   # regiontypes.select = c("UNICEF", "NewUnicef", "MDG", "SDG", "WHO", "WB", "UNPD", "M49")
#                  regiontypes.select = c("UNICEF", "NewUnicef"),
#                  test = TRUE
#                  ) ## add livebirths.file?

# # Note: The run fails to run in parallel for region aggregates,
# # so restart run with run.on.server set to FALSE in the next step
# # Total (UIs) for regions
# for (regiontype.select in c("UNICEF", "NewUnicef", "MDG", "SDG", "WHO", "WB", "UNPD", "M49"))
#   OutputAggregates(runname.U5MR = runname.U5MR,
#                    runname.IMR = runname.IMR,
#                     runname.NMR = runname.NMR,
#                    regiontypes.select = regiontype.select, # Select regiontype or set to NULL for all regions.
#                    run.on.server = FALSE, # Note: Must be set to FALSE for regions.
#                    year4 = year.lastestimatepublished,
#                    output.dir = file.dir.ui,
#                     filename.NMR = "finalresults.jtc.Rda",
#                     year.target = 2016.5, est.years = seq(1950.5,2016.5,1))
# if (get.notifications)
#     pbPost(type = "note",
#            title = paste0("6outputaggregates.R"),
#            body = paste0("Aggregates done for ", regiontype.select),
#            recipients = c(1, 2))
# # Note: The options for regiontypes.select are "UNICEF", "NewUnicef", "MDG", "SDG", "WHO", "WB", "UNPD", "OIC",
# # "Countdown", "ECAAfrica", "AU", "Fragile2013", "Fragile2014", "Fragile2015", "Fragile2017", "USAID", "M49"
# stop()
#
# #----------------------------------------------------------------------
# # ARR (median)
# # CalculateARRForOtherPeriods(year1 = 2000.5, year2 = 2010.5, output.dir = "output_numberofdeaths_20180812")
# # CalculateARRForOtherPeriods(year1 = 2000.5, year2 = 2010.5, output.dir = "output_numberofdeaths_median_20130812")
# #----------------------------------------------------------------------
# # 5. Combine median + UIs
# #----------------------------------------------------------------------
file.dir.median <- file.path("C:/Users/dsharrow/Dropbox/IGME Data/2017 Round Estimation/Code/Aggregate results (median) 2017-11-30") ##<< File directory to save median estimates to
# file.dir.ui <- file.path(paste("Aggregate results (UIs)", date)) ##<< File directory to save UIs to
file.dir.ui <- file.path("C:/Users/dsharrow/Dropbox/IGME Data/2017 Round Estimation/Code/Aggregate results (final) 2017-09-12") ##<< File directory to save UIs to
file.dir.output <- file.path("C:/Users/dsharrow/Dropbox/IGME Data/2017 Round Estimation/Code/Aggregate results (final) 2017-11-30") ##<< File directory to save final (combined) median estimates + UIs to
# dir.create(file.dir.output, showWarnings = F)
#
# # Country
 file.name <- "Rates & Deaths_Country Summary.csv"
 estimates.file <- file.path(file.dir.ui, file.name)
 estimates.median.file <- file.path(file.dir.median, file.name)
 estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
 estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)

 estimates.median <- estimates.median[,match(names(estimates), names(estimates.median))]
 
 estimates.output <- rbind(estimates[estimates$X != "Median",], estimates.median)
 order <- order(estimates.output$CountryName, estimates.output$X)
 estimates.output <- estimates.output[order, ]
 write.csv(estimates.output, file.path(file.dir.output, file.name), row.names = F, na = "")

 # Region & world
 file.names <- list.files(file.dir.median)
 #file.names <- list.files(file.dir.ui)[c(2,5)]
 # file.names <- file.names[!is.element(file.names, c("samples", "samples_combined", "Rates & Deaths_Country Summary.csv"))]

 file.names <- file.names[!is.element(file.names, c("samples", "samples_combined", "Rates & Deaths_Country Summary.csv", "NMR", "IMR", "U5MR"))]
 for (file.name in file.names) {
   estimates.file <- file.path(file.dir.ui, file.name)
   estimates.median.file <- file.path(file.dir.median, file.name)
   estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
   estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)
   estimates[, grepl("median", colnames(estimates))] <- estimates.median[grepl("median", colnames(estimates.median))]
   write.csv(estimates, file.path(file.dir.output, file.name), row.names = F, na = "")
 }
 
 ## special code to combine world bank files -- median is using corrected classification and UI (final) uses same classifciation but different names -- this shouldn't be necessary in 2018 when median and UI should have same regional names
 estimates.file <- file.path(file.dir.ui, "Rates & Deaths_WBRegion_XX.csv")
 estimates.median.file <- file.path(file.dir.median, "Rates & Deaths_WBRegion.csv")
 estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
 # estimates file has Low and mIddle income regions excluding high income -- add to name
 estimates$Region[estimates$Region=="East Asia and Pacific"] <- "East Asia and Pacific (excluding high-income)"
 estimates$Region[estimates$Region=="Europe and Central Asia"] <- "Europe and Central Asia (excluding high-income)"
 estimates$Region[estimates$Region=="Latin America and the Caribbean"] <- "Latin America and the Caribbean (excluding high-income)"
 estimates$Region[estimates$Region=="Middle East and North Africa"] <- "Middle East and North Africa (excluding high-income)"
 estimates$Region[estimates$Region=="South Asia"] <- "South Asia (excluding high-income)"
 estimates$Region[estimates$Region=="Sub-Saharan Africa"] <- "Sub-Saharan Africa (excluding high-income)"
 ## get NewWorldBank estimates (includes high income)
 estimates.NewWorldBank <- read.csv("C:/Users/dsharrow/Dropbox/IGME Data/2017 Round Estimation/Code/Aggregate results (final) 2017-09-12/Rates & Deaths_NewWorldBank.csv", as.is=T)
 estimates.NewWorldBank <- estimates.NewWorldBank[estimates.NewWorldBank$Region!="World",]
 ## rbind estimates and estimates.NewWorldBank
 estimates <- rbind(estimates, estimates.NewWorldBank)

# get median file -- this has both including and exclduing high income regions
 estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)
 # estimates.median <- estimates.median[!(estimates.median$Region=="East Asia and Pacific"|
 #                                        estimates.median$Region=="Europe and Central Asia"|
 #                                        estimates.median$Region=="Latin America and the Caribbean"|
 #                                        estimates.median$Region=="Middle East and North Africa"|
 #                                        estimates.median$Region=="South Asia"|
 #                                        estimates.median$Region=="Sub-Saharan Africa"),
 #                                        ]
 estimates <- estimates[with(estimates, order(Year, Region)),]
 estimates.median <- estimates.median[with(estimates.median, order(Year, Region)),]
 
 estimates[, grepl("median", colnames(estimates))] <- estimates.median[grepl("median", colnames(estimates.median))]
 write.csv(estimates, file.path(file.dir.output, "Rates & Deaths_WBRegion.csv"), row.names = F, na = "")
 
# #----------------------------------------------------------------------
# # 6. Output results with WPP 2010
# #----------------------------------------------------------------------
# # Total (with WPP 2010)
# OutputAggregates(results.U5MR.file = "input/Results_U5MR_Final_20130812.csv",
#                  results.IMR.file = "input/Results_IMR_Final_20130812.csv",
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  population.file = "input/population_2012.csv",
#                  output.dir = "output_numberofdeaths_IGME2013withWPP2010")
# #----------------------------------------------------------------------
# # 7. Reformat sex-specific results then output death/aggregate estimates
# #----------------------------------------------------------------------
# for (indicator in c("U5MR", "IMR")) {
#   for (sex in c("Male", "Female")) {
#     file.input <- file.path("input", paste0("Results_", indicator, tolower(sex), ".csv"))
#     file.output <- file.path("input", paste0("Results_", indicator, "_", sex, "_Final.csv"))
#     res <- read.csv(file = file.input, header = T, stringsAsFactors = F)
#     res$Indicator <- ifelse(indicator == "U5MR", "Under-five Mortality Rate", "Infant Mortality Rate")
#     res$Subgroup <- sex
#     res.output <- cbind(res[!grepl("X", colnames(res))], 1000*res[grepl("X", colnames(res))])
#     write.csv(res.output, file = file.output, row.names = F, na = "")
#   }
# }
# #----------------------------------------------------------------------
# # Note: Requires results for male/female U5MR/IMR & male/female population at age 0/under age 
source(file.path("R/outputaggregates-BWC.R"))
# Male (median)
date <- "2017-09-13 (male)"
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
OutputAggregates(results.U5MR.file = "input/Results_u5mr_m.csv",
                 results.IMR.file = "input/Results_imr_m.csv",
                 population.file = "input/data_male_CMEpopulation_20150817.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median,
                 livebirths.file = "input/data_livebirths_male.csv",
                 year.target = 2016.5, est.years = seq(1950.5,2016.5,1),
                 regiontypes.select = c("WorldBankReg2", "NewWorldBank"))

# # Female (median)
date <- "2017-09-13 (female)"
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
OutputAggregates(results.U5MR.file = "input/Results_u5mr_f.csv",
                 results.IMR.file = "input/Results_imr_f.csv",
                 population.file = "input/data_female_CMEpopulation_20150817.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median,
                 livebirths.file = "input/data_livebirths_female.csv",
                 year.target = 2016.5, est.years = seq(1950.5,2016.5,1),
                 regiontypes.select = c("WorldBankReg2", "NewWorldBank"))
