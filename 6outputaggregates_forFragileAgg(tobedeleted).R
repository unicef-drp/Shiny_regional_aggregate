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
user<-"dsharrow"
workdir <- paste0("C:/Users/",user,"/Dropbox/UN IGME Data/2019 Round Estimation/Code") # Give work directory file path if not running things on server

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

source(file.path("R/outputaggregates-BWC.R"))
source(file.path("R/chooseregion.R"))
country.info <- read.csv("input/country.info.CME.csv", as.is=T)

#----------------------------------------------------------------------
# 3. Get all required components and output death/aggregates estimates
#----------------------------------------------------------------------
# source(file.path("R/outputaggregates-BWC.R"))
workdir <- paste0("C:/Users/",user,"/Dropbox/IGME Data/2018 Round Estimation/Code")
setwd(workdir)

runname.U5MR <- "GR20180210_all"
runname.IMR <- "IMR20180213_all"
runname.NMR <- "NMR_forDeathCalculation"
year.lastestimatepublished <- 2017.5
# date <- Sys.Date()
date <- "2018-07-31"
#date <- "2018-11-29 (test -- to be deleted immedietly)"
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
file.dir.ui <- file.path(paste("Aggregate results (UIs)", date)) ##<< File directory to save UIs to
file.dir.output <- file.path(paste("Aggregate results (final)", date)) ##<< File directory to save final (combined) median estimates


#Note: if replace.rates.reg and replace.rates.cat are NULL, Outputaggregates will calculate aggregates in the conventional way, i.e. replacing missing rates with the regional aggregate rate

#Note: if replace.rates.reg and replace.rates.cat are indicated with the desired aggregate and aggregate categories respectrively, Outputaggregates will calculate aggregates by replacing missing rates with the rate from a replacement aggrgate indicated by replace.rates.reg (the code below is set up to use the M49 aggregate as a replacement)

#Note: to calculate aggregates with a replacement aggregate perform the following steps:
# 1. If generating aggregates for the first time this round, generate aggregates in the conventional way to get country deaths and deaths for the aggregate region (i.e. set replace.rates.reg=NULL and replace.rates.cat=NULL)
# 2. Delete the World files so the code will reproduce the world aggregate calculated with the missing rates replaced with replacement aggregate
# 3. Generate aggregates specifiying the replacement aggregate and categories (samples files for the replacement aggregate will have "-replace" in the file names)

#Total (median)
#1. Generate aggregates in the conventional way (only needed if this is first time running aggregates this round to get the replacement)
# OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
#                  results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
#                  results.NMR.file = file.path("output", runname.NMR, "finalres_nmr_2018-07-30.csv"),
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  year.target = 2017.5, est.years = seq(1950.5,2018.5,1),
#                  test=FALSE,
#                  regiontypes.select = c("M49"),
#                  replace.rates.reg=NULL,
#                  replace.rates.cat=NULL)


# first.time.agg <- F
# if(first.time.agg){
# ### 2.  DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
# unlink(file.path(file.dir.median, "samples_combined", paste0("coverage0.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("coverageu5.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("death0.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("deathu5.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("deathnn.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("imr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("u5mr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("nmr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop0.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop0.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop1to4.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop1to4.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("popu5.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("res.world.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("global.RoDs.ui.rda")))
# unlink(file.path(file.dir.median, paste0("Rates & Deaths_World.csv")))
# unlink(file.path(file.dir.median, paste0("Rates of Decline_World.csv")))
# ### DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
# }

# 3. Generate aggregates specifiying the replacement aggregate and categories
OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                 results.NMR.file = file.path("output", runname.NMR, "finalres_nmr_2018-07-30.csv"),
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median,
                 year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
                 test=FALSE,
                 # regiontypes.select = c("M49","UNICEFReport", "UNICEFProg","WHO", "WB", "UNPD", "SDGsimple"),
                 # regiontypes.select = c("UNICEFReport", "SDG", "WB", "WHO", "UNPD"),
                 regiontypes.select = c("Fragile2018"),
                 replace.rates.reg="M49Region",
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]))

# Total (UIs)
# first generate aggregates for M49Region to be used as replacements
# OutputAggregates(runname.U5MR = runname.U5MR,
#                   runname.IMR = runname.IMR,
#                   runname.NMR = runname.NMR,
#                   run.on.server = run.on.server,
#                   year4 = year.lastestimatepublished,
#                   output.dir = file.dir.ui,
#                   filename.NMR = "finalresults.jtc.Rda",
#                   year.target = 2018.5, est.years = seq(1950.5,2018.5,1),
#                  regiontypes.select = c("M49"),
#                  test = TRUE, ## TEST RUN will sample 5 trajectories instead of full 8000
#                  replace.rates.reg=NULL,
#                  replace.rates.cat=NULL
#                  )

# first.time.agg <- F
# if(first.time.agg){
# ### 2.  DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
# unlink(file.path(file.dir.ui, "samples_combined", paste0("coverage0.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("coverageu5.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("death0.all.wtj.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("deathu5.all.wtj.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("deathnn.all.wtj.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("imr.wtj.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("u5mr.wtj.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("nmr.wtj.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("pop0.orig.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("pop0.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("pop1to4.orig.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("pop1to4.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("popu5.orig.wt.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("res.world.rda")))
# unlink(file.path(file.dir.ui, "samples_combined", paste0("global.RoDs.ui.rda")))
# unlink(file.path(file.dir.ui, paste0("Rates & Deaths_World.csv")))
# unlink(file.path(file.dir.ui, paste0("Rates of Decline_World.csv")))
# # ### DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
# }

# run aggregates with M49 replacing missing rates
OutputAggregates(runname.U5MR = runname.U5MR,
                  runname.IMR = runname.IMR,
                  runname.NMR = runname.NMR,
                  run.on.server = run.on.server,
                  year4 = year.lastestimatepublished,
                 output.dir = file.dir.ui,
                 filename.NMR = "finalresults.jtc.Rda",
                 year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
                 regiontypes.select = c("UNICEFReport","WHO","WB","UNPD","SDGsimple"),
                 # regiontypes.select = c("M49"),
                 test = TRUE, ## TEST RUN will sample 5 trajectories instead of full 8000
                 replace.rates.reg="M49Region",
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]))

# # Projections
# source(file.path("R/outputaggregates-BWC.R"))
# projection <- c("Constant2013")#, "Constant2016", "HighIncome", "SDG2016", "Highincome2016", "Constant2000")
# projections <- c("HighIncome", "SDG2016", "Highincome2016", "Constant2000")
# for(projection in projections){
# file.dir.median <- file.path(paste0("Projections/",projection," Rev Aggregate results (median) ", date))
# year.lastestimatepublished <- 2030.5#2016.5
# ifelse(projection=="AdjARR"|projection=="Constant2016"|projection=="HighIncome"|projection=="SDG2016", est.years.proj <- seq(1950.5,2050.5,1), est.years.proj <- seq(1950.5,2030.5,1))
#
# OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
#                  results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
#                  results.NMR.file = file.path("output", runname.NMR, "2017-08-10res_nmr.csv"),
#                  #results.U5MR.file = paste0("Projections/Input results/",projection,"/",projection,"_Results_U5MR.csv"),
#                  #results.IMR.file = paste0("Projections/Input results/",projection,"/",projection,"_Results_IMR.csv"),
#                  #results.NMR.file = paste0("Projections/Input results/",projection,"/",projection,"_Results_NMR.csv"),
#                   run.on.server = run.on.server,
#                   year4 = 2016.5,
#                   output.dir = file.dir.median,
#                   year.target = 2016.5, #est.years = est.years.proj,
#                   regiontypes.select = c("Fragile2015"))#c("NewUnicef","SDG"))#, "Countdown", "ECAAfrica", "AU",
# #                                         "Fragile2013", "Fragile2014",
# #                                         "Fragile2015", "Fragile2017",
# #                                         "USAID")) ## add livebirths.file?
# } # loop for projections


# crisis-free (median)
# OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results (crisis-free).csv"),
#                  results.IMR.file = file.path("output", runname.IMR, "Results (crisis-free).csv"),
#                  results.NMR.file = file.path("output", runname.NMR, "2017-08-10res_nmrcf.csv"),
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  year.target = 2016.5, est.years = seq(1950.5,2016.5,1),
#                  regiontypes.select = c("NewUnicef")) ## add livebirths.file?




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
 
#----------------------------------------------------------------------
 # 5. Combine median + UIs
 #----------------------------------------------------------------------
 file.dir.median <- file.path("C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code/Aggregate results (median) 2018-07-31") ##<< File directory to save median estimates to
file.dir.ui <- file.path("C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code/Aggregate results (UIs) 2018-07-26") ##<< File directory to save UIs to
file.dir.output <- file.path("C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code/Aggregate results (final) 2018-08-06") ##<< File directory to save final (combined) median estimates + UIs to
dir.create(file.dir.output, showWarnings = F)

 # Country
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

s  # Region & world
  # file.names <- list.files(file.dir.median)
#  file.names <- list.files(file.dir.ui)
#  file.names <- file.names[!is.element(file.names, c("samples", "samples_combined", "Rates & Deaths_Country Summary.csv"))]
#
#  file.names <- file.names[!is.element(file.names, c("samples", "samples_combined", "Rates & Deaths_Country Summary.csv", "NMR", "IMR", "U5MR"))]
#  for (file.name in file.names) {
#    estimates.file <- file.path(file.dir.ui, file.name)
#    estimates.median.file <- file.path(file.dir.median, file.name)
#    estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
#    estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)
#    estimates[, grepl("median", colnames(estimates))] <- estimates.median[grepl("median", colnames(estimates.median))]
#    write.csv(estimates, file.path(file.dir.output, file.name), row.names = F, na = "")
#  }

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
# source(file.path("R/outputaggregates-BWC.R"))
#  source(file.path("R/outputaggregates-BWC_replaceregion.R"))
#
# # Male (median)
# date <- "2018-09-05 (male)"
# file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
# OutputAggregates(results.U5MR.file = "output/Sex_fordeathCalculation/Results_u5mr_m.csv",
#                  results.IMR.file = "output/Sex_fordeathCalculation/Results_imr_m.csv",
#                  results.NMR.file = NULL,
#                  population.file = "input/data_male_CMEpopulation_20150817.csv",
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  livebirths.file = "input/data_livebirths_male.csv",
#                  year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
#                  regiontypes.select = c("M49"),
#                  #regiontypes.select = c("SDGSimple", "UNICEFReport", "WHO", "UNPD", "WB"),
#                  replace.rates.reg=NULL,
#                  replace.rates.cat=NULL)

# ### DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
# unlink(file.path(file.dir.median, "samples_combined", paste0("coverage0.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("coverageu5.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("death0.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("deathu5.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("deathnn.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("imr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("u5mr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("nmr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop0.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop0.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop1to4.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop1to4.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("popu5.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("res.world.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("global.RoDs.ui.rda")))
# unlink(file.path(file.dir.median, paste0("Rates & Deaths_World.csv")))
# unlink(file.path(file.dir.median, paste0("Rates of Decline_World.csv")))
# ### DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
#
# OutputAggregates(results.U5MR.file = "output/Sex_fordeathCalculation/Results_u5mr_m.csv",
#                  results.IMR.file = "output/Sex_fordeathCalculation/Results_imr_m.csv",
#                  results.NMR.file = NULL,
#                  population.file = "input/data_male_CMEpopulation_20150817.csv",
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  livebirths.file = "input/data_livebirths_male.csv",
#                  year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
#                  regiontypes.select = c("SDG"),
#                  replace.rates.reg="M49Region",
#                  replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]))
#
#
#
# # # Female (median)
# date <- "2018-09-05 (female)"
# file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
# OutputAggregates(results.U5MR.file = "output/Sex_fordeathCalculation/Results_u5mr_f.csv",
#                  results.IMR.file = "output/Sex_fordeathCalculation/Results_imr_f.csv",
#                  results.NMR.file = NULL,
#                  population.file = "input/data_female_CMEpopulation_20150817.csv",
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  livebirths.file = "input/data_livebirths_female.csv",
#                  year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
#                  regiontypes.select = c("M49"),
#                  #regiontypes.select = c("SDGSimple", "UNICEFReport", "WHO", "UNPD", "WB"),
#                  replace.rates.reg=NULL,
#                  replace.rates.cat=NULL)
#
# ## DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
# unlink(file.path(file.dir.median, "samples_combined", paste0("coverage0.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("coverageu5.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("death0.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("deathu5.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("deathnn.all.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("imr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("u5mr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("nmr.wtj.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop0.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop0.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop1to4.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("pop1to4.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("popu5.orig.wt.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("res.world.rda")))
# unlink(file.path(file.dir.median, "samples_combined", paste0("global.RoDs.ui.rda")))
# unlink(file.path(file.dir.median, paste0("Rates & Deaths_World.csv")))
# unlink(file.path(file.dir.median, paste0("Rates of Decline_World.csv")))
# ## DELETE the WORLD files after getting M49 aggregate so it will generate world results with M49 replacements ###
#
# OutputAggregates(results.U5MR.file = "output/Sex_fordeathCalculation/Results_u5mr_f.csv",
#                  results.IMR.file = "output/Sex_fordeathCalculation/Results_imr_f.csv",
#                  results.NMR.file = NULL,
#                  population.file = "input/data_female_CMEpopulation_20150817.csv",
#                  run.on.server = run.on.server,
#                  year4 = year.lastestimatepublished,
#                  output.dir = file.dir.median,
#                  livebirths.file = "input/data_livebirths_female.csv",
#                  year.target = 2017.5, est.years = seq(1950.5,2017.5,1),
#                  regiontypes.select = c("SDG"),
#                  replace.rates.reg="M49Region",
#                  replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]))

# combine sex-specific files
sex <- c("female", "male")
for(i in 1:length(sex)){
  file.dir.median <- file.path(paste0("C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code/Aggregate results (median) 2018-09-05 (",sex[i],")")) ##<< File directory to save median estimates to
  file.dir.ui <- file.path(paste0("C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code/Aggregate results (UIs) 2018-12-16 (",sex[i],")")) ##<< File directory to save UIs to
  file.dir.output <- file.path(paste0("C:/Users/dsharrow/Dropbox/IGME Data/2018 Round Estimation/Code/Aggregate results (final) 2018-12-16 (",sex[i],")")) ##<< File directory to save final (combined) median estimates + UIs to
  #dir.create(file.dir.output, showWarnings = F)
  
  # Country
  file.name.ui <- "Rates & Deaths_Country Summary.csv"
  file.name.med <- "Rates & Deaths(ADJUSTED)_Country Summary.csv"
  estimates.file <- file.path(file.dir.ui, file.name.ui)
  estimates.median.file <- file.path(file.dir.median, file.name.med)
  estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
  estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)
  
  estimates.median <- estimates.median[,match(names(estimates), names(estimates.median))]
  
  estimates.output <- rbind(estimates[estimates$X != "Median",], estimates.median)
  order <- order(estimates.output$CountryName, estimates.output$X)
  estimates.output <- estimates.output[order, ]
  write.csv(estimates.output, file.path(file.dir.output, file.name.med), row.names = F, na = "")
} # i loop for sex


