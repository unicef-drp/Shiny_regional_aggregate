#----------------------------------------------------------------------
# 6outputaggregates.R
# Jin Rou New, 2013-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- TRUE # Indicate if run is on the server
get.notifications <- TRUE # Get notifications via Pushbullet about run progress (only if Pushbullet is set up)?
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
runname.U5MR <- "GR20150511_all"
runname.IMR <- "IMR20150518_all"
year.lastestimatepublished <- 2015.5
#date <- Sys.Date()
date <- "2015-06-04"
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
file.dir.ui <- file.path(paste("Aggregate results (UIs)", date)) ##<< File directory to save UIs to
file.dir.output <- file.path(paste("Aggregate results (final)", date)) ##<< File directory to save final (combined) median estimates + UIs to

# Total (median)
OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median)
# Total (UIs)
OutputAggregates(runname.U5MR = runname.U5MR,
                 runname.IMR = runname.IMR,
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.ui)
# Note: The run fails to run in parallel for region aggregates,
# so restart run with run.on.server set to FALSE in the next step
# Total (UIs) for regions
for (regiontype.select in c("UNICEF", "MDG", "WHO", "WB", "UNPD", "M49"))
  OutputAggregates(runname.U5MR = runname.U5MR,
                   runname.IMR = runname.IMR,
                   regiontypes.select = regiontype.select, # Select regiontype or set to NULL for all regions.
                   run.on.server = FALSE, # Note: Must be set to FALSE for regions.
                   year4 = year.lastestimatepublished,
                   output.dir = file.dir.ui)
if (get.notifications)
    pbPost(type = "note",
           title = paste0("6outputaggregates.R"),
           body = paste0("Aggregates done for ", regiontype.select),
           recipients = c(1, 2))
# Note: The options for regiontypes.select are "UNICEF", "MDG", "WHO", "WB", "UNPD", "OIC",
# "Countdown", "ECAAfrica", "AU", "Fragile2013", "Fragile2014", "Fragile2015", "USAID", "M49"
stop()
#----------------------------------------------------------------------
# ARR (median)
# CalculateARRForOtherPeriods(year1 = 2000.5, year2 = 2010.5, output.dir = "output_numberofdeaths_20180812")
# CalculateARRForOtherPeriods(year1 = 2000.5, year2 = 2010.5, output.dir = "output_numberofdeaths_median_20130812")
#----------------------------------------------------------------------
# 5. Combine median + UIs
#----------------------------------------------------------------------
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
file.dir.ui <- file.path(paste("Aggregate results (UIs)", date)) ##<< File directory to save UIs to
file.dir.output <- file.path(paste("Aggregate results (final)", date)) ##<< File directory to save final (combined) median estimates + UIs to
dir.create(file.dir.output, showWarnings = F)

# Country
file.name <- "Rates & Deaths_Country Summary.csv"
estimates.file <- file.path(file.dir.ui, file.name)
estimates.median.file <- file.path(file.dir.median, file.name)
estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)

estimates.output <- rbind(estimates[estimates$X != "Median",], estimates.median)
order <- order(estimates.output$CountryName, estimates.output$X)
estimates.output <- estimates.output[order, ]
write.csv(estimates.output, file.path(file.dir.output, file.name), row.names = F, na = "")

# Region & world
file.names <- list.files(file.dir.ui)
file.names <- file.names[!is.element(file.names, c("samples_combined", "Rates & Deaths_Country Summary.csv"))]
for (file.name in file.names) {
  estimates.file <- file.path(file.dir.ui, file.name)
  estimates.median.file <- file.path(file.dir.median, file.name)
  estimates <- read.csv(estimates.file, header = T, stringsAsFactors = F)
  estimates.median <- read.csv(estimates.median.file, header = T, stringsAsFactors = F)
  estimates[, grepl("median", colnames(estimates))] <- estimates.median[grepl("median", colnames(estimates.median))]
  write.csv(estimates, file.path(file.dir.output, file.name), row.names = F, na = "")
}
#----------------------------------------------------------------------
# 6. Output results with WPP 2010
#----------------------------------------------------------------------
# Total (with WPP 2010)
OutputAggregates(results.U5MR.file = "input/Results_U5MR_Final_20130812.csv",
                 results.IMR.file = "input/Results_IMR_Final_20130812.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 population.file = "input/population_2012.csv",
                 output.dir = "output_numberofdeaths_IGME2013withWPP2010")
#----------------------------------------------------------------------
# 7. Reformat sex-specific results then output death/aggregate estimates
#----------------------------------------------------------------------
for (indicator in c("U5MR", "IMR")) {
  for (sex in c("Male", "Female")) {
    file.input <- file.path("input", paste0("Results_", indicator, tolower(sex), ".csv"))
    file.output <- file.path("input", paste0("Results_", indicator, "_", sex, "_Final.csv"))
    res <- read.csv(file = file.input, header = T, stringsAsFactors = F)
    res$Indicator <- ifelse(indicator == "U5MR", "Under-five Mortality Rate", "Infant Mortality Rate")
    res$Subgroup <- sex
    res.output <- cbind(res[!grepl("X", colnames(res))], 1000*res[grepl("X", colnames(res))])
    write.csv(res.output, file = file.output, row.names = F, na = "")
  }
}
#----------------------------------------------------------------------
# Note: Requires results for male/female U5MR/IMR & male/female population at age 0/under age 5
# Male (median)
OutputAggregates(results.U5MR.file = "input/Results_U5MR_Male_Final_20130815.csv",
                 results.IMR.file = "input/Results_IMR_Male_Final_20130815.csv",
                 population.file = "input/data_male_CMEpopulation_JRformat.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = "output_numberofdeathsmale_20130821")
# Female (median)
OutputAggregates(results.U5MR.file = "input/Results_U5MR_Female_Final_20130815.csv",
                 results.IMR.file = "input/Results_IMR_Female_Final_20130815.csv",
                 population.file = "input/data_female_CMEpopulation_JRformat.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = "output_numberofdeathsfemale_20130821")
