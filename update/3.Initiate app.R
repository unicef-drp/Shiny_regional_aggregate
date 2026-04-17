# Initializing app after updating every year
# run the whole script
# Because there is no need to keep SDGsimple region output
# This script produces less files, easier to deploy

source("update_me_every_year.R")

# Libraries
check.and.install.pkgs <- function(pkgs){
  search_package <- sapply(pkgs, find.package, quiet = TRUE) # return a string or character(0)
  new.packages <- pkgs[sapply(search_package, function(x)length(x)==0)]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
check.and.install.pkgs(c("shiny", "shinyWidgets", "shinyjs",
                         "DT","data.table", "dplyr", "ggplot2", "plotly", "readxl"))

# source code
invisible(sapply(list.files(file.path("R"), full.names = TRUE, recursive = TRUE), source))

# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  # it seems listing the libraries is necessary if want to publish on shinyapps.io
  library("shiny")    # for shiny apps
  library("shinyWidgets")
  library("shinyjs")  # for  reset
  library("DT")       # for shiny table 
  library("data.table") 
  library("dplyr")
  library("ggplot2")
  library("plotly")
  library("readxl")
})

# Dataset and Parameters -----------------------------------------------------------------

# dc: country.info.CME dataset
dc <- fread(file.path("input/country.info.CME.csv"))
country.info <- dc
dc.5.14 <- fread(file.path("input/country.info.CME.5_14.csv"))
dc.15.24 <- fread(file.path("input/country.info.CME.15_24.csv"))


# median results for selected countries will be included in the downloaded data
# but won't be shown in the app. Rates are not rounded in the downloaded data
c_median_total <- read.country.summary(dir_dt_cs = file.path(dir_median_total, file_name_total), year_wanted = year_started:2030)
c_median_f     <- read.country.summary(dir_dt_cs = file.path(dir_median_female, file_name_female), year_wanted = year_started:2030)
c_median_m     <- read.country.summary(dir_dt_cs = file.path(dir_median_male, file_name_male), year_wanted = year_started:2030)
c_median_total_5_14  <- read.country.summary(dir_dt_cs = file.path(dir_median_total_5_14, file_name_total_5_24), year_wanted = year_started:2030)
c_median_f_5_14      <- read.country.summary(dir_dt_cs = file.path(dir_median_female_5_14, file_name_female_5_24), year_wanted = year_started:2030)
c_median_m_5_14      <- read.country.summary(dir_dt_cs = file.path(dir_median_male_5_14, file_name_male_5_24), year_wanted = year_started:2030)
c_median_total_15_24 <- read.country.summary(dir_dt_cs = file.path(dir_median_total_15_24, file_name_total_5_24), year_wanted = year_started:2030)
c_median_f_15_24     <- read.country.summary(dir_dt_cs = file.path(dir_median_female_15_24, file_name_female_5_24), year_wanted = year_started:2030)
c_median_m_15_24     <- read.country.summary(dir_dt_cs = file.path(dir_median_male_15_24, file_name_male_5_24), year_wanted = year_started:2030)

c_median_total_5_14 <- recode_ind_5_14(c_median_total_5_14)
c_median_f_5_14 <- recode_ind_5_14(c_median_f_5_14)
c_median_m_5_14 <- recode_ind_5_14(c_median_m_5_14)
c_median_total_15_24 <- recode_ind_15_24(c_median_total_15_24)
c_median_f_15_24 <- recode_ind_15_24(c_median_f_15_24)
c_median_m_15_24 <- recode_ind_15_24(c_median_m_15_24)

c_median_5_14    <- rbindlist(list(c_median_total_5_14, c_median_f_5_14, c_median_m_5_14))
c_median_15_24   <- rbindlist(list(c_median_total_15_24, c_median_f_15_24, c_median_m_15_24)) 
c_median_total_older <- merge(c_median_5_14, c_median_15_24)
c_median_total_older <- calculate.10q10(c_median_total_older)

col_order_older_children <- copy(colnames(c_median_total_older)) # colnames containing "X"
col_order_older_children_all_rate <- colnames(c_median_total_older)[grepl("Mortality rate", colnames(c_median_total_older))]

year_ended <- floor(max(c_median_total$Year))
year.lastestimatepublished <- year_ended + 0.5  # e.g. 2019.5 for IGME 2020


# for under-five, run M49 in advance, so this initiating step is required to run
# before running the App
OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                 results.NMR.file = file.path("output", file_name_NMR),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_total,
                 year.target = year.lastestimatepublished,
                 est.years = seq(1950.5, year.lastestimatepublished,1),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 regiontypes.select = c("M49"),
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)


OutputAggregates(results.U5MR.file = file.path("output/Sex_forDeathCalculation/Results_u5mr_m.csv"),
                 results.IMR.file = file.path("output/Sex_forDeathCalculation/Results_imr_m.csv"),
                 results.NMR.file = NULL,
                 population.file = file.path("input/data_male_CMEpopulation.WPP2024.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_male,
                 livebirths.file = file.path("input/data_livebirths_male.csv"),
                 year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("M49"),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)


OutputAggregates(results.U5MR.file = file.path("output/Sex_forDeathCalculation/Results_u5mr_f.csv"),
                 results.IMR.file = file.path("output/Sex_forDeathCalculation/Results_imr_f.csv"),
                 results.NMR.file = NULL,
                 population.file = file.path("input/data_female_CMEpopulation.WPP2024.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_female,
                 livebirths.file = file.path("input/data_livebirths_female.csv"),
                 year.target = year.lastestimatepublished, 
                 est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("M49"),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)


# Check -------------------------------------------------------------------

# check if results match for under-five
dirold <- file.path(dir_aggu5_median, "Rates & Deaths_M49Region.csv") # for comparison
dirnew <- file.path(file.path("median_results_total/Rates & Deaths_M49Region.csv"))
dt1 <- read.region.summary(dirold)
dt2 <- read.region.summary(dirnew)
setnames(dt2, "value", "value.new")
dtc <- merge(dt1, dt2)
dtc[, diff:= round(value.new - value, 4)]
dtc[diff!=0,] # should be none 



# If results match, 
# run aggregates for the selected countries ----------------------------------
init_select <- "Afghanistan"
dc[,AdhocCountries:=""]
dc[OfficialName %in% init_select, AdhocCountries:="Adhoc"]
dc.5.14[,AdhocCountries:=""]
dc.5.14[OfficialName %in% init_select, AdhocCountries:="Adhoc"]
dc.15.24[,AdhocCountries:=""]
dc.15.24[OfficialName %in% init_select, AdhocCountries:="Adhoc"]

# Apply multi region input to all datasets
# dc_input <- fread("Upload_ISO_example_WB.csv")
# dc <- apply.multi.region(dc, dc_input)
# dc.5.14 <- apply.multi.region(dc.5.14, dc_input)
# dc.15.24 <- apply.multi.region(dc.15.24, dc_input)

write.csv(dc, file = file.path("input", "country.info.CME_adhoc.csv"))
write.csv(dc.5.14, file = file.path("input", "country.info.CME.5_14_adhoc.csv"))
write.csv(dc.15.24, file = file.path("input", "country.info.CME.15_24_adhoc.csv"))

invisible(sapply(list.files(file.path("R"), full.names = TRUE, recursive = TRUE), source))

# basically what the app runs are these: first run with reuse.replacement.country = FALSE
system.time({
run.outputaggregates(year.lastestimatepublished, reuse.replacement.country = FALSE)
run.outputaggregates.gender(year.lastestimatepublished, reuse.replacement.country = FALSE)
adjust.u5.sex.specific.death()
run.outputaggregates.5.24(year.lastestimatepublished)
run.outputaggregates.5.24.gender(year.lastestimatepublished)
adjust.total.death.5.24()
})

system.time({
  run.outputaggregates(year.lastestimatepublished, reuse.replacement.country = TRUE)
  run.outputaggregates.gender(year.lastestimatepublished, reuse.replacement.country = TRUE)
  adjust.u5.sex.specific.death()
  run.outputaggregates.5.24(year.lastestimatepublished)
  run.outputaggregates.5.24.gender(year.lastestimatepublished)
  adjust.total.death.5.24()
})

# Clean up under-five cache files after replacement-country caches are built.
# Keep enough to use reuse.replacement.country = TRUE and avoid recomputing
# country trajectories in future median total and sex-specific runs.
invisible(lapply(
  c(dir_median_total, dir_median_male, dir_median_female),
  cleanup.outputaggregate.cache,
  replace.rates.reg = "M49Region",
  cache.type = "under_five"
))

invisible(lapply(
  c(
    dir_median_total_5_14,
    dir_median_female_5_14,
    dir_median_male_5_14,
    dir_median_total_15_24,
    dir_median_female_15_24,
    dir_median_male_15_24
  ),
  cleanup.outputaggregate.cache,
  cache.type = "older_children"
))

