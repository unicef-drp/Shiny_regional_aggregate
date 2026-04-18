# Initializing app after updating every year
# run the whole script
# I also use this script to help debugging, basically it runs everything without running the app.

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
dc <- fread(file.path(dir_input, "country.info.CME.csv"))
country.info <- dc
dc.5.14 <- fread(file.path(dir_input, "country.info.CME.5_14.csv"))
dc.15.24 <- fread(file.path(dir_input, "country.info.CME.15_24.csv"))


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
OutputAggregates(results.U5MR.file = file.path(dir_output, runname.U5MR, "Results.csv"),
                 results.IMR.file = file.path(dir_output, runname.IMR, "Results.csv"),
                 results.NMR.file = file.path(dir_output, file_name_NMR),
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


OutputAggregates(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_m.csv"),
                 results.IMR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_m.csv"),
                 results.NMR.file = NULL,
                 population.file = file.path(dir_input, "data_male_CMEpopulation.WPP2024.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_male,
                 livebirths.file = file.path(dir_input, "data_livebirths_male.csv"),
                 year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("M49"),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)


OutputAggregates(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_f.csv"),
                 results.IMR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_f.csv"),
                 results.NMR.file = NULL,
                 population.file = file.path(dir_input, "data_female_CMEpopulation.WPP2024.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_female,
                 livebirths.file = file.path(dir_input, "data_livebirths_female.csv"),
                 year.target = year.lastestimatepublished, 
                 est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("M49"),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)




# test script for older children: -------------------------------------------
# test if output matches
# Total 5-14
OutputAggregates.ori(results.U5MR.file = file.path(dir_output, "10q5", "Results.csv"),
                     results.IMR.file  = file.path(dir_output, "5q5",  "Results.csv"),
                     country.info.file = file.path(dir_input, "country.info.CME.5_14.csv"),
                     population.file   = file.path(dir_input, "country.info.CME.5_14.csv"),
                     output.dir = dir_median_total_5_14,
                     regiontypes.select = c("SDGSimple"))

# Male 5-14
OutputAggregates.ori(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_10q5_m.csv"),
                     results.IMR.file  = file.path(dir_output, "Sex_forDeathCalculation", "Results_5q5_m.csv"),
                     country.info.file = file.path(dir_input, "country.info.CME.5_14.csv"),
                     population.file   = file.path(dir_input, "data_male_CME_WPP2024_10q5.csv"),
                     output.dir = dir_median_male_5_14,
                     regiontypes.select = c("SDGSimple"))
# Female 5-14
OutputAggregates.ori(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_10q5_f.csv"),
                     results.IMR.file  = file.path(dir_output, "Sex_forDeathCalculation", "Results_5q5_f.csv"),
                     country.info.file = file.path(dir_input, "country.info.CME.5_14.csv"),
                     population.file   = file.path(dir_input, "data_female_CME_WPP2024_10q5.csv"),
                     output.dir = dir_median_female_5_14,
                     regiontypes.select = c("SDGSimple"))


# Total 15-24
OutputAggregates.ori(results.U5MR.file = file.path(dir_output, "10q15", "Results.csv"),
                     results.IMR.file  = file.path(dir_output, "5q15",  "Results.csv"),
                     country.info.file = file.path(dir_input, "country.info.CME.15_24.csv"),
                     population.file   = file.path(dir_input, "country.info.CME.15_24.csv"),
                     output.dir = dir_median_total_15_24,
                     regiontypes.select = c("SDGSimple"))


# Female 15-24
OutputAggregates.ori(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_10q15_f.csv"),
                     results.IMR.file  = file.path(dir_output, "Sex_forDeathCalculation", "Results_5q15_f.csv"),
                     country.info.file = file.path(dir_input, "country.info.CME.15_24.csv"),
                     population.file   = file.path(dir_input, "data_female_CME_WPP2024_10q15.csv"),
                     output.dir = dir_median_female_15_24,
                     regiontypes.select = c("SDGSimple"))

# Male 15-24
OutputAggregates.ori(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_10q15_m.csv"),
                     results.IMR.file  = file.path(dir_output, "Sex_forDeathCalculation", "Results_5q15_m.csv"),
                     country.info.file = file.path(dir_input, "country.info.CME.15_24.csv"),
                     population.file   = file.path(dir_input, "data_male_CME_WPP2024_10q15.csv"),
                     output.dir = dir_median_male_15_24,
                     regiontypes.select = c("SDGSimple"))




# adjust death
adjust.total.death.5.24(region_name = "SDGSimpleRegion")


# Check -------------------------------------------------------------------

# check if results match for under-five
dirold <- file.path(dir_aggu5_median, "Rates & Deaths_M49Region.csv") # for comparison
dirnew <- file.path(dir_median_total, "Rates & Deaths_M49Region.csv")
dt1 <- read.region.summary(dirold)
dt2 <- read.region.summary(dirnew)
setnames(dt2, "value", "value.new")
dtc <- merge(dt1, dt2)
dtc[, diff:= round(value.new - value, 4)]
dtc[diff!=0,] # should be none 



# check if results match for older children 5-14
dirold <- file.path(dir_agg10q5_median, "Rates & Deaths(ADJUSTED)_SDGSimpleRegion.csv") # for comparison
dirnew <- file.path(dir_median_total_5_14, "Rates & Deaths(ADJUSTED)_SDGSimpleRegion.csv")
dt1 <- read.region.summary(dirold)
dt2 <- read.region.summary(dirnew)
recode5_14 <- c("U5MR" = "X10q5", "IMR" = "X5q5", "CMR" = "X5q10",
                "10q5" = "X10q5", "5q5" = "X5q5", "5q10" = "X5q10",
                "Under.five.deaths" = "deaths.age.5to14",
                "Infant.deaths" = "deaths.age.5to9",
                "Child.deaths" = "deaths.age.10to14"
)
dt1[, Shortind := dplyr::recode(Shortind, !!!recode5_14)]
dt2[, Shortind := dplyr::recode(Shortind, !!!recode5_14)]
setnames(dt2, "value", "value2")
dtc <- merge(dt1, dt2)
dtc[, diff:= round(value- value2, 4)]
dtc[diff!=0,] # should be none 


# check if results match for total 15-24
dirold <- file.path(dir_agg10q15_median, "Rates & Deaths(ADJUSTED)_SDGSimpleRegion.csv") # for comparison
dirnew <- file.path(dir_median_total_15_24, "Rates & Deaths(ADJUSTED)_SDGSimpleRegion.csv")
dt1 <- read.region.summary(dirold)
dt2 <- read.region.summary(dirnew)
recode15_24 <- c("U5MR" = "X10q15", "IMR" = "X5q15", "CMR" = "X5q20",
                 "10q15" = "X10q15", "5q15" = "X5q15", "5q20" = "X5q20",
                 "Under.five.deaths" = "deaths.age.15to24",
                 "Infant.deaths" = "deaths.age.15to19",
                 "Child.deaths" = "deaths.age.20to24"
)
dt1[, Shortind := dplyr::recode(Shortind, !!!recode15_24)]
dt2[, Shortind := dplyr::recode(Shortind, !!!recode15_24)]
setnames(dt2, "value", "value2")
dtc <- merge(dt1, dt2)
dtc[, diff:= round(value- value2, 4)]
dtc[diff!=0]


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
# dc_input <- fread(file.path(dir_examples, "Upload_ISO_example_WB.csv"))
# dc <- apply.multi.region(dc, dc_input)
# dc.5.14 <- apply.multi.region(dc.5.14, dc_input)
# dc.15.24 <- apply.multi.region(dc.15.24, dc_input)

write.csv(dc, file = file.path(dir_input, "country.info.CME_adhoc.csv"))
write.csv(dc.5.14, file = file.path(dir_input, "country.info.CME.5_14_adhoc.csv"))
write.csv(dc.15.24, file = file.path(dir_input, "country.info.CME.15_24_adhoc.csv"))


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



dt5_14  <- (fread(file.path(dir_median_total_5_14,  "Rates & Deaths(ADJUSTED)_AdhocCountries.csv")))
dt15_24 <- (fread(file.path(dir_median_total_15_24, "Rates & Deaths(ADJUSTED)_AdhocCountries.csv")))
dt5_14  <- recode_ind_5_14(dt5_14)
dt15_24 <- recode_ind_15_24(dt15_24)
setkey(dt5_14, Region, Year)
setkey(dt15_24, Region, Year)
dt15_24 <- dt15_24[dt5_14]
dt15_24 <- calculate.10q10(dt15_24)[, Sex := "Both"]
both_5_24 <- dt15_24[Year >= 1990,]

# Process female 5-24
dt5_14_f  <- (fread(file.path(dir_median_female_5_14,  "Rates & Deaths_AdhocCountries.csv")))
dt15_24_f <- (fread(file.path(dir_median_female_15_24, "Rates & Deaths_AdhocCountries.csv")))
dt5_14_f  <- recode_ind_5_14(dt5_14_f)
dt15_24_f <- recode_ind_15_24(dt15_24_f)
setkey(dt5_14_f, Region, Year)
setkey(dt15_24_f, Region, Year)
dt15_24_f <- dt15_24_f[dt5_14_f]
dt15_24_f <- calculate.10q10(dt15_24_f)[, Sex := "Female"]
f_5_24 <- dt15_24_f[Year >= 1990,]


# Process male 5-24
dt5_14_m  <- (fread(file.path(dir_median_male_5_14,  "Rates & Deaths_AdhocCountries.csv")))
dt15_24_m <- (fread(file.path(dir_median_male_15_24, "Rates & Deaths_AdhocCountries.csv")))
dt5_14_m  <- recode_ind_5_14(dt5_14_m)
dt15_24_m <- recode_ind_15_24(dt15_24_m)
setkey(dt5_14_m, Region, Year)
setkey(dt15_24_m, Region, Year)
dt15_24_m <- dt15_24_m[dt5_14_m]
dt15_24_m <- calculate.10q10(dt15_24_m)[, Sex := "Male"]
m_5_24 <- dt15_24_m[Year >= 1990,]



# If everything runs, then the app can be uploaded to the server

# Final note 2023: If you run through this initiation process, and find out that
# something is wrong (e.g. some input files are not correct), delete all median
# folders after correcting the problem, and go through the initiation process
# from the beginning, as some intermediate files won't be overwritten if already
# created
