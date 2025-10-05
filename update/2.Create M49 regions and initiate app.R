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
                         "DT","data.table", "dplyr", "here", 
                         "ggplot2", "plotly", "readxl"))

# source code
invisible(sapply(list.files(here::here("R"), full.names = TRUE, recursive = TRUE), source))

# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  # it seems listing the libraries is necessary if want to publish on shinyapps.io
  library("here")
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
dc <- fread(here::here("input/country.info.CME.csv"))
country.info <- dc
dc.5.14 <- fread(here::here("input/country.info.CME.5_14.csv"))
dc.15.24 <- fread(here::here("input/country.info.CME.15_24.csv"))

# median results for selected countries will be included in the downloaded data
# but won't be shown in the app. Rates are not rounded in the downloaded data
c_median_total <- read.country.summary(dir_dt_cs = file.path(dir_median_total, file_name_total), year_wanted = year_started:2030)
c_median_f     <- read.country.summary(dir_dt_cs = file.path(dir_median_female, file_name_female), year_wanted = year_started:2030)
c_median_m     <- read.country.summary(dir_dt_cs = file.path(dir_median_male, file_name_male), year_wanted = year_started:2030)
c_median_total_5_14  <- read.country.summary(dir_dt_cs = file.path(dir_median_total_5_14, file_name_total), year_wanted = year_started:2030)
c_median_total_15_24 <- read.country.summary(dir_dt_cs = file.path(dir_median_total_15_24, file_name_total), year_wanted = year_started:2030)
c_median_total_older <- merge(recode_ind_5_14(c_median_total_5_14), 
                              recode_ind_15_24(c_median_total_15_24))
c_median_total_older <- calculate.10q10(c_median_total_older)
col_order_older_children <- copy(colnames(c_median_total_older)) # colnames containing "X"
setnames(c_median_total_older, dplyr::recode(col_order_older_children, !!!new_varname_list))
col_order_older_children_all_rate <- colnames(c_median_total_older)[grepl("Mortality rate", colnames(c_median_total_older))]
year_ended <- floor(max(c_median_total$Year))
year.lastestimatepublished <- year_ended + 0.5  # e.g. 2019.5 for IGME 2020


# for under-five, run M49 in advance
OutputAggregates(results.U5MR.file = here::here("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = here::here("output", runname.IMR, "Results.csv"),
                 results.NMR.file = here::here("output", file_name_NMR),
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


OutputAggregates(results.U5MR.file = here::here("output/Sex_forDeathCalculation/Results_u5mr_m.csv"),
                 results.IMR.file = here::here("output/Sex_forDeathCalculation/Results_imr_m.csv"),
                 results.NMR.file = NULL,
                 population.file = here::here("input/data_male_CMEpopulation.WPP2024.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_male,
                 livebirths.file = here::here("input/data_livebirths_male.csv"),
                 year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("M49"),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)


OutputAggregates(results.U5MR.file = here::here("output/Sex_forDeathCalculation/Results_u5mr_f.csv"),
                 results.IMR.file = here::here("output/Sex_forDeathCalculation/Results_imr_f.csv"),
                 results.NMR.file = NULL,
                 population.file = here::here("input/data_female_CMEpopulation.WPP2024.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_female,
                 livebirths.file = here::here("input/data_livebirths_female.csv"),
                 year.target = year.lastestimatepublished, 
                 est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("M49"),
                 test=FALSE,
                 get.world.results = FALSE,
                 round.output = FALSE,
                 replace.rates.reg=NULL,
                 replace.rates.cat=NULL)




# test script for older children: -------------------------------------------
OutputAggregates.ori(results.U5MR.file = file.path("output", "10q5", "Results.csv"),
                     results.IMR.file  = file.path("output", "5q5",  "Results.csv"),
                     country.info.file = file.path("input", "country.info.CME.5_14.csv"),
                     population.file   = file.path("input", "country.info.CME.5_14.csv"),
                     output.dir = dir_median_total_5_14,
                     regiontypes.select = c("SDGSimple"))

OutputAggregates.ori(results.U5MR.file = file.path("output", "10q15", "Results.csv"),
                     results.IMR.file  = file.path("output", "5q15",  "Results.csv"),
                     country.info.file = file.path("input", "country.info.CME.15_24.csv"),
                     population.file   = file.path("input", "country.info.CME.15_24.csv"),
                     output.dir = dir_median_total_15_24,
                     regiontypes.select = c("SDGSimple"))



# Check -------------------------------------------------------------------

# check if results match
dirold <- file.path(dir_agg10q5, "Rates & Deaths(ADJUSTED)_SDGSimpleRegion.csv") # for comparison
dirnew <- file.path(here::here("median_results_total_5_14/Rates & Deaths_SDGSimpleRegion.csv"))
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
dtc[diff!=0, table(Shortind)] # should be none 

dir_IGME_15_24 <- file.path(USERPROFILE, "Dropbox/IGME 15-24/2023 Round Estimation")
dirold <- file.path(dir_IGME_15_24, "Aggregate results (median) 2023-12-28/Rates & Deaths_SDGSimpleRegion.csv")
dirnew <- file.path(here::here("median_results_total_15_24/Rates & Deaths_SDGSimpleRegion.csv"))
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
write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))

dc.5.14[,AdhocCountries:=""]
dc.5.14[OfficialName %in% init_select, AdhocCountries:="Adhoc"]
write.csv(dc.5.14, file = here::here("input", "country.info.CME.5_14_adhoc.csv"))
dc.15.24[,AdhocCountries:=""]
dc.15.24[OfficialName %in% init_select, AdhocCountries:="Adhoc"]
write.csv(dc.15.24, file = here::here("input", "country.info.CME.15_24_adhoc.csv"))

# basically what the app runs are these: 
run.outputaggregates(year.lastestimatepublished)
run.outputaggregates.gender(year.lastestimatepublished)
adjust.death()
run.outputaggregates.5.24(year.lastestimatepublished)

c_median_total <- read.country.summary(dir_dt_cs = file.path(dir_median_total, file_name_total), year_wanted = year_started:2030)
c_median_f     <- read.country.summary(dir_dt_cs = file.path(dir_median_female, file_name_female), year_wanted = year_started:2030)
c_median_m     <- read.country.summary(dir_dt_cs = file.path(dir_median_male, file_name_male), year_wanted = year_started:2030)
c_median_total_5_14  <- read.country.summary(dir_dt_cs = file.path(dir_median_total_5_14, file_name_total), year_wanted = year_started:2030)
c_median_total_15_24 <- read.country.summary(dir_dt_cs = file.path(dir_median_total_15_24, file_name_total), year_wanted = year_started:2030)
c_median_total_older <- merge(recode_ind_5_14(c_median_total_5_14), 
                              recode_ind_15_24(c_median_total_15_24))
c_median_total_older <- calculate.10q10(c_median_total_older)
col_order_older_children <- copy(colnames(c_median_total_older)) # colnames containing "X"

dt5_14  <- (fread(file.path(dir_median_total_5_14,  "Rates & Deaths_AdhocCountries.csv")))
dt15_24 <- (fread(file.path(dir_median_total_15_24, "Rates & Deaths_AdhocCountries.csv")))
setnames(dt5_14, gsub(" median", "", colnames(dt5_14)))
setnames(dt15_24, gsub(" median", "", colnames(dt15_24)))
dt5_14  <- recode_ind_5_14(dt5_14)
dt15_24 <- recode_ind_15_24(dt15_24)
setkey(dt5_14, Region, Year)
setkey(dt15_24, Region, Year)
dt15_24 <- dt15_24[dt5_14]
dt15_24 <- calculate.10q10(dt15_24)[, Sex := "Both"]
both_5_24 <- dt15_24[Year >= 1990,]

output_list <- list(
  both =  (fread(file.path(dir_median_total,  "Rates & Deaths_AdhocCountries.csv"))),
  f    =  (fread(file.path(dir_median_female, "Rates & Deaths(ADJUSTED)_female_AdhocCountries.csv"))),
  m    =  (fread(file.path(dir_median_male,   "Rates & Deaths(ADJUSTED)_male_AdhocCountries.csv"))),
  both_5_24  =  both_5_24,
  c_median_total = c_median_total[Region%in%init_select,],
  c_median_f     = c_median_f[Region%in%init_select,],
  c_median_m     = c_median_m[Region%in%init_select,],
  c_median_total_older = c_median_total_older[Region%in%init_select,]
)


# If everything runs, then the app can be uploaded to the server

# Final note 2023: If you run through this initiation process, and find out that
# something is wrong (e.g. some input files are not correct), delete all median
# folders after correcting the problem, and go through the initiation process
# from the beginning, as some intermediate files won't be overwritten if already
# created
