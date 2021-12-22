# Initializing app after updating every year


# The only script to update:
source("update_me_every_year.R")

# Libraries
check.and.install.pkgs <- function(pkgs){
  search_package <- sapply(pkgs, find.package, quiet = TRUE) # return a string or character(0)
  new.packages <- pkgs[sapply(search_package, function(x)length(x)==0)]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
check.and.install.pkgs(c("shiny", "shinyWidgets", "shinyjs", "leaflet",
                         "maps", "maptools", "rgeos",
                         "DT","data.table", "dplyr", "here", 
                         "ggplot2", "plotly", "readxl"))

# source code
invisible(sapply(list.files(here::here("R"), full.names = TRUE), source))

# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  # it seems listing the libraries is necessary if want to publish on shinyapps.io
  library("here")
  library("shiny")    # for shiny apps
  library("shinyWidgets")
  library("shinyjs")  # for  reset
  library("leaflet")  # for openstreetmap
  library("maps")     # provide shap files for selected countries
  library("maptools") # modify shap files
  library("rgeos")
  library("DT")       # for shiny table 
  library("data.table") 
  library("dplyr")
  library("ggplot2")
  library("plotly")
  library("readxl")
})

# Sanitizing error messages
options(shiny.sanitize.errors = TRUE)

# Language -------------------------------------------------------------------
note_header <- p("This ShinyApp produces regional aggregates of child mortality
estimates based on individually selected countries. The ", 
                 a("UN IGME", href = "https://childmortality.org", target = "_blank"),
                 "\'s latest estimates of neonatal, infant and under-five mortality are used. 
Country data will also be included in the downloaded dataset from the \"Tables and Data Download\" panel
after running the aggregates.")

note_input <- "Please add countries by clicking the list, or uploading a file of selected ISOs."
default_select <- "Afghanistan"

panel_title1   <- "Results of selected regional aggregates for"
panel_title1.2 <- "Tables of selected regional aggregates"
panel_note1.2 <- "Regional, world, and country data are available for download."
panel_title2 <- "Sex-specific results for selected regional aggregates"

note_map <- "Note: This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

adhoc_name <- "Selected Countries"

# Dataset and Parameters -----------------------------------------------------------------


# dc: country.info.CME dataset
dc <- country.info <- fread(here::here("input", "country.info.CME.csv"))

# Define region
dc[, UNICEF_region:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
dc[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
dc$SDG_region <- revise.name(dc$SDG_region, new_list = SDG_list)
regions_1 <- sort(dc[, unique(UNICEF_region)])
regions_2 <- sort(dc[, unique(SDG_region)])
regions_3 <- sort(dc[, unique(M49Region1)])
# a list by countries grouped by regions for shinyWidgets::pickerInput(country_input)
input_country_list <- list()
for (i in 1:length(regions_3)){
  input_country_list[[regions_3[i]]] <- sort(unique(dc[M49Region1==regions_3[i], OfficialName]))
}

# Country names and ISOs
countries <- sort(dc[,unique(OfficialName)])
ISOs <- sort(dc[,unique(ISO3Code)])

# Get world map with modified country names 
world_map <- get.world.map()

# median results by country 
# (results for each country will be included in the download as well)
c_median_all <- read.country.summary(dir_dt_cs = file.path(dir_median_total, file_name_total), year_wanted = year_started:2030)
c_median_f <- read.country.summary(dir_dt_cs = file.path(dir_median_female, file_name_female), year_wanted = year_started:2030)
c_median_m <- read.country.summary(dir_dt_cs = file.path(dir_median_male, file_name_male), year_wanted = year_started:2030)

year_ended <- floor(max(c_median_all$Year))
year.lastestimatepublished <- year_ended + 0.5  # e.g. 2019.5 for IGME 2020


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
                 population.file = here::here("input/data_male_CMEpopulation_20150817.csv"),
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
                 population.file = here::here("input/data_female_CMEpopulation_20150817.csv"),
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
