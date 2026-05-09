# This is the script that walks through the updating process of the app


# Change release date in the `About` panel, please revise the date every year: 
update_string0 <- "Last updated: March, 2026"

# now, first source this script and then walk through following steps:
# 1. If needed (like in the 2022 round, we add the number of countries, or WPP
# got updated), update a0.csv, livebirths files and male/female population from
# the IGME round input folder. Copy-paste from Dropbox\UN IGME data\202x Round
# Estimation\Code\input. Not all these files change from year to year.
# 

# The "country.info.CME.csv" files are updated by code later. No need to update
# "country.info.CME_adhoc.csv" --- you can delete all the "_adhoc.csv"

# Package data locations ------------------------------------------------------
dir_extdata <- file.path("inst", "extdata")
dir_input <- file.path(dir_extdata, "input")
dir_output <- file.path(dir_extdata, "output")
dir_examples <- file.path(dir_extdata, "examples")
dir_www <- file.path("inst", "app", "www")

# 2. Delete all these folders
dir_median_total  <- file.path(dir_extdata, "median_results_total")
dir_median_female <- file.path(dir_extdata, "median_results_female")
dir_median_male   <- file.path(dir_extdata, "median_results_male")
dir_median_total_5_14   <- file.path(dir_extdata, "median_results_total_5_14")
dir_median_female_5_14  <- file.path(dir_extdata, "median_results_female_5_14")
dir_median_male_5_14    <- file.path(dir_extdata, "median_results_male_5_14")
dir_median_total_15_24  <- file.path(dir_extdata, "median_results_total_15_24")
dir_median_female_15_24 <- file.path(dir_extdata, "median_results_female_15_24")
dir_median_male_15_24   <- file.path(dir_extdata, "median_results_male_15_24")


# 3. Modify and run "update/1.Update median agg and results.csv.R" to copy
# required results.csv files, and country summary.csv folder names in "output/".

# no need to change:
runname.U5MR   <- "U5MR"
runname.IMR    <- "IMR"
runname.NMR    <- "NMR"
file_name_NMR  <- "NMR/Results.csv"

# The name of the final results file (usually remain unchanged):
file_name_total <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"
file_name_male <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# for older children 
file_name_total_5_24 <- "Rates & Deaths(ADJUSTED)_Country Summary.csv"
file_name_female_5_24 <- "Rates & Deaths_Country Summary.csv"
file_name_male_5_24 <- "Rates & Deaths_Country Summary.csv"

# Stillbirth median-only input for the app.
dir_stillbirth_aggregate_results <- file.path(Sys.getenv("USERPROFILE"), "Dropbox/UNICEF Stillbirth/Aggregate results 2026-01-28")
file_name_stillbirth_country_results <- "UNIGME_SBR_CountryResults.csv"
file_name_stillbirth_country_medians <- "stillbirth_country_medians.csv"

# 4. Build M49 region and initiate folder structure, all the files in these
# "median_results" median folders will be created by the script: 
# "update/2.Create M49 regions and initiate app.R"

# 5. Extra
# Show data from (usually remain unchanged): 
year_started <- 1990 # no need to change, this is starting year to show
# year_end is set to be the maximum available year in results.csv, which is the latest year. There is no need to set manually
